
#include "SymbolGroupComponent.h"

#include "PageComponent.h"
#include "ScoreComponent.h"


bool SymbolGroupComponent::hitTest (int x, int y)
{
    if( in_edit_mode || is_selected )
    {
        return true; // true in edit mode for drawing?
    }
    
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        if( getSubcomponent(i)->hitTest(x, y) )
        {
            return true;
        }
    }
    
    return false;
}


void SymbolGroupComponent::paint ( Graphics& g )
{
    BaseComponent::paint( g );
    g.setColour( Colour::fromRGB(240,240,240) ) ;
    const Rectangle<int> b = ((BaseComponent*)this)->getLocalBounds();
    g.drawRect(b);
}


void SymbolGroupComponent::selectComponent()
{
    if ( ! in_edit_mode )
    {
        BaseComponent::selectComponent();
        for (int i = 0; i < getNumSubcomponents(); i++ )
        {
            getSubcomponent(i)->selectComponent();
        }
    }
}

void SymbolGroupComponent::deselectComponent()
{
    BaseComponent::deselectComponent();
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        getSubcomponent(i)->deselectComponent();
    }
}



void SymbolGroupComponent::setEditMode( bool val )
{
    BaseComponent::setEditMode(val);
    
    if (val == false ) // exit
    {
        ScoreComponent* sc = ((ScoreComponent*)getParentComponent());
        sc->addToSelection(this);
        
        if ( getNumSubcomponents() == 1 )
        {
            sc->ungroupSelectedSymbols();
        }
        else if ( getNumSubcomponents() == 0 )
        {
            sc->deleteSelectedComponents();
        }
    }
}

void SymbolGroupComponent::updateRelativeAttributes()
{
    updateRelativePos();
    updateRelativeSize();
    
    // called by notify modify when exiting edit mode
    // overridden here to iterate through subcomponent since only groups have subcomponents
    
    if( in_edit_mode )
    {
        for ( int i = 0; i < getNumSubcomponents(); i++ )
        {
            ((BaseComponent*)getSubcomponent(i))->updateRelativeAttributes();
        }
    }
}


bool SymbolGroupComponent::intersectRect( Rectangle<int> rect)
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        if ( getSubcomponent(i)->intersectRect( rect.translated( -getX(), -getY()) ) ) return true ;
    }
    return false;
}

void SymbolGroupComponent::h_flip()
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = ((BaseComponent *)getSubcomponent(i));
        b->h_flip();
        
        auto rel_b = b->getRelativeBounds();
        auto new_x = 1.0 - (rel_b.getX() + rel_b.getWidth());
        rel_b.setX( new_x );
        
        b->setRelativeBounds( rel_b );
    }
    updateSubcomponents();
}

void SymbolGroupComponent::v_flip()
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = ((BaseComponent *)getSubcomponent(i));
        b->v_flip();
        
        auto rel_b = b->getRelativeBounds();
        auto new_y = 1.0 - (rel_b.getY() + rel_b.getHeight());
        rel_b.setY( new_y );
        
        b->setRelativeBounds( rel_b );
    }
    updateSubcomponents();
}

/*============================*
 * SYMBOL MANAGEMENT
 *============================*/

int SymbolGroupComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    
//    cout << "SymbolGroupComponent::addSymbolMessages " << s << " " << base_address << " " << getNumSubcomponents() << endl;
    
    int messages_added = BaseComponent::addSymbolMessages( s, base_address );
    
    String addr = base_address + "/numsymbols";
    
    /* 
     * the group symbol is created fist (with the default state), then added to, then updated,
     * so we need (getNumSubcomponents() > 0) to wait to set /numsymbols until after the symbol has been updated,
     * otherwise, /numsymbols gets stuck at 0
     */
    
    if( s->getOSCMessagePos(addr) == -1 && (getNumSubcomponents() > 0) )
    {
        s->addOSCMessage( addr, (int)getNumSubcomponents() );
        messages_added++;
    }
    
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        addr = base_address + "/subsymbol/" + String(i+1);
        if( s->getOSCMessagePos(addr) == -1 )
        {
            messages_added += ((BaseComponent*)getSubcomponent(i))->addSymbolMessages( s, addr );
        }
    }
    
    return messages_added;
}


void SymbolGroupComponent::importFromSymbol( const Symbol &s )
{

    clearAllSubcomponents();
    
    BaseComponent::importFromSymbol(s);
    
    int pos = s.getOSCMessagePos("/numsymbols");
    
    if ( pos >= 0 )
    {
        int n_symbols = s.getOSCMessageValue(pos).getInt32();
 
        std::cout << "Importing Group of " << n_symbols << " symbols..." << std::endl;
        for (int i = 0; i < n_symbols; i++ )
        {
            String filter = "/subsymbol/" + String(i+1) ;   // we start at 1 .. (?)
            
            cout << "IMPORT FROM: " << filter << endl;
            Symbol sub_s = s.makeSubSymbol( filter );
            BaseComponent* c = getSymbolistHandler()->makeComponentFromSymbol( &sub_s , false );
            
            if ( c != NULL)
                addSubcomponent( c );
            else
                cout << "Error importing subsymbol #" << i << endl;
        }
    }
}

