
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


void SymbolGroupComponent::setSymbolComponentColor( Colour c )
{
    sym_color = c;
    for( int i = 0; i < getNumSubcomponents(); i++ )
    {
        SymbolistComponent *sub = getSubcomponent( i );
        sub->setSymbolComponentColor( c );
    }
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
        ScoreComponent* sc = dynamic_cast<ScoreComponent*>(getParentComponent());
        
        // Checks downcast result.
        if (sc != NULL)
        {
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
}

bool SymbolGroupComponent::intersectRect( Rectangle<int> rect)
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        if ( getSubcomponent(i)->intersectRect( rect.translated( -getX(), -getY()) ) ) return true ;
    }
    return false;
}

void SymbolGroupComponent::h_flip(float ax, float ay)
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = dynamic_cast<BaseComponent*>(getSubcomponent(i));
        
        // Checks the downcast result.
        if (b != NULL)
            b->h_flip(ax - getX(), ay - getY());
    }
}

void SymbolGroupComponent::v_flip(float ax, float ay)
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = dynamic_cast<BaseComponent*>(getSubcomponent(i));
        
        // Checks the downcast result.
        if (b != NULL)
            b->v_flip(ax - getX(), ay - getY());
    }
}

// note: rotateScoreComponent uses the coordinate system of the parent)
void SymbolGroupComponent::rotateScoreComponent(float theta, float ax, float ay)
{
    // cout << "group rotate ref point " << ax - getX() << " " << ay - getY() << endl;
    
    int minx = getParentWidth(), maxx = 0, miny = getParentHeight(), maxy = 0;

    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = dynamic_cast<BaseComponent*>(getSubcomponent(i));
        
        // Checks the downcast result.
        if (b != NULL)
        {
            b->rotateScoreComponent(theta, ax - getX(), ay - getY() );
            
            minx =  min( minx, getX() + b->getX() );
            miny =  min( miny, getY() + b->getY() );
            maxx =  max( maxx, getX() + b->getRight() );
            maxy =  max( maxy, getY() + b->getBottom() );
        }
        
    }
    
    int offsetX = minx - getX();
    int offsetY = miny - getY();

    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto sub = getSubcomponent(i);
        sub->setTopLeftPosition(sub->getX() - offsetX, sub->getY() - offsetY );
    }

    //printRect(getBounds(), "current bounds");
    //printRect(Rectangle<int>( minx + offsetX, miny + offsetY, maxx-minx, maxy-miny), "min group bounds");

    auto temp = in_edit_mode;
    in_edit_mode = true;
    setTopLeftPosition( getX() + offsetX, getY() + offsetY);
    setSize(maxx-minx, maxy-miny); // n.b. resize function updates relative attributes
    in_edit_mode = temp;

}

void SymbolGroupComponent::scaleScoreComponent(float scale_w, float scale_h)
{
    
    cout << "SymbolGroupComponent::scaleScoreComponent " << scale_w << " " << scale_h << endl;
    
    BaseComponent::scaleScoreComponent(scale_w, scale_h); //<< base component only scales subcomponents
    
    setSize(getWidth() * scale_w, getHeight() * scale_h);
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
    
    BaseComponent* subComponent;
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        addr = base_address + "/subsymbol/" + String(i+1);
        if( s->getOSCMessagePos(addr) == -1 )
        {
            subComponent = dynamic_cast<BaseComponent*>(getSubcomponent(i));
            
            // Checks downcast result.
            if (subComponent != NULL)
                messages_added += subComponent->addSymbolMessages( s, addr );
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

