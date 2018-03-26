
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
        auto b = ((BaseComponent *)getSubcomponent(i));
        b->h_flip(ax - getX(), ay - getY());
    }
}

void SymbolGroupComponent::v_flip(float ax, float ay)
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = ((BaseComponent *)getSubcomponent(i));
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
        auto b = ((BaseComponent *)getSubcomponent(i));
        b->rotateScoreComponent(theta, ax - getX(), ay - getY() );
        
        minx =  min( minx, getX() + b->getX() );
        miny =  min( miny, getY() + b->getY() );
        maxx =  max( maxx, getX() + b->getRight() );
        maxy =  max( maxy, getY() + b->getBottom() );
        
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

void SymbolGroupComponent::addSymbolMessages( Symbol* s )
{
    
//    cout << "SymbolGroupComponent::addSymbolMessages " << s << " " << base_address << " " << getNumSubcomponents() << endl;
    
    BaseComponent::addSymbolMessages( s );
    
    s->addMessage( "/numsymbols", (int)getNumSubcomponents() );

    /*
     * the group symbol is created first (with the default state), then added to, then updated,
     * so we need (getNumSubcomponents() > 0) to wait to set /numsymbols until after the symbol has been updated,
     * otherwise, /numsymbols gets stuck at 0
    
    
     i don't understand this again, commenting out to test
    if( getNumSubcomponents() )
    {
        s->addMessage( addr, (int)getNumSubcomponents() );
    }
     */
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        Symbol sub_sym;
        ((BaseComponent*)getSubcomponent(i))->addSymbolMessages( &sub_sym );
        s->addMessage( "/subsymbol/" + to_string(i), sub_sym );
        //s->print();
    }
    
}

void SymbolGroupComponent::importFromSymbol( const Symbol &s )
{
    clearAllSubcomponents();
    
    BaseComponent::importFromSymbol(s);
    
    auto subsymbols = s.matchAddress( "/subsymbol", false ); // later try /* with full match at default true
    
    int count = 0;
    for ( auto sub : subsymbols )
    {
        if( sub[0].isBundle() )
        {
            cout << "IMPORT FROM: " << sub.getAddress() << endl;
            Symbol sub_s( sub.getBundle().get_o_ptr() );
            
            BaseComponent* c = getSymbolistHandler()->makeComponentFromSymbol( &sub_s , false );
            
            if ( c != NULL)
            {
                addSubcomponent( c );
                count++;
            }
            else
                cout << "Error importing subsymbol #" << count << endl;
        }
    }
    
    std::cout << "Imported Group of " << count << " symbols..." << std::endl;
}

