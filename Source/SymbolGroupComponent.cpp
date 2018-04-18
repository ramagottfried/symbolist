
#include "SymbolGroupComponent.h"

#include "PageComponent.h"
#include "ScoreComponent.h"

void SymbolGroupComponent::groupSelectedSymbols()
{
	DEBUG_TRACE();
	
	if ( selected_components.size() > 1 )
    {
    	DEBUG_FULL("Creating a group from " << selected_components.size()
										    << " selected components." << endl);
		
        // get the position an bounds of the group
        int minx = getWidth(), maxx = 0, miny = getHeight(), maxy = 0;
        for( auto it = selected_components.begin(); it != selected_components.end(); it++ )
        {
            Rectangle<int> compBounds = (*it)->getBounds();
            minx =  min( minx, compBounds.getX() );
            miny =  min( miny, compBounds.getY() );
            maxx =  max( maxx, compBounds.getRight() );
            maxy =  max( maxy, compBounds.getBottom() );
        }

        auto symbolistHandler = getSymbolistHandler();
		
		// Creating a temporary symbol, because it will not integrate the score.
        Symbol groupSymbol = Symbol();
        groupSymbol.setTypeXYWH("group", minx, miny, maxx-minx, maxy-miny);
		
        int count = 0;
		
		/* Adds a "/subsymbol" message in the group symbol bundle
		 * for each selected component.
		 */
        for (SymbolistComponent *c : selected_components)
        {
            auto selectedComponent = dynamic_cast<BaseComponent* >(c);
			
            // Checks downcast result.
            if (selectedComponent != NULL)
            {
            	/* Creates a symbol for the selectedComponent.
				 * Adding messages to the bundle is easier then.
            	 */
                Symbol associatedSymbol = selectedComponent->createSymbolFromComponent();
				
                if (associatedSymbol.size() > 0)
                {
                    // Copies bundle from subcomponent symbol and join into new group symbol
                    associatedSymbol.addMessage("/x", selectedComponent->getX() - minx);
                    associatedSymbol.addMessage("/y", selectedComponent->getY() - miny);
					
                    groupSymbol.addMessage( "/subsymbol/" + to_string(count++), associatedSymbol );
					
                }
				
            }
        }
		
        SymbolGroupComponent *group = dynamic_cast<SymbolGroupComponent*>(
									  	symbolistHandler->makeComponentFromSymbol(&groupSymbol, true)
									  );
        addSubcomponent(group);
        deleteSelectedComponents();
		
        addToSelection(group);
    }
	
	DEBUG_TRACE();
}

void SymbolGroupComponent::deleteSelectedComponents()
{
	DEBUG_TRACE();
	ScoreComponent::deleteSelectedComponents();
}

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

void SymbolGroupComponent::addSymbolMessages(Symbol* s )
{
    
//    cout << "SymbolGroupComponent::addSymbolMessages " << s << " " << base_address << " " << getNumSubcomponents() << endl;
    
    BaseComponent::addSymbolMessages(s);
    
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
   
    BaseComponent* subComponent; 
    Symbol* subSymbol = new Symbol();
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        subComponent = dynamic_cast<BaseComponent*>(getSubcomponent(i));
        if (subComponent != NULL)
        {
           subComponent->addSymbolMessages(subSymbol);
           s->addMessage( "/subsymbol/" + to_string(i), *subSymbol);
        }
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
            Symbol* sub_s = new Symbol(sub.getBundle().get_o_ptr());
            
            BaseComponent* c = getSymbolistHandler()->makeComponentFromSymbol(sub_s, false);
            
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

