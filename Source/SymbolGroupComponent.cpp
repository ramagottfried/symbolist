
#include "SymbolGroupComponent.h"

#include "PageComponent.h"
#include "ScoreComponent.h"

void SymbolGroupComponent::groupSelectedSymbols()
{
	if ( selected_components.size() > 1 )
    {
    	DEBUG_FULL("Creating a group from " << selected_components.size()
										    << " selected components." << endl)
		
        PageController* controller = getPageComponent()->getController();
		Symbol symbolGroup = controller->createNestedSymbolGroup(selected_components, this);
		
        SymbolGroupComponent *groupComponent = dynamic_cast<SymbolGroupComponent*>(
									  	controller->makeComponentFromSymbol(&symbolGroup, true)
									  );
		
		addSubcomponent(groupComponent);
        deleteSelectedComponents();
		
		/* Adds a new /subsymbol entry in the symbol bundle
		 * attached to this SymbolGroupComponent.
		 */
		if (getScoreSymbol() != NULL)
			getScoreSymbol()->addMessage("/subsymbol/" + to_string(getNumSubcomponents() + 1), symbolGroup);
		
        addToSelection(groupComponent);
    }
	
}

void SymbolGroupComponent::deleteSelectedComponents()
{
	DEBUG_TRACE()
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
        if( getSubcomponentByIndex(i)->hitTest ( x - getSubcomponentByIndex(i)->getX() , y - getSubcomponentByIndex(i)->getY() ) )
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
        SymbolistComponent *sub = getSubcomponentByIndex( i );
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
            getSubcomponentByIndex(i)->selectComponent();
        }
    }
}

void SymbolGroupComponent::deselectComponent()
{
    BaseComponent::deselectComponent();
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        getSubcomponentByIndex(i)->deselectComponent();
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
        if ( getSubcomponentByIndex(i)->intersectRect( rect.translated( -getX(), -getY()) ) ) return true ;
    }
    return false;
}

void SymbolGroupComponent::h_flip(float ax, float ay)
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = dynamic_cast<BaseComponent*>(getSubcomponentByIndex(i));
        
        // Checks the downcast result.
        if (b != NULL)
            b->h_flip(ax - getX(), ay - getY());
    }
}

void SymbolGroupComponent::v_flip(float ax, float ay)
{
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        auto b = dynamic_cast<BaseComponent*>(getSubcomponentByIndex(i));
        
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
        auto b = dynamic_cast<BaseComponent*>(getSubcomponentByIndex(i));
        
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
        auto sub = getSubcomponentByIndex(i);
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
    
    BaseComponent::scaleScoreComponent(scale_w, scale_h); // base component only scales subcomponents
    
    setSize(getWidth() * scale_w, getHeight() * scale_h);
}

/*============================*
 * SYMBOL MANAGEMENT
 *============================*/

void SymbolGroupComponent::addSymbolMessages(Symbol* s )
{
        
    BaseComponent::addSymbolMessages(s);
    s->addMessage( "/numsymbols", (int)getNumSubcomponents() );
   
    BaseComponent* subComponent;
    Symbol subSymbol = Symbol();

    /* For each subcomponent of this SymbolGroupComponent instance
	 * add a /subsymbol message in s.
     */
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        subComponent = dynamic_cast<BaseComponent*>(getSubcomponentByIndex(i));
        if (subComponent != NULL)
        {
			subComponent->addSymbolMessages(&subSymbol);
           	s->addMessage( "/subsymbol/" + to_string(i), subSymbol);
			
			String componentId = subComponent->getComponentID();
			string typeOfSymbol = s->getType();
			
        }
		
    }
	
}

void SymbolGroupComponent::importFromSymbol( const Symbol &symbol )
{
    clearAllSubcomponents();
    
    BaseComponent::importFromSymbol(symbol);
	
    // Returns the list of OdotMessage in s which address matches "/subsymbol"
    auto subsymbols = symbol.matchAddress( "/subsymbol", false ); // later try /* with full match at default true
    
    int count = 0;
    for ( auto subsymbolMessage : subsymbols )
    {
    	/* Verifies that the first argument of
    	 * the subsymbol message is a bundle.
    	 */
        if( subsymbolMessage[0].isBundle() )
        {
            DEBUG_FULL("IMPORT FROM: " << subsymbolMessage.getAddress() << endl)
            Symbol subSymbol = Symbol(subsymbolMessage.getBundle().get_o_ptr());
			
			BaseComponent* c = getSymbolistHandler()->makeComponentFromSymbol(&subSymbol, false);
			
			if (c != NULL)
			{
				addSubcomponent(c);
				count++;
			}
			else
				DEBUG_FULL("Error importing subsymbol #" << count << endl)
			
        }
    }
    
    DEBUG_FULL("Imported Group of " << count << " symbols..." << endl)
}

