
#include "StaffComponent.hpp"

#include "PageComponent.h"
#include "ScoreComponent.h"

StaffComponent::~StaffComponent()
{

	for (BaseComponent* component : components_on_staff)
		removeStaffObject(component);
}

void StaffComponent::importFromSymbol( const Symbol &s )
{
    clearAllSubcomponents();
    BaseComponent::importFromSymbol(s);
	
    // there can be only one staff subsymbol, must be grouped if multiple
    Symbol subsymbol = Symbol(s.getMessage( "/subsymbol" ).getBundle().get_o_ptr());
    
    if (subsymbol.size() == 0)
    {
        DEBUG_FULL("No staff subsymbol found" << endl)
        return;
    }
	
    BaseComponent* c = getSymbolistHandler()->makeComponentFromSymbol(&subsymbol, false);
    
    if (c != NULL)
    {
    	DEBUG_FULL("Adding " << c->getComponentID() << " to " << getComponentID() << endl)
		addSubcomponent(c);
	}
    else
        DEBUG_FULL("Error importing staffSymbol " << endl);
        
}

void StaffComponent::addSymbolMessages(Symbol* s)
{
    BaseComponent::addSymbolMessages(s);

    if ( getNumSubcomponents() > 0 )
    {
        auto soleSubComponent = getSubcomponentByIndex(0);
        if ( soleSubComponent )
        {
            BaseComponent* staffSubComponent = dynamic_cast<BaseComponent*>(soleSubComponent);
            
            // Checks downcast result and if component is attached to a score symbol.
            if (staffSubComponent != NULL)
            {
				Symbol staffSubSymbol = Symbol();
				staffSubComponent->addSymbolMessages(&staffSubSymbol);
		   		s->addMessage("/subsymbol", staffSubSymbol);
			}
			
        }
        else
            DEBUG_FULL("No subcomponent found." << endl);
    }
    
}

void StaffComponent::parentHierarchyChanged()
{
    BaseComponent::parentHierarchyChanged();
    
    PageComponent* scoreView = getPageComponent();
    if (scoreView)
    {
        Symbol* staffSymbol = getScoreSymbol();
        if (staffSymbol != NULL)
        {
            String id = staffSymbol->getID();
        
            auto subComponents = scoreView->getSubcomponentsByStaff( id );
            for( auto subComponent : subComponents )
                addObjectToStave( subComponent );
            
            subComponents.clear();
        }
    }
    
}

bool StaffComponent::hitTest (int x, int y)
{
    if( in_staff_selection_mode )
    {
        return true;
    }
    
    return SymbolGroupComponent::hitTest ( x, y );
    
}

void StaffComponent::mouseDown( const MouseEvent& event )
{
    
    if( in_staff_selection_mode )
    {
        for( auto s : getPageComponent()->getSelectedItems() )
        {
            BaseComponent *c = dynamic_cast<BaseComponent* >(s);
            
            // Checks downcast result.
            if( c != NULL && c->getSymbolTypeStr() != "staff" )
            {
				addObjectToStave(c);
				c->setStaff(this);
				getSymbolistHandler()->modifySymbolInScore(c);
            }
        }
    }
    else
    {
       BaseComponent::mouseDown(event);
    }
}

void StaffComponent::mouseDrag( const MouseEvent& event )
{
    PageComponent* scoreView = getPageComponent();

	if (scoreView == NULL)
		return;
	
    if ( is_selected )
        for( auto it = components_on_staff.begin(); it != components_on_staff.end(); it++ )
			scoreView->addToSelection( *it );
	
    BaseComponent::mouseDrag( event );
    
    scoreView->updateTimeCursor();
}

void StaffComponent::paint(Graphics& g)
{
    BaseComponent::paint( g );
 
    if( draw_timepoints )
    {
    	if (getPageComponent() != NULL)
    	{
			auto& timePointArray = getPageComponent()
										->getModel()
										->getScore()
										->getTimePoints()->getConstSymbolTimePoints();
			
			float startTime = getScoreSymbol()->getTime();
			float endTime = startTime + getScoreSymbol()->getDuration();
			
			for (int i = 0; i < timePointArray.size(); i++)
			{
				auto& t = timePointArray[i];
				if( t->time >= startTime && t->time <= endTime )
					g.fillEllipse( (t->time - startTime) * 100.0f, getHeight() / 2, 2, 2);
			}
		}
		
    }
    
    if ( in_staff_selection_mode )
    {
        g.setColour( Colours::lightblue );
        g.fillRect( getLocalBounds() );
        
        auto f = g.getCurrentFont();
        f.setItalic(true);
        f.setHeight( 10 );
        g.setFont( f );
        
        g.setColour( Colours::black );
        g.drawText( getScoreSymbol()->getID(), getLocalBounds().reduced(10), Justification::centredLeft );
    }
}
