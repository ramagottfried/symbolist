#include "PageComponent.h"
#include "SymbolistMainComponent.h"


PageComponent::PageComponent()
{
    setComponentID("PageComponent");
    edited_component = NULL;
    addAndMakeVisible(score_cursor);
}

PageComponent::~PageComponent() {}

/***************************************************/
/* MODIFICATIONS TO BE TRANSFERRED TO THE SCORE    */
/* will update the data (score) and notify to host environment */
/***************************************************/

void PageComponent::addSubcomponent(ScoreComponent *c)
{
    ScoreComponent::addSubcomponent(c);
    getSymbolistHandler()->executeUpdateCallback(-1);
}

void PageComponent::removeSubcomponent( ScoreComponent *c )
{
	BaseComponent* componentToRemove = dynamic_cast<BaseComponent* >(c);
	
	if (componentToRemove != NULL)
	{
		if (getController() != NULL)
			getController()->removeAttachedSymbolFromScore(componentToRemove);
		else throw logic_error("PageComponent as no associated controller");
	}
	
    ScoreComponent::removeSubcomponent( c );
}

void PageComponent::groupSelectedSymbols()
{	
	if ( selected_components.size() > 1 )
    {
    	DEBUG_FULL("Creating a top level group from " << selected_components.size()
												      << " selected components." << endl)
		if (getController() != NULL)
		{
			Symbol* symbolGroup = getController()->createTopLevelSymbolGroup(selected_components);
	
			/* Creates the corresponding graphic component and adds it to
			 * this PageComponent instance.
			 */
			SymbolGroupComponent *group = dynamic_cast<SymbolGroupComponent*>(
											getController()->makeComponentFromSymbol(symbolGroup, true)
										  );
			
			/* Adds the new symbol group component as a subcomponent of this PageComponent
			 * and destroy the previously selected components and their score symbols.
			 */
			addSubcomponent(group);
			deleteSelectedComponents();
			
			addToSelection(group);
		}
		else throw logic_error("PageComponent as no associated controller");
    }
}

void PageComponent::copySelectedToClipBoard()
{
	if (getController() != NULL)
		getController()->copySelectedToClipBoard();
	else throw logic_error("PageComponent as no associated controller");
}

void PageComponent::newFromClipBoard()
{
	if (getController() != NULL)
		getController()->newFromClipBoard();
	else throw logic_error("PageComponent as no associated controller");
}

void PageComponent::enterStaffSelMode()
{
    exitEditMode();
    
    for( auto subComponent : subcomponents )
    {
    	// If it's a staff the dynamic_cast prob isn't necessary
        BaseComponent* b = dynamic_cast<BaseComponent* >(subComponent);
        if ( b )
        {
            if( b->getSymbolTypeStr() == "staff" || b->isSelected() )
                b->setStaffSelectionMode( true );
			else
			{
                    b->setStaffSelectionMode( false );
                    b->setVisible(false);
			}
			
        }
       
    }
    
    display_mode = STAFF;
}

void PageComponent::exitStaffSelMode()
{

    for( auto c : subcomponents )
    {
        BaseComponent* b = dynamic_cast<BaseComponent*>(c);
        if( b )
        {
            b->setStaffSelectionMode( false );
            b->setVisible(true);
        }
    }
    
    display_mode = MAIN;

}

void PageComponent::createStaffFromSelected()
{
    auto selectedItems = getSelectedItems();
    if( selectedItems.size() > 1 )
        groupSelectedSymbols();
	
    auto staffRefComponent = dynamic_cast<BaseComponent* >(getSelectedItems().getFirst());
	
    // Checks downcast result.
    if( staffRefComponent != NULL )
    {
    	// Embedding staff in staff is forbidden.
        if( staffRefComponent->getSymbolTypeStr() == "staff" )
            return;
			
        Symbol* staffSymbol = getController()->createStaff(staffRefComponent);
		
        // Creates the new component and attach the new symbol pointer to it.
        StaffComponent *staffComponent = dynamic_cast<StaffComponent* >(getController()->makeComponentFromSymbol(staffSymbol, true));

        /* Removes from parent (which also sets the refSymbol to NULL)
         * the parent is not necessarily 'this' (selected_items can be indirect children...)
         */
        ScoreComponent* parentOfStaffRefComponent = dynamic_cast<ScoreComponent* >(staffRefComponent->getParentComponent());
		
        // Checks downcast result.
        if (parentOfStaffRefComponent != NULL)
            parentOfStaffRefComponent->removeSubcomponent( staffRefComponent );
		
        // Sets the position now relative to the group.
        staffRefComponent->setBounds( 0, 0, staffRefComponent->getWidth(), staffRefComponent->getHeight() );
		
        /* Add subcomponent to the parent staff component.
         * Indeed, a staff component is a component group.
         */
        staffComponent->addSubcomponent( staffRefComponent );
		
        /* Once the subcomponent is in place, the attached staff symbol can be updated.
         * Note: add symbol messages does not attach the symbol, it just adds the messages.
         */
        staffComponent->addSymbolMessages( staffSymbol );
		
        addSubcomponent(staffComponent);
        addToSelection(staffComponent);
		
    }
	
}

vector<BaseComponent*> PageComponent::getSubcomponentsByStaff( String& staff_name )
{
    vector<BaseComponent*> objects;
    Symbol* s = NULL;
    for( int i = 0; i < subcomponents.size(); i++ )
    {
        BaseComponent *c = dynamic_cast<BaseComponent*>(subcomponents[i]);
        if( c )
        {
            s = c->getScoreSymbol();
            
            if( s->getStaff() == staff_name )
                objects.emplace_back( c );
        }
    }
    
    return objects;
}

void PageComponent::enterEditMode( BaseComponent* c )
{
    exitEditMode();
    unselectAllComponents();
    edited_component = c;
    edited_component->setEditMode(true);
    edited_component->toFront(true);
    edited_component->recursiveMaximizeBounds();
    
    display_mode = EDIT;

}


void PageComponent::exitEditMode( )
{
    if ( edited_component != NULL )
    {
        edited_component->recursiveShrinkBounds();
        edited_component->reportModification();
		
        // reportModificaiton must be before setting Edit mode to handle case of repositioned sub symbols.
        edited_component->setEditMode(false);
        edited_component = NULL;
    }
    
    display_mode = MAIN;

}

ScoreComponent* PageComponent::getEditedComponent()
{
    if ( edited_component == NULL )
        return this;
    else
        return edited_component;
}

void PageComponent::resized()
{
    if (edited_component != NULL)
    {
        edited_component->recursiveMaximizeBounds();
    }
    
    score_cursor.setBounds( score_cursor.getPlayPoint() * 100, 0, 50, getHeight() );
}


/************************/
/* Draws the score page */
/************************/

void PageComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
/*
    auto visibleBounds = getMainComponent()->getViewRect() ;
    String timestr = " t = ";
    timestr += (String) (getSymbolistHandler()->getCurrentTime()) ;
    g.drawText (timestr, visibleBounds , Justification::topLeft, false);
  */
}
