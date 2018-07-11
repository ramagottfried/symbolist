#include "ScoreComponent.h"
#include "EditSelectionBox.h"
#include "SymbolistMainComponent.h"
#include "SymbolGroupComponent.h"
#include "StaffComponent.hpp"

ScoreComponent::ScoreComponent()
{
    addChildComponent( selection_box = new EditSelectionBox(&selected_components) );
    selection_box->setBorderThickness( BorderSize<int>(6) );
    selection_box->setAlwaysOnTop(true);
    selection_box->setVisible(false);
}

ScoreComponent::ScoreComponent(ScoreComponent* componentToCopy) : SymbolistComponent(componentToCopy)
{
	
	is_selected = componentToCopy->isSelected();
	
	for (ScoreComponent* subComponent : *(componentToCopy->getSubcomponents()))
	{
		subcomponents.add(new ScoreComponent(subComponent));
		
		if (componentToCopy->getSelectedItems().contains(subComponent))
			selected_components.add(subcomponents.getLast());
	}
	
	addChildComponent( selection_box = new EditSelectionBox(&selected_components) );
    selection_box->setBorderThickness( BorderSize<int>(6) );
    selection_box->setAlwaysOnTop(true);
    selection_box->setVisible(false);
}

ScoreComponent::~ScoreComponent()
{
    unselectAllComponents();
    clearAllSubcomponents();
}


void ScoreComponent::selectComponent()
{
    is_selected = true;
    repaint();
}

void ScoreComponent::deselectComponent()
{
    is_selected = false;
    repaint();
}

const size_t ScoreComponent::getNumSubcomponents()
{
    return subcomponents.size() ;
}

ScoreComponent* ScoreComponent::getSubcomponentByIndex( int i )
{
    return subcomponents[i] ;
}

ScoreComponent* ScoreComponent::getSubcomponentByID( const string& id )
{
    for( int i = 0; i < subcomponents.size(); i++ )
    {
        if( subcomponents[i]->getComponentID() == String(id) )
            return subcomponents[i];
    }
	
    return nullptr;
}

void ScoreComponent::addSubcomponent( ScoreComponent *c )
{
    subcomponents.add(c);
    addAndMakeVisible(c);
}

/**************/
/* Selection  */
/**************/

void ScoreComponent::addToSelection(ScoreComponent *c)
{
    if( selected_components.addIfNotAlreadyThere(c) )
    {
        c->selectComponent();
        selection_box->updateEditSelBox();
    }
}

void ScoreComponent::removeFromSelection(ScoreComponent *c)
{
    c->deselectComponent();
    selected_components.removeAllInstancesOf(c);
    selection_box->updateEditSelBox();
}

void ScoreComponent::selectAllComponents()
{
    for (int i = 0 ; i < getNumSubcomponents(); i++ )
    {
        addToSelection(getSubcomponentByIndex(i));
    }
}

void ScoreComponent::unselectAllComponents()
{
    for (int i = 0 ; i < getNumSubcomponents(); i++ )
    {
        ScoreComponent *c = getSubcomponentByIndex(i);
        
        c->deselectComponent();
        selected_components.removeAllInstancesOf(c);
    }
    selection_box->updateEditSelBox();
}

// redefinitions from SymbolComponents
void ScoreComponent::removeSubcomponent( ScoreComponent *c )
{
    removeFromSelection(c);
    removeChildComponent(c);
    for ( int i = 0; i < subcomponents.size(); i++ )
        if ( subcomponents[i] == c ) subcomponents.remove( i );
}

void ScoreComponent::clearAllSubcomponents()
{
    for ( int i = 0; i < subcomponents.size(); i++ )
    {
        subcomponents[i]->clearAllSubcomponents();
        removeChildComponent( subcomponents[i] );
        delete subcomponents[i];
    }
	
    subcomponents.clear();
    selected_components.clear();
}

Point<int> ScoreComponent::positionRelativeTo(ScoreComponent* to)
{
    ScoreComponent* parentComponent = dynamic_cast<ScoreComponent*>(getParentComponent());
    if (to == getParentComponent())
        return getPosition();
	
    // Checks downcast result.
    else if(parentComponent != NULL)
        return getPosition() + parentComponent->positionRelativeTo(to);
	
    return Point<int>(0, 0);
}

// basic selection mechanism
void ScoreComponent::mouseDownSelection(const MouseEvent& event)
{
    ScoreComponent* parent = dynamic_cast<ScoreComponent* >(getParentComponent());
	
    // Checks downcast exception.
    if (parent != NULL)
    {
        if (event.mods.isShiftDown())
        {
            if (isSelected())
                parent->removeFromSelection(this);
            else
                parent->addToSelection(this);
			
        }
        else
        {
            if (!isSelected())
            {
                parent->unselectAllComponents();
                parent->addToSelection(this);
            }
        }
    }
	
}

bool ScoreComponent::intersectRect( Rectangle<int> rect)
{
    return getBounds().intersects(rect);
}

void ScoreComponent::reportModificationForSelectedSymbols()
{
    // cout << "void ScoreComponent::reportModificationForSelectedSymbols()" << endl;
    BaseComponent* baseComponent;
    
    for( SymbolistComponent *symbolistComponent : selected_components )
    {
        baseComponent = dynamic_cast<BaseComponent*>(symbolistComponent);
        
        // Checks downcast result.
        if (baseComponent != NULL)
            baseComponent->reportModification();
    }
}

void ScoreComponent::selectedToFront()
{
    for( ScoreComponent *c : selected_components )
    {
        c->toFront(true);
    }
}

void ScoreComponent::selectedToBack()
{
    for( ScoreComponent *c : selected_components )
    {
        c->toBack();
    }
}

/*****************
 * Custom lasso tool
 *****************/

void ScoreComponent::beginLassoSelection(Point<int> position)
{
    unselectAllComponents();
    addAndMakeVisible(s_lasso);
    s_lasso.begin(position.getX(), position.getY());
}

void ScoreComponent::dragLassoSelection(Point<int> position)
{
    s_lasso.update(position.getX(), position.getY());
    
    for (int i = 0; i < getNumSubcomponents(); ++i)
    {
        ScoreComponent* cc = getSubcomponentByIndex(i);
		
        // Selects component if it intersects with the lasso.
        if (!cc->isSelected() && cc->intersectRect(s_lasso.getBounds()))
            addToSelection(cc);
		
		// Unselects if already selected but doesn't intersect.
		else if (cc->isSelected() && !cc->intersectRect(s_lasso.getBounds()))
		{
			cc->unselectAllComponents();
			
			cc->deselectComponent();
			selected_components.removeAllInstancesOf(cc);
			selection_box->updateEditSelBox();
		}
		
    }
}

void ScoreComponent::endLassoSelection()
{
    removeChildComponent(&s_lasso);
    s_lasso.end();
}

void SymbolistLasso::begin(int x, int y)
{
    start_x = x;
    start_y = y;
    setBounds(x, y, 0, 0);
}

void SymbolistLasso::update(int x, int y)
{
    int x1, x2, y1, y2;
    
    if (x > start_x)
    {
        x1 = start_x;
        x2 = x;
    } else {
        x1 = x;
        x2 = start_x;
    }
    
    if (y > start_y)
    {
        y1 = start_y;
        y2 = y;
    } else {
        y1 = y;
        y2 = start_y;
    }
    
    setBounds(x1, y1, x2-x1, y2-y1);
}

void SymbolistLasso::end() {}


void SymbolistLasso::paint ( Graphics &g)
{
    g.setColour(Colours::cornflowerblue);
    g.drawRect(0, 0, getWidth(), getHeight());
}


/**************************
 * User actions
 **************************/

void ScoreComponent::deleteSelectedComponents()
{
    vector<ScoreComponent *> items;
    
    for ( ScoreComponent *c : selected_components ) // there's probably a better way to copy a vector's contents :)
    {
        DEBUG_FULL(c << endl)
        items.push_back(c);
    }
    
    unselectAllComponents();
    
    for ( ScoreComponent *c : items )
    {
        removeSubcomponent( c );
        delete c;
    }
	
}

void ScoreComponent::groupSelectedSymbols()
{
	if ( selected_components.size() > 1 )
    {
    	bool isTopLevelGroup = ( this == getPageComponent() );

    	DEBUG_FULL("Creating a group (top level? " << isTopLevelGroup << "), from "
												   << selected_components.size()
												   << " selected components." << endl)

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

        Symbol* groupSymbol = symbolistHandler->createSymbol();
        groupSymbol->setTypeXYWH("group", minx, miny, maxx-minx, maxy-miny);

        int count = 0;

        for (SymbolistComponent *c : selected_components)
        {
            auto selectedComponent = dynamic_cast<BaseComponent*>(c);

            // Checks downcast result.
            if (selectedComponent != NULL)
            {
                auto associatedSymbol = selectedComponent->getScoreSymbol();
                if (associatedSymbol->size() > 0)  // this fails within groups because subcomponents do not have score symbols...
                {
                    // copies bundles from subcomponent symbols and join into new group symbol
                    Point<float> symbolPos = selectedComponent->computeSymbolPosition( selectedComponent->getX() - minx, selectedComponent->getY() - miny,
                                                                                       selectedComponent->getWidth() , selectedComponent->getHeight() );
                    associatedSymbol->addMessage("/x", symbolPos.getX() );
                    associatedSymbol->addMessage("/y", symbolPos.getY() );

                    groupSymbol->addMessage( "/subsymbol/" + to_string(count++), *associatedSymbol );
                }
            }
        }

        SymbolGroupComponent *group = dynamic_cast<SymbolGroupComponent*>(
									  	symbolistHandler->makeComponentFromSymbol(groupSymbol, isTopLevelGroup)
									  );
        addSubcomponent(group);

        getPageComponent()->deleteSelectedComponents();

        addToSelection(group);
    }
}

void ScoreComponent::ungroupSelectedSymbols()
{
    vector<SymbolistComponent* > items;
    for( SymbolistComponent *c : selected_components )
        items.push_back(c);
	
    unselectAllComponents();
    
    for ( int i = 0; i < items.size(); i++ )
    {
        BaseComponent* c = dynamic_cast<BaseComponent*>(items[i]);
        
        // Checks downcast result.
        if (c != NULL)
        {
            int n = ((int)c->getNumSubcomponents());
            
            vector<SymbolistComponent* > subitems;
            for ( int ii = 0 ; ii < n ; ii++ ) subitems.push_back(c->getSubcomponentByIndex(ii));
            
            for ( int ii = 0; ii < n ; ii++ )
            {
                BaseComponent* sc = dynamic_cast<BaseComponent* >(subitems[ii]);
                
                // Checks downcast result.
                if (sc != NULL)
                {
                	/* If sc is removed from c's childs list then sc has no parent component
					 * and cannot call the SymbolistHandler to create and attach a new symbol.
                	 */
                    if ( c->isTopLevelComponent() ) sc->createAndAttachSymbol();
					
                    c->removeSubcomponent(sc);
                    addSubcomponent(sc);
					
                    sc->setTopLeftPosition(sc->getPosition().translated(c->getPosition().getX(), c->getPosition().getY()));
                }
        
            }
            
            removeSubcomponent(c);
        }
        
    }
}

void ScoreComponent::translateSelectedComponents( Point<int> delta_xy )
{
    for ( auto c : selected_components )
    {
        c->setTopLeftPosition( c->getPosition() + delta_xy );
    }
    selection_box->updateEditSelBox();
}

Rectangle<int> ScoreComponent::getSelectionBounds()
{
    return selection_box->getSelectionBounds();
}

void ScoreComponent::flipSelectedSymbols( int axis )
{
    selection_box->flipSelectedSymbols( axis );
}

void ScoreComponent::nudgeSelected(int direction)
{
    switch(direction)
    {
        case 0:
            translateSelectedComponents( Point<int>(-1,0) );
            break;
        case 1:
            translateSelectedComponents( Point<int>(1,0) );
            break;
        case 2:
            translateSelectedComponents( Point<int>(0,-1) );
            break;
        case 3:
            translateSelectedComponents( Point<int>(0,1) );
            break;
    }
}

/***************************/
/* UI callbacks from Juce  */
/***************************/

void ScoreComponent::mouseAddClick(const MouseEvent& event)
{
    unselectAllComponents();

    BaseComponent *newComponent;
    
    bool topLevel = (this == getPageComponent());
    auto mainController = getSymbolistHandler();

    if (getMainDrawMode() == UI_DrawType::FROM_TEMPLATE)
    {
        /* Creates a new symbol with the same settings as the selected
         * symbol template in the palette.
         * Template symbols all have a default type of "path" and bounds of 0,0,30,30
         * the generic symbol has the same OSC data as the BaseComponent.
         */
        Symbol* newSymbol = mainController->createSymbolFromTemplate();
        
        // Sets default position before creating the graphic component.
        newSymbol->setPosition(event.position);
		
        // Creates a new component of the current selected symbol type.
        newComponent = mainController->makeComponentFromSymbol(newSymbol, topLevel);
        
        // Adds component in the view.
        addSubcomponent(newComponent);
		
    }
    else
    {
        Symbol* newSymbol = mainController->createSymbol();
        newSymbol->setTypeXYWH("path", event.position.x, event.position.y, 40.0, 40.0) ;
        
        newComponent = mainController->makeComponentFromSymbol(newSymbol, topLevel);
        addSubcomponent(newComponent);
        
        getPageComponent()->enterEditMode(newComponent);
        newComponent->mouseAddClick(event.getEventRelativeTo(newComponent));
    }

    if (!topLevel)
        newComponent->reportModification();
    
    // deselect other items and select this one
    //addToSelection( c );
}

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    UI_EditType ed = getMainMouseMode();
    if( ed == SELECTION )
    {
        beginLassoSelection( event.getPosition() );
    }
    else if( ed == DRAW )
    {
        mouseAddClick(event.getEventRelativeTo(getPageComponent()));
    }
}

void ScoreComponent::mouseDrag(const MouseEvent& event)
{
    if (getMainMouseMode() == SELECTION)
    {
        dragLassoSelection(event.getPosition());
    }
}

void ScoreComponent::mouseUp ( const MouseEvent& event )
{
    UI_EditType ed = getMainMouseMode();
    
    if ( ed == SELECTION )
    {
        endLassoSelection();
    }
    else if ( ed == DRAW )
    {
        // when the mousedown on this triggered an entry to edit mode, we might want to pass the mouse up there, too
        ScoreComponent* sc = getPageComponent()->getEditedComponent();
        if (sc != this)
            sc->mouseUp(event.getEventRelativeTo(sc));
    }
}




