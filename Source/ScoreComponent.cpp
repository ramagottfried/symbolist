#include "ScoreComponent.h"
#include "SymbolistMainComponent.h"
#include "SymbolGroupComponent.h"
#include "StaffComponent.hpp"

ScoreComponent::ScoreComponent()
{
    addChildComponent( sel_resize_box = new EditSelectionBox(&selected_components) );
    sel_resize_box->setBorderThickness( BorderSize<int>(6) );
    sel_resize_box->setAlwaysOnTop(true);
    sel_resize_box->setVisible(false);
}

ScoreComponent::~ScoreComponent()
{
    unselectAllComponents();
    clearAllSubcomponents();
}

/**************/
/* Selection  */
/**************/

void ScoreComponent::addToSelection(SymbolistComponent *c)
{
    if( selected_components.addIfNotAlreadyThere(c) )
    {
        c->selectComponent();
        sel_resize_box->updateEditSelBox();
    }
}

void ScoreComponent::removeFromSelection(SymbolistComponent *c)
{
    c->deselectComponent();
    selected_components.removeAllInstancesOf(c);
    sel_resize_box->updateEditSelBox();
}

void ScoreComponent::selectAllComponents()
{
    for (int i = 0 ; i < getNumSubcomponents(); i++ )
    {
        addToSelection(getSubcomponent(i));
    }
}

void ScoreComponent::unselectAllComponents()
{
    for (int i = 0 ; i < getNumSubcomponents(); i++ )
    {
        SymbolistComponent *c = getSubcomponent(i);
        
        c->deselectComponent();
        selected_components.removeAllInstancesOf(c);
    }
    sel_resize_box->updateEditSelBox();
}

// redefinitions from SymbolComponents
void ScoreComponent::removeSubcomponent( SymbolistComponent *c )
{
    removeFromSelection(c);
    SymbolistComponent::removeSubcomponent( c );
}

void ScoreComponent::clearAllSubcomponents()
{
    SymbolistComponent::clearAllSubcomponents();
    selected_components.clear();
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
    for( SymbolistComponent *c : selected_components )
    {
        c->toFront(true);
    }
}

void ScoreComponent::selectedToBack()
{
    for( SymbolistComponent *c : selected_components )
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

    // this slows things down noticably where there are a lot of objects selected
    unselectAllComponents();
    
    for (int i = 0; i < getNumSubcomponents(); ++i)
    {
        SymbolistComponent* cc = getSubcomponent(i);
        
        if (!cc->componentSelected() && cc->intersectRect(s_lasso.getBounds()))
            addToSelection(cc);
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
    vector<SymbolistComponent *> items;
    
    for ( SymbolistComponent *c : selected_components ) // there's probably a better way to copy a vector's contents :)
    {
        DEBUG_FULL(c << endl);
        items.push_back(c);
    }
    
    unselectAllComponents();
    
    for ( SymbolistComponent *c : items )
    {
        removeSubcomponent( c );
        delete c;
    }
	
}

void ScoreComponent::groupSelectedSymbols()
{
	if ( selected_components.size() > 1 )
    {
    	bool creating_a_top_level_group = ( this == getPageComponent() );
    
  /*
        scoreView->groupSelectedSymbols();
    }
    
    auto staff_ref_comp = dynamic_cast<BaseComponent*>(scoreView->getSelectedItems().getFirst());
    
    // Checks downcast result.
    if( staff_ref_comp != NULL )
    {
        if( staff_ref_comp->getSymbolType() == STAFF )
            return;
        
        Symbol ref_sym = *(staff_ref_comp->getScoreSymbolPointer());
        
        auto sh = getSymbolistHandler();
        
        // Calls controller to create new symbol in score.
        Symbol* staff_sym = sh->createSymbol();
        staff_sym->setTypeXYWH("staff", staff_ref_comp->getX(), staff_ref_comp->getY(), staff_ref_comp->getWidth(), staff_ref_comp->getHeight() );
        
        // create the new component and attach the new symbol pointer to it
        StaffComponent *staff_comp = (StaffComponent *)sh->makeComponentFromSymbol(staff_sym, true);
 */

    	DEBUG_FULL("Creating a group (top level? " << creating_a_top_level_group << "), from "
												   << selected_components.size()
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

        Symbol* groupSymbol = symbolistHandler->createSymbol();
        groupSymbol->setTypeXYWH("group", minx, miny, maxx-minx, maxy-miny);

        int count = 0;

        for (SymbolistComponent *c : selected_components)
        {
            auto selectedComponent = dynamic_cast<BaseComponent*>(c);

            // Checks downcast result.
            if (selectedComponent != NULL)
            {
                auto associatedSymbol = selectedComponent->getScoreSymbolPointer();
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
									  	symbolistHandler->makeComponentFromSymbol(groupSymbol, creating_a_top_level_group)
									  );
        addSubcomponent(group);

        getPageComponent()->deleteSelectedComponents();

        addToSelection(group);
    }
}

void ScoreComponent::ungroupSelectedSymbols()
{
    vector<SymbolistComponent*> items;
    for( SymbolistComponent *c : selected_components ) {
        items.push_back(c);
    }
    unselectAllComponents();
    
    for ( int i = 0; i < items.size(); i++ )
    {
        BaseComponent* c = dynamic_cast<BaseComponent*>(items[i]);
        
        // Checks downcast result.
        if (c != NULL)
        {
            int n = ((int)c->getNumSubcomponents());
            
            vector< SymbolistComponent *> subitems;
            for ( int ii = 0 ; ii < n ; ii++ ) { subitems.push_back(c->getSubcomponent(ii)); }
            
            for ( int ii = 0; ii < n ; ii++ )
            {
                BaseComponent* sc = dynamic_cast<BaseComponent*>(subitems[ii]);
                
                // Checks downcast result.
                if (sc != NULL)
                {
                    c->removeSubcomponent(sc);
                    if ( c->isTopLevelComponent() ) sc->createAndAttachSymbol() ;
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
    sel_resize_box->updateEditSelBox();
}

Rectangle<int> ScoreComponent::getSelectionBounds()
{
    return sel_resize_box->getSelectionBounds();
}

void ScoreComponent::flipSelectedSymbols( int axis )
{
    sel_resize_box->flipSelectedSymbols( axis );
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

    BaseComponent *c;
    
    bool top_level = (this == getPageComponent());
    auto sh = getSymbolistHandler();

    if (getMainDrawMode() == UI_DrawType::FROM_TEMPLATE)
    {
        /* Creates a new symbol with the same settings as the selected
         * symbol template in the palette.
         * Template symbols all have a default type of "path" and bounds of 0,0,30,30
         * the generic symbol has the same OSC data as the BaseComponent.
         */
        Symbol* s = sh->createSymbolFromTemplate();
        
        // Sets default position before creating the graphic component.
        s->setPosition(event.position);
        
        // Creates a new component of the current selected symbol type.
        c = sh->makeComponentFromSymbol(s, top_level);
        
        // Adds component in the view.
        addSubcomponent(c);
    }
    else
    {
        Symbol* s = sh->createSymbol();
        s->setTypeXYWH("path", event.position.x, event.position.y, 40.0, 40.0) ;
        
        c = sh->makeComponentFromSymbol(s, top_level);
        addSubcomponent(c);
        
        getPageComponent()->enterEditMode(c);
        c->mouseAddClick(event.getEventRelativeTo(c));
    }

    if (!top_level)
        c->reportModification();
    
    // deselect other items and select this one
    //addToSelection( c );
}

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    //getPageComponent()->getEditedComponent()->unselectAllComponents();
    //getPageComponent()->exitEditMode() ;
    
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
    
    if( ed == SELECTION )
    {
        endLassoSelection();
    }
    else if( ed == DRAW )
    {
        // when the mousedown on this triggered an entry to edit mode, we might want to pass the mouse up there, too
        ScoreComponent* sc = getPageComponent()->getEditedComponent();
        if (sc != this)
            sc->mouseUp(event.getEventRelativeTo(sc));
    }
}




