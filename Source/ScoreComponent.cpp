

#include "ScoreComponent.h"
#include "MainComponent.h"
#include "SymbolGroupComponent.h"


ScoreComponent::~ScoreComponent()
{
    selected_items.deselectAll(); //<< required to avoid callback after deleting components
    clearAllSubcomponents();
}


/*****************************
 * Management of sucomponents
 * Add/remove operations apply on views only
 *****************************/

const size_t ScoreComponent::getNumSubcomponents()
{
    return subcomponents.size() ;
}

BaseComponent* ScoreComponent::getSubcomponent( int i )
{
    return subcomponents.at(i) ;
}

void ScoreComponent::addSubcomponent( BaseComponent *c )
{
    subcomponents.emplace_back( c ) ;
    
    c->setComponentID(String(String(c->getSymbolTypeStr()) += String("_") += String(subcomponents.size())));
    addAndMakeVisible( c );
    
    c->addMouseListener(this, false);
    
}

void ScoreComponent::removeSubcomponent( BaseComponent *c )
{
    removeChildComponent(c);
    subcomponents.erase ( std::remove(subcomponents.begin(),subcomponents.end(), c) , subcomponents.end() );
}

void ScoreComponent::clearAllSubcomponents()
{
    for ( int i = 0; i < subcomponents.size(); i++ )
    {
<<<<<<< HEAD
        //removeChildComponent(subcomponents[i]);
=======
        subcomponents[i]->clearAllSubcomponents();
        removeChildComponent(subcomponents[i]);
>>>>>>> aef4be8994bf72e056a4a3809c014467bf8d99b5
        delete subcomponents[i];
    }
    subcomponents.clear();
}


/**************************
 * Apply on the view but except for PageComponent subclass => propagates to the Score
 **************************/

void ScoreComponent::addSymbolComponent ( BaseComponent* c )
{
    addSubcomponent( c );
}

void ScoreComponent::removeSymbolComponent( BaseComponent* c )
{
    removeSubcomponent( c );
}


/**************************
 * UI callbacks
 **************************/

void ScoreComponent::deleteSelectedSymbols()
{
    vector< BaseComponent *> items;
    
    for( BaseComponent *c : selected_items ) // there's probably a better way to copy a vector's contents :)
    {
        items.push_back(c);
    }
    
    selected_items.deselectAll();
    
    for( BaseComponent *c : items )
    {
        removeSymbolComponent( c );
        delete c;
    }
}


/************************/
/* Grouping             */
/************************/

void ScoreComponent::groupSelectedSymbols()
{
    if ( selected_items.getNumSelected() > 1 )
    {
        // get the position an bounds of the group
        int minx = getWidth(), maxx = 0, miny = getHeight(), maxy = 0;
        for( auto it = selected_items.begin(); it != selected_items.end(); it++ )
        {
            Rectangle<int> compBounds = (*it)->getBounds();
            minx =  min( minx, compBounds.getX() );
            miny =  min( miny, compBounds.getY() );
            maxx =  max( maxx, compBounds.getRight() );
            maxy =  max( maxy, compBounds.getBottom() );
        }
        
        // create a list from selected items
        vector< BaseComponent *> items;
        for( BaseComponent *c : selected_items ) { items.push_back(c); }
        selected_items.deselectAll();
        
        // create a symbol with these bounds
        Symbol s ("group", minx, miny, maxx-minx, maxy-miny);
        s.addOSCMessage( "/numsymbols", 0 );
        SymbolGroupComponent *group = (SymbolGroupComponent*) SymbolistMainComponent::makeComponentFromSymbol( &s );
        
        
        Rectangle<int> groupBounds( minx, miny, maxx-minx, maxy-miny );
        
        for ( auto it = items.begin(); it != items.end(); it++ )
        {
            BaseComponent *c = *it ;
            
            // sets the position now relative to the group
            Rectangle<int> compBounds = c->getBounds();
            
            c->setBounds(compBounds.getX() - groupBounds.getX(),
                         compBounds.getY() - groupBounds.getY(),
                         compBounds.getWidth(), compBounds.getHeight());
            
            this->removeSymbolComponent( c );
            group->addSymbolComponent( c );
        }
        // will add the symbol to the score if this is a PageComponent
        this->addSymbolComponent( group );
        group->selectComponent();
    }
}


/***************************/
/* UI callbacks from Juce  */
/***************************/

BaseComponent* ScoreComponent::addSymbolAt ( Point<float> p )
{
    Symbol* symbol_template = ((SymbolistMainComponent*) getMainComponent())->getCurrentSymbol();
    
    // sets position in symbol before creation
    // will need to make offset for center based symbols (circle, square, etc.)
    symbol_template->setPosition( p );
    
    // create a new component from the current selected symbol of the palette
    BaseComponent *c = SymbolistMainComponent::makeComponentFromSymbol( symbol_template );
    
    // add component in the view
    addSymbolComponent( c );
    // deselect other itams and select this one
    selected_items.deselectAll();
    selected_items.addToSelection( c );
    
    return c;
}

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    UI_EditType ed = getMainEditMode();
    
    BaseComponent *c = (BaseComponent *) event.eventComponent;
    //    SymbolistMainComponent* smc = (SymbolistMainComponent *) getMainComponent();
    
    if( ed == select_mode )
    {
        if (event.eventComponent != this ) // we're on a symbol
        {
            selected_items.addToSelectionBasedOnModifiers( c, event.mods );
        }
        else
        {   // we're on the score
            lassoSelector.beginLasso( event, this );
        }
    }
    else
    { // => draw mode
        
        if( ed == draw_mode && !component_grabbing_mouse )
        {
            addSymbolAt( event.position ); // positionshould be in the score referential : pb in clicked on top of another symbol
        }
    }
}


void ScoreComponent::mouseDrag ( const MouseEvent& event )
{
    if( getMainEditMode() == select_mode )
    {
        lassoSelector.dragLasso(event);
    }
}

void ScoreComponent::mouseMove ( const MouseEvent& event )
{}

void ScoreComponent::mouseUp ( const MouseEvent& event )
{
    lassoSelector.endLasso();
}


void ScoreComponent::resized () {}



/************************/
/* Selection / "Lasso"  */
/************************/

void ScoreSelectedItemSet::itemSelected (BaseComponent *c) { c->selectComponent(); }
void ScoreSelectedItemSet::itemDeselected (BaseComponent *c) { c->deselectComponent(); }


void ScoreComponent::findLassoItemsInArea (Array <BaseComponent*>& results, const Rectangle<int>& area)
{
    for (int i = 0; i < getNumChildComponents(); ++i)
    {
        Component *cc = getChildComponent(i);
        
        if( &lassoSelector != (LassoComponent< BaseComponent * > *)cc )
        {
            BaseComponent *c = (BaseComponent *)cc;
            
            // this needs to change to look for intersection with path
            if (c->getBounds().intersects (area))
            {
                results.add (c);
            }
        }
    }
}

void ScoreComponent::addItemToSelection(BaseComponent *c)
{
    selected_items.addToSelection(c);
}

SelectedItemSet<BaseComponent*> & ScoreComponent::getLassoSelection()
{
    return selected_items;
}


void ScoreComponent::translateSelected( Point<int> delta_xy )
{
    for ( auto c : selected_items )
    {
        auto b = c->getBounds();
        
        c->setTopLeftPosition( b.getPosition() + delta_xy );
    }
}

void ScoreComponent::deselectAllSelected()
{
    selected_items.deselectAll();
    repaint();
}

void ScoreComponent::notifyEditModeChanged( UI_EditType current_mode )
{
    for( auto s : selected_items )
    {
        s->notifyEditModeChanged( current_mode );
    }
}
