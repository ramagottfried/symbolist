

#include "ScoreComponent.h"
#include "SymbolistMainComponent.h"
#include "SymbolGroupComponent.h"


ScoreComponent::~ScoreComponent()
{
    unselectAllComponents();
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
    
    subcomponents.emplace_back( c );
    c->setComponentID(String(String(c->getSymbolTypeStr()) += String("_") += String(subcomponents.size())));
    addAndMakeVisible( c );
    //c->addMouseListener(this, false); // get rid of this ??
    //std::cout << "ADDING SUBCOMP " << c->getComponentID() << " IN " << getComponentID() << std::endl;
}

void ScoreComponent::removeSubcomponent( BaseComponent *c )
{
    removeFromSelection(c);
    removeChildComponent(c);
    subcomponents.erase ( std::remove(subcomponents.begin(),subcomponents.end(), c) , subcomponents.end() );
}

void ScoreComponent::clearAllSubcomponents()
{
    selected_components.clear();
    for ( int i = 0; i < subcomponents.size(); i++ )
    {
        subcomponents[i]->clearAllSubcomponents();
        removeChildComponent( subcomponents[i] );
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
    unselectAllComponents ( );
    removeSubcomponent( c );
}

/**************/
/* Selection  */
/**************/

void ScoreComponent::addToSelection(BaseComponent *c)
{
    selected_components.add(c);
    c->selectComponent();
}

void ScoreComponent::removeFromSelection(BaseComponent *c)
{
    c->deselectComponent();
    selected_components.removeAllInstancesOf(c);
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
        removeFromSelection(getSubcomponent(i));
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
    
    s_lasso.update( position.getX(), position.getY() );

    unselectAllComponents();
    
    for (int i = 0; i < getNumSubcomponents(); ++i)
    {
        BaseComponent* cc =  getSubcomponent(i);
        
        if (cc->getBounds().intersects( s_lasso.getBounds() ))
        {
            addToSelection( cc );
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

void ScoreComponent::deleteSelectedSymbols()
{
    vector< BaseComponent *> items;
    
    for( BaseComponent *c : selected_components ) // there's probably a better way to copy a vector's contents :)
    {
        std::cout << c << std::endl;
        items.push_back(c);
    }
    
    //selected_items.deselectAll();
    unselectAllComponents();
    
    for( BaseComponent *c : items )
    {
        //ScoreComponent* parent = (ScoreComponent*) c->getParentComponent() ; // the selected_items are not necesarily direct children
        //parent->
        removeSymbolComponent( c );
        delete c;
    }
}



void ScoreComponent::groupSelectedSymbols()
{
    if ( selected_components.size() > 1 )
    {
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
        
        // create a list from selected items
        vector< BaseComponent *> items;
        for( BaseComponent *c : selected_components ) { items.push_back(c); }
        
        unselectAllComponents();
        
        Symbol s ("group", minx, miny, maxx-minx, maxy-miny);
        s.addOSCMessage( "/numsymbols", 0 );
        SymbolGroupComponent *group = (SymbolGroupComponent*) SymbolistHandler::makeComponentFromSymbol( &s );
        
        
        Rectangle<int> groupBounds( minx, miny, maxx-minx, maxy-miny );
        
        for ( auto it = items.begin(); it != items.end(); it++ )
        {
            BaseComponent *c = *it ;
            
            // sets the position now relative to the group
            Rectangle<int> compBounds = c->getBounds();
            
            c->setBounds(compBounds.getX() - groupBounds.getX(),
                         compBounds.getY() - groupBounds.getY(),
                         compBounds.getWidth(), compBounds.getHeight());
            
            ((ScoreComponent*)c->getParentComponent())->removeSymbolComponent( c ); // the parent is not necessarily 'this' (selected_items can be indirect children...)
            group->addSymbolComponent( c );
        }
        // will add the symbol to the score if this is a PageComponent
        this->addSymbolComponent( group );
        addToSelection( group );
    }
}


void ScoreComponent::ungroupSelectedSymbols()
{
    vector< BaseComponent *> items;
    for( BaseComponent *c : selected_components ) { items.push_back(c); }
    unselectAllComponents();
    
    for ( int i = 0; i < items.size(); i++ )
    {
        BaseComponent* c = items[i];
        int n = ((int)c->getNumSubcomponents());
        
        vector< BaseComponent *> subitems;
        for ( int ii = 0 ; ii < n ; ii++ ) { subitems.push_back(c->getSubcomponent(ii)); }

        for ( int ii = 0; ii < n ; ii++ )
        {
            BaseComponent* sc = subitems[ii];
            c->removeSymbolComponent(sc);
            this->addSymbolComponent(sc);
            sc->setTopLeftPosition(sc->getPosition().translated(c->getPosition().getX(), c->getPosition().getY()));
        }
        
        removeSymbolComponent(c);
    }
}


void ScoreComponent::translateSelectedSymbols( Point<int> delta_xy )
{
    //std::cout << "TRANSLATE IN " << getComponentID() << std::endl;
    for ( auto c : selected_components )
    {
        auto b = c->getBounds();
        c->setTopLeftPosition( b.getPosition() + delta_xy );
    }
}

void ScoreComponent::flipSelectedSymbols( int axis )
{
    for ( auto c : selected_components )
    {
        if( axis == 0)
            c->v_flip();
        else
            c->h_flip();
    }
}


/***************************/
/* UI callbacks from Juce  */
/***************************/

void ScoreComponent::mouseAddClick ( const MouseEvent& event )
{
    
    BaseComponent *c;
    
    if ( getMainDrawMode() == UI_DrawType::from_template )
    {
        Symbol* symbol_template = getSymbolistHandler()->getCurrentSymbol();
        // sets position in symbol before creation
        // will need to make offset for center based symbols (circle, square, etc.)
        symbol_template->setPosition ( event.position );
        // create a new component from the current selected symbol of the palette
        c = SymbolistHandler::makeComponentFromSymbol( symbol_template );
        // add component in the view
        addSymbolComponent( c );
    }
    else
    {
        Symbol* dummy_symbol = new Symbol("path", event.position.x, event.position.y, 40.0, 40.0) ;
        c = SymbolistHandler::makeComponentFromSymbol( dummy_symbol );
        delete dummy_symbol;
        addSymbolComponent( c );
        getPageComponent()->enterEditMode(c);
        c->mouseAddClick(event.getEventRelativeTo(c));
    }
    
    // deselect other items and select this one
    //unselectAllComponents();
    //addToSelection( c );
    
}

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    UI_EditType ed = getMainEditMode();
    
    if( ed == selection )
    {
        beginLassoSelection( event.getPosition() );
    }
    else if( ed == draw )
    {
        mouseAddClick( event.getEventRelativeTo(getPageComponent()) );
    }
}

void ScoreComponent::mouseDrag ( const MouseEvent& event )
{
    if( getMainEditMode() == selection )
    {
        dragLassoSelection(event.getPosition());
    }
}

void ScoreComponent::mouseUp ( const MouseEvent& event )
{
    endLassoSelection();
}




