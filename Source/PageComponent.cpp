//
//  PageComponent.cpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#include "PageComponent.h"
#include "MainComponent.h"
#include "SymbolGroupComponent.h"

PageComponent::PageComponent()
{
    setComponentID("PageComponent");
    addChildComponent( lassoSelector );
    lassoSelector.setComponentID("lasso");
    getLookAndFeel().setColour( lassoSelector.lassoFillColourId, Colours::transparentWhite );
    getLookAndFeel().setColour( lassoSelector.lassoOutlineColourId, Colour::fromFloatRGBA(0, 0, 0, 0.2) );
}

PageComponent::~PageComponent()
{
    selected_items.deselectAll(); //<< required to avoid callback after deleting components
    clearAllSubcomponents();
}


/***************************************************/
/* MODIFICATIONS TO BE TRANSFERRED TO THE SCORE VIA MAINCOMPONENT   */
/* will update data and notify to host environment */
/***************************************************/

void PageComponent::addSymbolToScore ( BaseComponent* c )
{
    static_cast<SymbolistMainComponent*>(getParentComponent())->handleComponentAdded( c );
}

void PageComponent::removeSymbolFromScore ( BaseComponent* c )
{
    static_cast<SymbolistMainComponent*>(getParentComponent())->handleComponentRemoved( c );
}

void PageComponent::modifySymbolInScore( BaseComponent* c )
{
    auto smc = getParentComponent();
    if ( smc != NULL ) static_cast<SymbolistMainComponent*>(smc)->handleComponentModified( c );
}



/**************************/
/* Add/remove operations on Score AND View */
/**************************/


/* modifies the view AND the score */
void PageComponent::addSymbolAt ( Point<float> p )
{
    const Symbol* symbol_template = ((SymbolistMainComponent*) getMainComponent())->getCurrentSymbol();
    
    // create a new component from the cureent selected symbol of the thesis
    //Symbol* s = new Symbol( *symbol_template ) ;
    BaseComponent *obj = SymbolistMainComponent::makeComponentFromSymbol( symbol_template );
    //set the symbol at the click position
    obj->setCentrePosition( p.getX(), p.getY() );
    
    // add component in the view
    addSubcomponent( obj );
    
    // add the created component's symbol in the score
    addSymbolToScore( obj );
    
    draw_mode = true;
    obj->setEditState( true );
    addMouseListener(obj, false);
}


/* modifies the view AND the score */
void PageComponent::deleteSelectedSymbols()
{
    vector< BaseComponent *> items;
    
    for( BaseComponent *c : selected_items ) // there's probably a better way to copy a vector's contents :)
    {
        items.push_back(c);
    }
    
    selected_items.deselectAll();
    
    for( BaseComponent *c : items )
    {
        if( c->getSymbolType() != "UI_only") // UI only elements are bound to a component
        {
            removeSubcomponent(c, true);
            removeSymbolFromScore(c);
        }
    }
}



/************************/
/* Grouping             */
/************************/
/* todo:
 deal with the fact that subComponents have no symbol
 create the compound component's symbol
 */

void PageComponent::groupSymbols()
{
    printf("grouping\n");
    
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
        
        // create a symbol with these bounds
        Symbol* s = new Symbol ("group", minx, miny, maxx-minx, maxy-miny);
        
        Rectangle<int> groupBounds( minx, miny, maxx-minx, maxy-miny );
        SymbolGroupComponent *group = (SymbolGroupComponent*) SymbolistMainComponent::makeComponentFromSymbol( s );
        
        
        // create a list from selected items
        vector< BaseComponent *> items;
        for( BaseComponent *c : selected_items ) { items.push_back(c); }
        selected_items.deselectAll();
        
        for ( auto it = items.begin(); it != items.end(); it++ )
        {
            BaseComponent *c = *it ;
            std::cout << "grouping: " << c->getComponentID() << std::endl;
            
            removeSubcomponent( c , false );
            if ( c->isTopLevelComponent() ) { removeSymbolFromScore( c ); }
            
            // sets the position now relative to the group
            Rectangle<int> compBounds = c->getBounds();
            c->setBounds(compBounds.getX() - groupBounds.getX(),
                         compBounds.getY() - groupBounds.getY(),
                         compBounds.getWidth(), compBounds.getHeight());
            
            group->addSubcomponent( c );
        }
        
        addSubcomponent( group );
        if ( group->isTopLevelComponent() ) { addSymbolToScore( group ); }
        
        group->selectComponent();
        
    }
    
}



/************************/
/* Selection / "Lasso"  */
/************************/


void PageComponent::findLassoItemsInArea (Array <BaseComponent*>& results, const Rectangle<int>& area)
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

void PageComponent::addItemToSelection(BaseComponent *c)
{
    selected_items.addToSelection(c);
}

SelectedItemSet<BaseComponent*> & PageComponent::getLassoSelection()
{
    return selected_items;
}


void PageComponent::translateSelected( Point<int> delta_xy )
{
    for ( auto c : selected_items )
    {
        auto b = c->getBounds();
        
        c->setTopLeftPosition( b.getPosition() + delta_xy );
    }
}

void PageComponent::deselectAllSelected()
{
    selected_items.deselectAll();
    repaint();
}


/***************************/
/* UI callbacks from Juce  */
/***************************/

void PageComponent::mouseDown ( const MouseEvent& event )
{
    UI_EditType ed = getMainEditMode();
    
    BaseComponent *c = (BaseComponent *) event.eventComponent;
//    SymbolistMainComponent* smc = (SymbolistMainComponent *) getMainComponent();
    
    if( ed == edit )
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
        addSymbolAt( event.position ); // positionshoudl be in the score referential : pb in clicked on top of anothe symbol
    }
}


void PageComponent::mouseDrag ( const MouseEvent& event )
{
    if( getMainEditMode() == edit )
    {
        lassoSelector.dragLasso(event);
    }
    
}

void PageComponent::mouseUp ( const MouseEvent& event )
{
    
    cout << "mouse up on score" << endl;
    
    //if( !event.mods.isCommandDown()  )
    { // what is this for ?
        if( subcomponents.size() > 0 )
        {
            removeMouseListener( subcomponents.back() );
            subcomponents.back()->setEditState( false );
        }
        draw_mode = false;
    }
    
    lassoSelector.endLasso();
}


void PageComponent::resized () {}
void PageComponent::mouseMove ( const MouseEvent& event )
{
    if( getMainEditMode() == draw )
    {
        
    }
}


/************************/
/* Draws the score page */
/************************/

void PageComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );

    g.setFont (Font (16.0f));
    g.setColour (Colours::grey);
    
    String msg = "";
    
    switch ( getMainEditMode() )
    {
        case edit:
            msg += " select/transform mode" ;
            break;
        case draw:
            msg += " draw mode: " ;
            Symbol *s = static_cast<SymbolistMainComponent*>( getMainComponent() )->getCurrentSymbol();
            msg += s->getOSCMessageValue(s->getOSCMessagePos("/type")).getString();
            break;
    }
    g.drawText (msg, getLocalBounds() , Justification::bottom, false);
}
