#include "ScoreComponent.h"
#include "MainComponent.h"

ScoreComponent::ScoreComponent()
{
    setComponentID("ScoreComponent");
    addChildComponent( lassoSelector );
    lassoSelector.setComponentID("lasso");
    getLookAndFeel().setColour( lassoSelector.lassoFillColourId, Colours::transparentWhite );
    getLookAndFeel().setColour( lassoSelector.lassoOutlineColourId, Colour::fromFloatRGBA(0, 0, 0, 0.2) );
}

ScoreComponent::~ScoreComponent()
{
    selected_items.deselectAll(); //<< required to avoid callback after deleting components
    clearAllSymbolComponents();
}


/***************************************************/
/* MODIFICATIONS TO BE TRANSFERRED TO THE SCORE    */
/* will update data and notify to host environment */
/***************************************************/

void ScoreComponent::addSymbolToScore ( BaseComponent* c )
{
    static_cast<SymbolistMainComponent*>(getParentComponent())->handleComponentAdded( c );
}

void ScoreComponent::removeSymbolFromScore ( BaseComponent* c )
{
    static_cast<SymbolistMainComponent*>(getParentComponent())->handleComponentRemoved( c );
}

void ScoreComponent::modifySymbolInScore( BaseComponent* c )
{
    auto smc = getParentComponent();
    if ( smc != NULL ) static_cast<SymbolistMainComponent*>(smc)->handleComponentModified( c );
}


/***************************************************/
/***************************************************/


/**************************/
/* Add/remove operations  */
/**************************/

/* modifies the view (not the score) */
void ScoreComponent::addChildToScoreComponent( BaseComponent *c )
{
    addAndMakeVisible ( c );
    c->addMouseListener(this, false);
    score_stack.emplace_back ( c );
    // selected_items.addToSelection( c );
    // selected_items.addChangeListener(c);
}

/* modifies the view (not the score) */
void ScoreComponent::removeChildFromScoreComponent( BaseComponent *c )
{
    removeChildComponent(c);
    score_stack.erase ( std::remove(score_stack.begin(),score_stack.end(), c) ,
                        score_stack.end() );
    delete c;
}

/* modifies the view (not the score) */
void ScoreComponent::clearAllSymbolComponents()
{
    for ( int i = 0; i < score_stack.size(); i++ )
    {
        removeChildComponent(score_stack[i]);
        delete score_stack[i];
    }
    score_stack.clear();
}




/* modifies the view AND the score */
void ScoreComponent::userAddSymbolAt ( Point<float> p )
{
    // would be much simpler with a proper copy-contructor..
    // or in Lisp :(
    
    const Symbol* symbol_template = ((SymbolistMainComponent*) getMainComponent())->getCurrentSymbol();
    
    // create a new component from the cureent selected symbol of the thesis
    //BaseComponent *obj = new CircleComponent( 0, 0, 40, 40 );
    BaseComponent *obj = SymbolistMainComponent::makeComponentFromSymbol( new Symbol( *symbol_template ));
    //set the symbol at the click position
    obj->setCentrePosition( p.getX(), p.getY() );
    
    // add component in the view
    addChildToScoreComponent( obj );
    // add the created component's symbol in the score
    addSymbolToScore( obj );
    
    draw_mode = true;
    obj->setEditState( true );
    addMouseListener(obj, false);
}


/* modifies the view AND the score */
void ScoreComponent::deleteSelectedSymbolComponents()
{
    vector< BaseComponent *> items;
    
    for( BaseComponent *c : selected_items ) // there's probably a better way to copy a vector's contents :)
    {
        items.push_back(c);
    }
    
    selected_items.deselectAll();
    
    for( BaseComponent *c : items )
    {
        removeChildFromScoreComponent(c);
        removeSymbolFromScore(c);
    }
}



/************************/
/* Grouping             */
/************************/

void ScoreComponent::groupSymbols()
{
    printf("grouping\n");
    
    if ( selected_items.getNumSelected() > 1 )
    {

        int minx = getWidth(), maxx = 0, miny = getHeight(), maxy = 0;
        
        for( auto it = selected_items.begin(); it != selected_items.end(); it++ )
        {
            Rectangle<int> compBounds = (*it)->getBounds();
//            printf("%i %i %i %i\n", compBounds.getX(), compBounds.getY(), compBounds.getRight(), compBounds.getBottom() );
            minx =  min( minx, compBounds.getX() );
            miny =  min( miny, compBounds.getY() );
            maxx =  max( maxx, compBounds.getRight() );
            maxy =  max( maxy, compBounds.getBottom() );
        }

        Rectangle<int> groupBounds( minx, miny, maxx-minx, maxy-miny );

        BaseComponent *group = new BaseComponent( "group", Point<float>(minx, miny) );
        group->setComponentID("group");
        
        group->setBounds( groupBounds );
        
        addChildToScoreComponent( group );


        for( auto it = selected_items.begin(); it != selected_items.end(); it++ )
        {
            std::cout << (*it)->getComponentID() << "\n";

            Rectangle<int> compBounds = (*it)->getBounds();

            group->addAndMakeVisible( *it );
            group->addSubcomponent( *it );
            // j: DOES THIS REMOVES THE COMPONENT FROM ITS ORIGINAL CONTAINER ??
            // r: no I don't think so, the memory is still in the score_stack

            (*it)->setBounds(   compBounds.getX() - groupBounds.getX(),
                                compBounds.getY() - groupBounds.getY(),
                                compBounds.getWidth(), compBounds.getHeight() );
            
            removeSymbolFromScore( *it );
        }
        
        addSymbolToScore( group );
        group->selectComponent();
        
    }
    
}


/************************/
/* Selection / "Lasso"  */
/************************/


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


/***************************/
/* UI callbacks from Juce  */
/***************************/

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    UI_EditType ed = getMainEditMode();
    
    BaseComponent *c = (BaseComponent *) event.eventComponent;
    SymbolistMainComponent* smc = (SymbolistMainComponent *) getMainComponent();
    
    if( ed == edit )
    {
        if (event.eventComponent != this ) // we're on a symbol
        {
            if ( smc->shift_down )
            {
                // shift-down performs multiple selection
                if ( selected_items.isSelected( c ) )
                {   // remove if already in
                    selected_items.deselect( c );
                }
                else
                {   // add otherwise
                    selected_items.addToSelection( c );
                }
            } else {
                // no-shitf = single selection
                // selected_items.deselectAll();
                selected_items.addToSelection( c );
            }
        }
        else
        {   // we're on the score
            lassoSelector.beginLasso( event, this );
        }
    }
    else
    { // => draw mode
        userAddSymbolAt( event.position ); // positionshoudl be in the score referential : pb in clicked on top of anothe symbol
    }
}


void ScoreComponent::mouseDrag ( const MouseEvent& event )
{
    if( getMainEditMode() == edit )
    {
        lassoSelector.dragLasso(event);
    }
    
}

void ScoreComponent::mouseUp ( const MouseEvent& event )
{
    
    cout << "mouse up on score" << endl;
    
    //if( !event.mods.isCommandDown()  )
    { // what is this for ?
        if( score_stack.size() > 0 )
        {
            removeMouseListener( score_stack.back() );
            score_stack.back()->setEditState( false );
        }
        draw_mode = false;
    }
    
    lassoSelector.endLasso();
}


void ScoreComponent::resized () {}
void ScoreComponent::mouseMove ( const MouseEvent& event ) {}


/************************/
/* Draws the score page */
/************************/

void ScoreComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
    //    g.setColour( Colours::black );
    //    g.drawRect( getLocalBounds() );
    
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



