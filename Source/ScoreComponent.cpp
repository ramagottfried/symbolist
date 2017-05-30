#include "ScoreComponent.h"
#include "MainComponent.h"

ScoreComponent::ScoreComponent()
{
    setComponentID("ScoreComponent");
    
    addChildComponent( lassoSelector );
    lassoSelector.setComponentID("lasso");
    
    getLookAndFeel().setColour( lassoSelector.lassoFillColourId, Colours::transparentWhite );
    getLookAndFeel().setColour( lassoSelector.lassoOutlineColourId, Colour::fromFloatRGBA(0, 0, 0, 0.2) );
    
    addKeyListener( (KeyListener *)getParentComponent() );
}

ScoreComponent::~ScoreComponent()
{
    selected_items.deselectAll(); //<< required to avoid callback after deleting components
    
    for ( int i = 0; i < score_stack.size(); i++ )
    {
        removeChildComponent(score_stack[i]);
        delete score_stack[i];
    }
}


void ScoreComponent::removeAllSymbolComponents()
{
    for ( int i = 0; i < score_stack.size(); i++ )
    {
        removeChildComponent(score_stack[i]);
        delete score_stack[i];
    }
    score_stack.clear();
}

void ScoreComponent::deleteSelectedSymbolComponents()
{
    for( BaseComponent *c : selected_items )
    {
        removeChildComponent(c);

        auto rem = std::remove(score_stack.begin(),
                               score_stack.end(),
                               c );
        score_stack.erase ( rem, score_stack.end() );
        
        scoreSymbolRemoved( c );
        
        delete c;
        
    }
}



// MODOFICATIONS TO BE TRANSFERRED TO THE SCORE
void ScoreComponent::scoreSymbolAdded ( BaseComponent* c )
{
    // will update data and notify to host environment
    static_cast<SymbolistMainComponent*>(getParentComponent())->handleComponentAdded( c );
}

void ScoreComponent::scoreSymbolRemoved ( BaseComponent* c )
{
    // will update data and notify to host environment
    static_cast<SymbolistMainComponent*>(getParentComponent())->handleComponentRemoved( c );
}

void ScoreComponent::scoreSymbolModified ( BaseComponent* c )
{
    // will update data and notify to host environment
    auto smc = getParentComponent();
    if ( smc != NULL )
    {
        static_cast<SymbolistMainComponent*>(smc)->handleComponentModified( c );

    }
}
    
void ScoreComponent::addScoreChildComponent( BaseComponent *c )
{
    c->attachScoreView ( this );
    addAndMakeVisible ( c );
    c->addMouseListener(this, false);
    
    score_stack.emplace_back ( c );
//    selected_items.addToSelection( c );
    

    // selected_items.addChangeListener(c);
}

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
        
        addScoreChildComponent( group );


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
            
            scoreSymbolRemoved( *it );
        }
        
        scoreSymbolAdded( group );
        group->select();
        
    }
    
}



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
            msg = "select/transform mode";
            break;
        case path:
            msg = "path mode";
            break;
        case circle:
            msg = "circle mode";
            break;
    }
    g.drawText (msg, getLocalBounds(), Justification::bottom, false);
    
}

void ScoreComponent::resized ()
{
}

void ScoreComponent::mouseMove ( const MouseEvent& event )
{
}


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


void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    
    UI_EditType ed = getMainEditMode();
    
    if( ed == edit )
    {

        if (event.eventComponent != this )
        {
            selected_items.addToSelection( (BaseComponent *)event.eventComponent );
        }
        else
        {
            lassoSelector.beginLasso( event, this );
        }
    }
    else if ( ed == circle )
    {
        CircleComponent *obj = new CircleComponent( event.position.getX(), event.position.getY() );
        // add in the view
        addScoreChildComponent( obj );
        // add in the score
        scoreSymbolAdded( obj );
        
        draw_mode = true;
        obj->setEditMode( true );
        addMouseListener(obj, false);
    }
    else if ( ed == path )
    {
        PathComponent *obj = new PathComponent( event.position );
        // add in the view
        addScoreChildComponent( obj );
        // add in the score
        scoreSymbolAdded( obj );
        
        draw_mode = true;
        obj->setEditMode( true );
        addMouseListener(obj, false);
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
//    if( !event.mods.isCommandDown()  )
    {
        if( score_stack.size() > 0 )
        {
            removeMouseListener( score_stack.back() );
            score_stack.back()->setEditMode( false );
        }
        draw_mode = false;
    }
    
    lassoSelector.endLasso();
    
}


