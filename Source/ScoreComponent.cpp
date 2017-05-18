#include "ScoreComponent.h"
#include "MainWindow.h"

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

/*
 
to do: check if score component still  
 
 */
void ScoreComponent::groupSymbols()
{
    printf("grouping\n");
    
    if ( selected_items.getNumSelected() > 1 )
    {
        BaseComponent *group = new BaseComponent;

        
        for( auto it = selected_items.begin(); it != selected_items.end(); it++ )
        {
            
            group->addAndMakeVisible( *it );
            
            std::cout << (*it)->getComponentID() << "\n";
            
            //   removeChildComponent( *it );

        }
        
        addScoreChildComponent( group );
        score_stack.emplace_back ( group );
        

        
    }
    
}

void ScoreComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
    g.setColour( Colours::black );
    g.drawRect( getLocalBounds() );
    
    g.setFont (Font (16.0f));
    g.setColour (Colours::grey);
    g.drawText ("shift to make new circle", getLocalBounds(), Justification::centred, true);
}

void ScoreComponent::resized ()
{
}

void ScoreComponent::mouseMove ( const MouseEvent& event )
{
}

void ScoreComponent::addScoreChildComponent( BaseComponent *c )
{
    c->attachScoreView ( this );
    addAndMakeVisible ( c );
    c->addMouseListener(this, false);

    // add to score here
    
    // notify to host environment
    SymbolistMainWindow *w = static_cast<SymbolistMainWindow*>( getTopLevelComponent() );
    w->notifyUpdate();
}

void ScoreComponent::findLassoItemsInArea (Array <BaseComponent*>& results, const Rectangle<int>& area)
{
    for (int i = 0; i < getNumChildComponents(); ++i)
    {
        Component *cc = getChildComponent(i);
        
        if( &lassoSelector != (LassoComponent< BaseComponent * > *)cc )
        {
            BaseComponent *c = (BaseComponent *)cc;
            if (c->getBounds().intersects (area))
                results.add (c);
        }
    }
}

SelectedItemSet<BaseComponent*> & ScoreComponent::getLassoSelection()
{
    return selected_items;
}


void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    
    if( event.eventComponent != this )
    {
        selected_items.addToSelection( (BaseComponent *)event.eventComponent );
    }
    else
    {
        
        if ( event.mods.isShiftDown() )
        {
            CircleComponent *circle = new CircleComponent( event.position.getX(), event.position.getY() );
            
            addScoreChildComponent( circle );
            
            score_stack.emplace_back ( circle );
            
        }
        else
        {
            lassoSelector.beginLasso( event, this );
        }
    }
    
}

void ScoreComponent::mouseDrag ( const MouseEvent& event )
{
    lassoSelector.dragLasso(event);
}

void ScoreComponent::mouseUp ( const MouseEvent& event )
{
    lassoSelector.endLasso();
}


