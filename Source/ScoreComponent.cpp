#include "ScoreComponent.h"


ScoreComponent::ScoreComponent()
{}

ScoreComponent::~ScoreComponent()
{
    for ( int i = 0; i < score_stack.size(); i++ )
    {
        delete score_stack[i];
    }
}

void ScoreComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
    
    g.setFont (Font (16.0f));
    g.setColour (Colours::grey);
    g.drawText ("shift to make new circle | alt to resize", getLocalBounds(), Justification::centred, true);
}

void ScoreComponent::resized ()
{
}

void ScoreComponent::mouseMove ( const MouseEvent& event )
{
    /*
     std::cout << event.eventComponent->getComponentID() << "\n" ;
     
     MouseEvent scoreEvent = event.getEventRelativeTo(this);
     
     printf ( "score move at %f %f\n", scoreEvent.position.getX(), scoreEvent.position.getY() );
     */
}

void ScoreComponent::addScoreChildComponent( BaseComponent *c )
{
    c->attachScore ( this );
    addAndMakeVisible ( c );
    c->addMouseListener(this, false);
    
    // add to score here
}

void ScoreComponent::findLassoItemsInArea (Array <BaseComponent*>& results, const Rectangle<int>& area)
{
    
    for (int i = 0; i < getNumChildComponents(); ++i)
    {
        BaseComponent *c = (BaseComponent *)getChildComponent (i);
        
        if (c->getBounds().intersects (area))
            results.add (c);
    }
}

SelectedItemSet<BaseComponent*> & ScoreComponent::getLassoSelection()
{
    /// todo need to be completed
    
    printf("num selected %i\n", selected_items.getNumSelected() );
    
    return selected_items;
}

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    
    if( event.eventComponent != this )
    {
        ((BaseComponent *)event.eventComponent)->select();
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
            addChildComponent( lassoSelector );
            
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


