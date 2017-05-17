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

void ScoreComponent::addChildComponent( BaseComponent *c )
{
    c->attachScore ( this );
    addAndMakeVisible ( c );
    c->addMouseListener(this, false);
    
    // add to score here
}

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    
    if ( event.mods.isShiftDown() )
    {
        CircleComponent *circle = new CircleComponent( event.position.getX(), event.position.getY() );
        
        addChildComponent( circle );
        
        score_stack.emplace_back ( circle );
    }
   
    
}

void ScoreComponent::mouseDrag ( const MouseEvent& event )
{
}

