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

void ScoreComponent::addChildComponentOSC( Component *c )
{
    addAndMakeVisible ( c );
    // add to score here
}

void ScoreComponent::mouseDown ( const MouseEvent& event )
{
    
    if ( event.mods.isShiftDown() )
    {
        CircleComponent *circle = new CircleComponent( event.position.getX(), event.position.getY() );
        addChildComponentOSC ( circle );
        circle->addMouseListener(this, false);
        score_stack.emplace_back ( circle );
    }
    else
        m_down = event.position;
    
}

void ScoreComponent::mouseDrag ( const MouseEvent& event )
{
    
    if( event.eventComponent != this )
    {
        MouseEvent scoreEvent = event.getEventRelativeTo(this);
        
        Rectangle<int> bounds = event.eventComponent->getBounds();
        
        event.eventComponent->setBounds (   scoreEvent.getPosition().getX() - m_down.getX(),
                                            scoreEvent.getPosition().getY() - m_down.getY(),
                                            bounds.getWidth(),
                                            bounds.getHeight() );
        
    }
    
}