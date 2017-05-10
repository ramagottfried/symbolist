/*
  ==============================================================================

    This file was auto-generated!

  ==============================================================================
*/

#include "MainComponent.h"


//==============================================================================
MainContentComponent::MainContentComponent()
{
    addAndMakeVisible(m_circle);
    addAndMakeVisible(m_line);

    setSize (600, 400);
}

MainContentComponent::~MainContentComponent()
{
}

void MainContentComponent::paint (Graphics& g)
{
    // (Our component is opaque, so we must completely fill the background with a solid colour)
    g.fillAll (getLookAndFeel().findColour (ResizableWindow::backgroundColourId));

    g.setFont (Font (16.0f));
    g.setColour (Colours::white);
    g.drawText ("Hello World!", getLocalBounds(), Justification::centred, true);

}

void MainContentComponent::resized()
{
    // This is called when the MainContentComponent is resized.
    // If you add any child components, this is where you should
    // update their positions.
    
}


void MainContentComponent::mouseEnter ( const MouseEvent& event )
{
    printf ( "entered at %f %f\n", event.position.getX(), event.position.getY() );
    printf ( "same as %i %i\n", event.x, event.y );
}

void MainContentComponent::mouseMove ( const MouseEvent& event )
{
//    printf ( "%s at %f %f\n", __func__, event.position.getX(), event.position.getY() );
//    printf ( "%s same as %i %i\n", __func__, event.x, event.y );
    if( m_down.getX() )
        m_line.set( m_down.getX(), m_down.getY(), event.position.getX(), event.position.getY());
    
    repaint();
}

void MainContentComponent::mouseDown ( const MouseEvent& event )
{
    m_down = event.position;
 //   printf ( "mouse down\n" );

    m_circle.set(event.position.getX()-5, event.position.getY()-5, 10);

}

void MainContentComponent::mouseDrag ( const MouseEvent& event )
{}

void MainContentComponent::mouseUp ( const MouseEvent& event )
{}

void MainContentComponent::mouseExit ( const MouseEvent& event )
{}

