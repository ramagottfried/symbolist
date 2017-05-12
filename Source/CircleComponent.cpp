
#include "CircleComponent.h"

CircleComponent::CircleComponent()
{
    setComponentID ( "Circle" );
}

// add options for other params: color, stroke...
CircleComponent::CircleComponent( float x, float y, float diameter, float stroke )
{

    setComponentID ( "Circle" );
    setBounds ( x, y, diameter, diameter );
    m_diameter = diameter;
    strokeWeight = stroke;
    
}


CircleComponent::~CircleComponent(){}

void CircleComponent::paint ( Graphics& g )
{
    BaseComponent::paint ( g );
    
    g.setColour( m_color );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
    g.drawEllipse ( bounds, (float) strokeWeight );
    
}

void CircleComponent::moved ()
{
    BaseComponent::moved ();
    
    // push to score here?
}

void CircleComponent::mouseDoubleClick (const MouseEvent& event)
{
    printf("2x click");
}

void CircleComponent::mouseEnter( const MouseEvent& event )
{
    BaseComponent::mouseEnter( event );
    
    m_color = sel_color;
    repaint();
}

void CircleComponent::mouseMove( const MouseEvent& event )
{
}

void CircleComponent::mouseDown( const MouseEvent& event )
{
    BaseComponent::mouseDown ( event );
    
       // at the moment mouse down is only for creating new objects
}

void CircleComponent::mouseDrag( const MouseEvent& event )
{
    BaseComponent::mouseDrag ( event );

    
}

void CircleComponent::mouseExit( const MouseEvent& event )
{
    BaseComponent::mouseExit( event );
    
    m_color = Colours::black;
    repaint();
}
