
#include "CircleComponent.h"


CircleComponent::CircleComponent()
{
    setComponentID ( "Circle" );
}

CircleComponent::CircleComponent( float x, float y, float diameter )
{
    setComponentID ( "Circle" );
    setBounds ( x, y, diameter, diameter );
    m_diameter = diameter;
    
}

CircleComponent::~CircleComponent(){}


void CircleComponent::paint ( Graphics& g )
{
    g.setColour( m_color );
    
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( m_strokeWeight );
    
    g.drawEllipse ( bounds, (float) m_strokeWeight );
}

void CircleComponent::moved ()
{
    m_pos = getPosition().toFloat();
    // push to score here?
    
    //        std::cout << m_pos.getX() << " " << m_pos.getY() << "\n";
}

void CircleComponent::mouseDoubleClick (const MouseEvent& event)
{
    printf("2x click");
}

void CircleComponent::mouseEnter( const MouseEvent& event )
{
    m_color = Colours::hotpink;
    repaint();
}

void CircleComponent::mouseMove( const MouseEvent& event )
{
}

void CircleComponent::mouseDown( const MouseEvent& event )
{
}

void CircleComponent::mouseDrag( const MouseEvent& event )
{
    if ( event.mods.isAltDown() )
    {
        // resize here
        ;
    }
    
}

void CircleComponent::mouseExit( const MouseEvent& event )
{
    m_color = Colours::black;
    repaint();
}
