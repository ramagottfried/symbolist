
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
    m_down = event.position;
    m_bounds = getBounds();
    // at the moment mouse down is only for creating new objects
}

void CircleComponent::mouseDrag( const MouseEvent& event )
{
    
    /*
     if possible add move and resize operations to BaseComponent
    */
    
    Component *score = this->getScore();
    MouseEvent scoreEvent = event.getEventRelativeTo ( score );
    
    Point<float> mouseoffset = scoreEvent.position - m_down;
    
    if ( event.mods.isAltDown() )
    {
        
        float newX = ( scoreEvent.position.getX() < m_bounds.getX() ) ? scoreEvent.position.getX() : m_bounds.getX();
        float newY = ( scoreEvent.position.getY() < m_bounds.getY() ) ? scoreEvent.position.getY() : m_bounds.getY();
        
        float newW = std::abs( m_bounds.getWidth()  + mouseoffset.getX() - m_bounds.getX() );
        float newH = std::abs( m_bounds.getHeight() + mouseoffset.getY() - m_bounds.getY() );
        
        newW = (newW < m_strokeWeight*2) ? m_strokeWeight*2 : newW;
        newH = (newH < m_strokeWeight*2) ? m_strokeWeight*2 : newH;
        
        setBounds ( newX, newY, newW, newH );
    }
    else
    {
        setBounds ( mouseoffset.getX(),
                    mouseoffset.getY(),
                    m_bounds.getWidth(),
                    m_bounds.getHeight() );
    }
    
}

void CircleComponent::mouseExit( const MouseEvent& event )
{
    m_color = Colours::black;
    repaint();
}
