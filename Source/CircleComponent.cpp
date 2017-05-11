
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
    
    if ( event.mods.isAltDown() )
    {
        
        // figure out how to deal with negtive width/height (this means that the x/y needs to be changed?)
        
        setBounds ( m_bounds.getX(),
                    m_bounds.getY(),
                    m_bounds.getWidth() + event.position.getX() - m_down.getX(),
                    m_bounds.getHeight() + event.position.getY() - m_down.getY() );
    }
    else
    {

        setBounds ( scoreEvent.getPosition().getX() - m_down.getX(),
                    scoreEvent.getPosition().getY() - m_down.getY(),
                    m_bounds.getWidth(),
                    m_bounds.getHeight() );
    }
    
}

void CircleComponent::mouseExit( const MouseEvent& event )
{
    m_color = Colours::black;
    repaint();
}
