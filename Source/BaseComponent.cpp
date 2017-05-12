
#include "BaseComponent.h"

BaseComponent::BaseComponent(){}
BaseComponent::~BaseComponent(){}

void BaseComponent::moved ()
{}

void BaseComponent::resized ()
{}

void BaseComponent::paint ( Graphics& g )
{
    if( showBoundingBox )
    {
        g.setColour( Colours::red );
        const Rectangle<float> l_bounds = getLocalBounds().toFloat().reduced( strokeWeight );
        g.drawRect ( 0.0, 0.0, l_bounds.getWidth(), l_bounds.getHeight()  );
    }
}

void BaseComponent::mouseEnter( const MouseEvent& event )
{}

void BaseComponent::mouseMove( const MouseEvent& event )
{
  //    m_pos = getPosition().toFloat();
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
    bounds = getBounds();
}

void BaseComponent::mouseDrag( const MouseEvent& event )
{
    Component *score = this->getScore();
    MouseEvent scoreEvent = event.getEventRelativeTo ( score );
    
    Point<float> mouseoffset = scoreEvent.position - m_down;
    
    if ( event.mods.isAltDown() )
    {
        
        float newX = ( scoreEvent.position.getX() < bounds.getX() ) ? scoreEvent.position.getX() : bounds.getX();
        float newY = ( scoreEvent.position.getY() < bounds.getY() ) ? scoreEvent.position.getY() : bounds.getY();
        
        float newW = std::abs( bounds.getWidth()  + mouseoffset.getX() - bounds.getX() );
        float newH = std::abs( bounds.getHeight() + mouseoffset.getY() - bounds.getY() );
        
        newW = (newW < strokeWeight*2) ? strokeWeight*2 : newW;
        newH = (newH < strokeWeight*2) ? strokeWeight*2 : newH;
        
        setBounds ( newX, newY, newW, newH );
    }
    else
    {
        setBounds ( mouseoffset.getX(),
                   mouseoffset.getY(),
                   bounds.getWidth(),
                   bounds.getHeight() );
    }


}

void BaseComponent::mouseExit( const MouseEvent& event )
{}

