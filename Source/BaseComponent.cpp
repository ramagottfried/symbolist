
#include "BaseComponent.h"

BaseComponent::BaseComponent()
{}

BaseComponent::~BaseComponent(){}


void BaseComponent::select()
{
    printf("selected\n" );

    is_selected = true;
    
    /*
    if( !resizableBorder )
    {
        addChildComponent( resizableBorder = new ResizableBorderComponent(this, nullptr) );
        resizableBorder->setBounds( getLocalBounds() );
        resizableBorder->setBorderThickness( BorderSize<int>(1) );
    }
    
    resizableBorder->setVisible(1);
     */
}


void BaseComponent::deselect()
{
    is_selected = false;
    resizableBorder->setVisible(0);
}


void BaseComponent::moved ()
{
    symbol_moved();
}

void BaseComponent::resized ()
{
    symbol_resized();
    
    if( resizableBorder )
        resizableBorder->setBounds( getLocalBounds() );

}

void BaseComponent::paint ( Graphics& g )
{
    if( showBoundingBox )      // setup control points for resize...
    {
        g.setColour( bb_color );
        const Rectangle<float> l_bounds = getLocalBounds().toFloat().reduced( bb_strokeWeight-1 );
        g.drawRect ( 0.0, 0.0, l_bounds.getWidth(), l_bounds.getHeight()  );
    }
    
    symbol_paint( g );
    
}

void BaseComponent::mouseEnter( const MouseEvent& event )
{
    showBoundingBox = true;
    
    current_color = sel_color;
    
    symbol_mouseEnter(event);
    
    repaint();
}

void BaseComponent::mouseMove( const MouseEvent& event )
{
  //    m_pos = getPosition().toFloat();
    
    symbol_mouseMove(event);

}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
    bounds = getBounds();
    
    std::cout << bounds.getX() << " " << bounds.getY() << " " << bounds.getWidth() << " " << bounds.getHeight() << std::endl;
    
    symbol_mouseDown(event);
    
    // do selection here
}

void BaseComponent::mouseDrag( const MouseEvent& event )
{
    
    Component *score = this->getScore();
    MouseEvent scoreEvent = event.getEventRelativeTo ( score );
    
    Point<float> mouseoffset = scoreEvent.position - m_down;
    /*
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
     */
    
    if( is_selected )
    {
        setBounds ( mouseoffset.getX(),
                   mouseoffset.getY(),
                   bounds.getWidth(),
                   bounds.getHeight() );
    }

    
    symbol_mouseDrag(event);

}

void BaseComponent::mouseExit( const MouseEvent& event )
{    
    showBoundingBox = false;
    
    current_color = sym_color;
    
    symbol_mouseExit(event);
    
    repaint();
    
}

void BaseComponent::mouseDoubleClick( const MouseEvent& event )
{
    symbol_mouseDoubleClick(event);
}