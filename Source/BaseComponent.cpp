
#include "BaseComponent.h"


BaseComponent::BaseComponent()
{
    addChildComponent( resizableBorder = new ResizableBorderComponent(this, nullptr) );
    resizableBorder->setBounds( getLocalBounds() );
    resizableBorder->setBorderThickness( BorderSize<int>(1) );
}


BaseComponent::~BaseComponent() {}

void BaseComponent::select()
{
    is_selected = true;
    repaint();
}

void BaseComponent::deselect()
{
    is_selected = false;
    repaint();
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
    resizableBorder->setVisible(is_selected);
    
    current_color = is_selected ? sel_color : sym_color;
    
    symbol_paint( g );
    
}

void BaseComponent::mouseEnter( const MouseEvent& event )
{
    symbol_mouseEnter(event);
}

void BaseComponent::mouseMove( const MouseEvent& event )
{
    symbol_mouseMove(event);
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
    
    symbol_mouseDown(event);
}

void BaseComponent::mouseDrag( const MouseEvent& event )
{
    if( is_selected )
    {
        Point<float> mouseoffset = event.getEventRelativeTo( getParentComponent() ).position - m_down;
        setBounds ( mouseoffset.getX(), mouseoffset.getY(), getWidth(), getHeight() );
    }
    
    symbol_mouseDrag(event);
}

void BaseComponent::mouseExit( const MouseEvent& event )
{
    symbol_mouseExit(event);
}

void BaseComponent::mouseDoubleClick( const MouseEvent& event )
{
    symbol_mouseDoubleClick(event);
}
