
#include "SymbolComponent.h"

SymbolComponent::SymbolComponent()
{
    addChildComponent( resizableBorder = new ResizableBorderComponent(this, nullptr) );
    resizableBorder->setBounds( getLocalBounds() );
    resizableBorder->setBorderThickness( BorderSize<int>(1) );
    
}

SymbolComponent::~SymbolComponent() {}

void SymbolComponent::select()
{
    is_selected = true;
}


void SymbolComponent::deselect()
{
    is_selected = false;
}


void SymbolComponent::moved ()
{
    symbol_moved();
}

void SymbolComponent::resized ()
{
    symbol_resized();
    
    if( resizableBorder )
        resizableBorder->setBounds( getLocalBounds() );
    
}

void SymbolComponent::paint ( Graphics& g )
{
    resizableBorder->setVisible(is_selected);
    current_color = is_selected ? sel_color : sym_color;
    
    symbol_paint( g );
        
}

void SymbolComponent::mouseEnter( const MouseEvent& event )
{
    symbol_mouseEnter(event);
}

void SymbolComponent::mouseMove( const MouseEvent& event )
{
    symbol_mouseMove(event);
}

void SymbolComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
    bounds = getBounds();
    
    symbol_mouseDown(event);
}

void SymbolComponent::mouseDrag( const MouseEvent& event )
{
    
    Component *score = this->getScoreView();
    MouseEvent scoreEvent = event.getEventRelativeTo ( score );
    
    Point<float> mouseoffset = scoreEvent.position - m_down;
    
    if( is_selected )
    {
        setBounds ( mouseoffset.getX(),
                   mouseoffset.getY(),
                   bounds.getWidth(),
                   bounds.getHeight() );
    }
    
    
    symbol_mouseDrag(event);
    
}

void SymbolComponent::mouseExit( const MouseEvent& event )
{
    symbol_mouseExit(event);
    
}

void SymbolComponent::mouseDoubleClick( const MouseEvent& event )
{
    symbol_mouseDoubleClick(event);
}
