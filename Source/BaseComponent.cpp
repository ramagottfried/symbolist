
#include "BaseComponent.h"


BaseComponent::BaseComponent() {}

BaseComponent::BaseComponent( Point<float> startPt )
{
    m_down = startPt;
    setBounds( startPt.getX(), startPt.getY(), 10, 10 );
    
//    setCentrePosition( centerPt.toInt() );
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
    is_being_edited = false;
    repaint();
}

void BaseComponent::moved ()
{
    symbol_moved();
}

void BaseComponent::resized ()
{
    symbol_resized();

    if( !resizableBorder )
    {
        addChildComponent( resizableBorder = new ResizableBorderComponent(this, nullptr) );
        resizableBorder->setBorderThickness( BorderSize<int>(1) );
    }
    
    printRect( getLocalBounds(), "resizableBorder" );
    
    resizableBorder->setBounds( getLocalBounds() );

}

void BaseComponent::paint ( Graphics& g )
{
    current_color = is_selected ? sel_color : sym_color;
    
    symbol_paint( g );
    
}


void BaseComponent::mouseEnter( const MouseEvent& event )
{
    symbol_mouseEnter(event);
}

void BaseComponent::mouseMove( const MouseEvent& event )
{
    resizableBorder->setVisible( is_selected && !is_being_edited && event.mods.isAltDown() );

    symbol_mouseMove(event);
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;

    symbol_mouseDown(event);
    
    if( is_selected )
    {
        is_being_edited = true;
        repaint();
    }

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
    resizableBorder->setVisible( is_selected && !is_being_edited && event.mods.isAltDown() );

    symbol_mouseExit(event);
}

void BaseComponent::mouseUp( const MouseEvent& event )
{
    symbol_mouseUp(event);
}


void BaseComponent::mouseDoubleClick( const MouseEvent& event )
{
    symbol_mouseDoubleClick(event);
}
