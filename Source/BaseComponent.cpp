
#include "BaseComponent.h"
#include "ScoreComponent.h"


BaseComponent::BaseComponent(String s)
{
    symbol_type = s;
}

BaseComponent::BaseComponent() : BaseComponent("symbol") {}

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
    {
        resizableBorder->setBounds( getLocalBounds() );
    }
    else
    {
        addChildComponent( resizableBorder = new ResizableBorderComponent(this, nullptr) );
        resizableBorder->setBounds( getLocalBounds() );
        resizableBorder->setBorderThickness( BorderSize<int>(1) );
    }
}

void BaseComponent::symbol_moved ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getTPLScoreComponent() );
    // sc can be null if the symbol is moved when not yet on screen
    // best would be to call this from the moving action
    if (sc != NULL) { sc->scoreSymbolModified( this ); }
}

void BaseComponent::symbol_resized ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getTPLScoreComponent() );
    if (sc != NULL) { sc->scoreSymbolModified( this ); }
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
