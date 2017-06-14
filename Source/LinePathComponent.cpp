
#include "LinePathComponent.h"
#include "PageComponent.h"

LinePathComponent::LinePathComponent(const Symbol &s) : PathBaseComponent( s )
{}



/*************
 *  SELECT, AND MODE SETTING FOR MOUSE LISTENING
 ************/

/*  currently need to use the draw_mode selection actaion as the cue that this is a new path
    this is a little kludgy, but AFAICT this is the only time this would happen (selection while in draw_mode)
    better would be to signal that this is a new path in the constructor,
    but we'd need to know that the symbol is in the page not the palette,
    so would need to do the parent->ddChild inside this constructor
*/

void LinePathComponent::componentCretated()
{
    m_path.clear();
    enterPathEdit();
}

void LinePathComponent::selectComponent ()
{
    PathBaseComponent::selectComponent();
}

void LinePathComponent::deselectComponent ()
{
    PathBaseComponent::deselectComponent();
    // might need to remove and give back mouse here? if deselect/cancel becomes a keyboard shortcut (esc)
}


/*************
 *  MOUSE UI
 ************/

void LinePathComponent::mouseMove( const MouseEvent& event )
{
    PathBaseComponent::mouseMove( event );

    if( in_edit_mode && getMainEditMode() == draw_mode )
    {
        Path p;
        
        if( m_path.isEmpty() )
            p.startNewSubPath( m_path_origin );
        else
            p = m_path;
        
        p.lineTo( shiftConstrainMouseAngle( event ) );
        
        m_preview_path.swapWithPath( p );
        
        repaint();
    }
}

void LinePathComponent::mouseDrag( const MouseEvent& event )
{
    PathBaseComponent::mouseDrag( event );

    if( in_edit_mode && getMainEditMode() == draw_mode && event.getDistanceFromDragStart() > 10 )
    {
        Path p;
        
        if( m_path.isEmpty() )
            p.startNewSubPath( m_path_origin );
        else
            p = m_path;
    
        p.quadraticTo( shiftConstrainMouseAngle( event ), m_down );

        m_preview_path.swapWithPath( p );
        
        repaint();
    }
}

void LinePathComponent::mouseDown(const MouseEvent& event)
{
    PathBaseComponent::mouseDown(event);
}

void LinePathComponent::mouseUp(const MouseEvent& event)
{
    PathBaseComponent::mouseUp(event);

    updatePathFromPreivew();
}

