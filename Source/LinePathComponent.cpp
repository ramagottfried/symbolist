
#include "LinePathComponent.h"
#include "PageComponent.h"

LinePathComponent::LinePathComponent(const Symbol &s) : PathBaseComponent( s )
{}


/*************
 *  SELECT, AND MODE SETTING FOR MOUSE LISTENING
 ************/

void LinePathComponent::componentCretated()
{
    m_path.clear();
    setEditMode(true);
}


/*************
 *  MOUSE UI
 ************/


Point<float> LinePathComponent::shiftConstrainMouseAngle( const MouseEvent& event )
{
    if( event.mods.isShiftDown() )
    {
        float angle = event.position.getAngleToPoint( m_down );
        if( fabs(angle) < 0.78539816339745 ) // pi / 4
            return Point<float>( m_down.getX(), event.position.getY() );
        else
            return Point<float>( event.position.getX(), m_down.getY() );
    }
    return event.position;
}


void LinePathComponent::mouseMove( const MouseEvent& event )
{
    PathBaseComponent::mouseMove( event );

    if( in_edit_mode && event.mods.isCommandDown() )
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

    if( in_edit_mode && event.mods.isCommandDown() && event.getDistanceFromDragStart() > 10 )
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

    updatePathFromPreview();
}

