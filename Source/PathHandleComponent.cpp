
#include "PathHandleComponent.h"
#include "PathBaseComponent.h"
#include "SymbolistMainComponent.h"


PathHandle::PathHandle( handleType type, float x, float y, Component *pc)
{
    setComponentID("path_handle");
    
    m_path = pc;
    h_type = type;
    
    float halfsize = m_size * 0.5;
    setBounds( x-halfsize, y-halfsize, m_size, m_size);
}

void PathHandle::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
    m_prev_theta = -111;
    auto parent_path = static_cast<PathBaseComponent*>( m_path );
    m_anchor_bounds = parent_path->getPathBounds();

}

void PathHandle::mouseDrag( const MouseEvent& event )
{
    Point<int> draggy = event.getEventRelativeTo( m_path ).getPosition();
    setTopLeftPosition ( draggy - m_down.toInt() );
    
    auto parent_path = static_cast<PathBaseComponent*>( m_path );
    
    if( h_type == rotate )
    {
        auto centre = m_anchor_bounds.getCentre();

        auto delta = centre - getBounds().getCentre().toFloat();
        auto dx = delta.getX(), dy = delta.getY();

        auto dist = max( m_anchor_bounds.getHeight(), m_anchor_bounds.getWidth() ) * 0.5 + 5;
        auto theta = atan2(dy, dx) - float_Pi;
        
        setCentrePosition( centre.getX() + cos(theta) * dist, centre.getY() + sin(theta) * dist );
        
        if( m_prev_theta == -111 ) m_prev_theta = theta;
        
        auto delta_rad = theta - m_prev_theta;
        m_prev_theta = theta;
        
        parent_path->rotatePath( delta_rad, centre.getX(), centre.getY()  );

    }
    else
    {
        parent_path->updatePathPoints();
        parent_path->repaint();
    }
    
}

void PathHandle::paint ( Graphics& g )
{
    g.setColour ( Colours::cornflowerblue );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( m_strokeweight );
    
    if( h_type == anchor )
        g.drawRect ( bounds, (float) m_strokeweight );
    else if ( h_type == curve_control )
        g.fillRect( bounds.reduced( m_strokeweight ) );
    else if ( h_type == rotate )
        g.drawEllipse ( bounds, (float) m_strokeweight );
    
}
