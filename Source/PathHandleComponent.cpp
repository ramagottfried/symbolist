
#include "PathHandleComponent.h"
#include "PathBaseComponent.h"
#include "MainComponent.h"


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
}

void PathHandle::mouseDrag( const MouseEvent& event )
{
    Point<int> draggy = event.getEventRelativeTo( m_path ).getPosition();
    setTopLeftPosition ( draggy - m_down.toInt() );
    
    auto parent_path = static_cast<PathBaseComponent*>( m_path );
    
    if( h_type == rotate )
    {
        auto bounds = parent_path->getPathBounds();
/*
        float half_w = bounds.getWidth() * 0.5;
        float half_h = bounds.getHeight() * 0.5;
        
        auto length = sqrt( half_w * half_w + half_h * half_h ) + 20 ;
*/
        auto delta = parent_path->getCentroid() - getBounds().getCentre().toFloat();
        auto dx = delta.getX(), dy = delta.getY();
        
        auto theta = atan2(dy, dx) - float_Pi;
        
        if( m_prev_theta == -111 ) m_prev_theta = theta;
        
        auto delta_rad = theta - m_prev_theta;
        m_prev_theta = theta;
        
//        setCentrePosition( bounds.getX() + cos(theta) * length, bounds.getY() + sin(theta) * length);
        parent_path->rotatePath( delta_rad  );

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