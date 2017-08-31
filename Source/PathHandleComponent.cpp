
#include "PathHandleComponent.h"
#include "PathBaseComponent.h"
#include "SymbolistMainComponent.h"


PathHandle::PathHandle( handleType type, float x, float y)
{
    setComponentID("path_handle");
    h_type = type;
    float halfsize = m_size * 0.5;
    setBounds( x-halfsize, y-halfsize, m_size, m_size);
}

Point<float> PathHandle::getCenter () const
{
    return Point<float>( getX() + m_size*0.5, getY() + m_size*0.5);
}

void PathHandle::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
    m_prev_theta = -111;
    
    PathBaseComponent* parent = (PathBaseComponent*)getParentComponent();
    m_anchor_bounds = parent->getPathBounds();
    
    mouseDownSelection(event);
}


void PathHandle::mouseDrag( const MouseEvent& event )
{
    PathBaseComponent* parent = (PathBaseComponent*)getParentComponent();

    Point<int> delta_xy = (event.position - m_down).toInt() ;
    parent->translateSelectedComponents(delta_xy);
    
    
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
        
        parent->rotatePath( delta_rad, centre.getX(), centre.getY()  );

    }
    else
    {
        parent->updatePathPoints();
        parent->updatePathBounds();
        parent->repaint();
    }
}


void PathHandle::mouseDoubleClick(const MouseEvent& event)
{
    if ( end )
    {
        closing = !closing;
        
        PathBaseComponent* parent = (PathBaseComponent*)getParentComponent();
        parent->updatePathPoints();
        parent->repaint();
    }
}


/******************
 * Paint
 *****************/

void PathHandle::paint ( Graphics& g )
{
    g.setColour ( is_selected ? Colours::darkred : Colours::cornflowerblue );
    const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( m_strokeweight );
    
    if( h_type == anchor || h_type == start )
    {
        g.drawRect ( bounds, (float) m_strokeweight );
        if ( end ) g.drawRect ( bounds.reduced(1.5), (float) m_strokeweight );
    }
    else if ( h_type == quadratic_control || h_type == cubic_control )
        g.fillRect( bounds.reduced( m_strokeweight ) );
    
    else if ( h_type == rotate )
        g.drawEllipse ( bounds, (float) m_strokeweight );
}
