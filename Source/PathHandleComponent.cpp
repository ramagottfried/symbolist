
#include "PathHandleComponent.h"
#include "PathBaseComponent.h"
#include "MainComponent.h"


PathHandle::PathHandle( handleType type, float x, float y, Component *pc)
{
    setComponentID("control_handle");
    
    m_path = pc;
    h_type = type;
    
    float halfsize = m_size * 0.5;
    setBounds( x-halfsize, y-halfsize, m_size, m_size);
}

void PathHandle::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
}

void PathHandle::mouseDrag( const MouseEvent& event )
{
    // not sure why I need to make this relative, it was jumping back and forth between being relative to the score and then to the component. is it possibe that it has an extra mouselistener somewhere?
    Point<int> draggy = event.getEventRelativeTo( static_cast<PathBaseComponent*>(m_path)->getPageComponent() ).getPosition();
    setTopLeftPosition ( draggy - (m_down).toInt() );
    static_cast<PathBaseComponent*>(m_path)->updatePathPoints();
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