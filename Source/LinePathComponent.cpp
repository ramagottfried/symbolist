
#include "LinePathComponent.h"

LinePathComponent::LinePathComponent(const Symbol &s) : PathBaseComponent( s )
{
}

void LinePathComponent::selectComponent ()
{
    PathBaseComponent::selectComponent();
    getPageComponent()->addMouseListener(this, false);
    
}

void LinePathComponent::deselectComponent ()
{
    PathBaseComponent::deselectComponent();
    getPageComponent()->removeMouseListener(this);
}

void LinePathComponent::mouseDrag( const MouseEvent& event )
{
    /*
    PathBaseComponent::mouseDrag(event);
    
    UI_EditType edit_mode = getMainEditMode();
    if(  edit_mode == draw_mode )
    {
        Path p;
        float strokeOffset = strokeType.getStrokeThickness() * 0.5;
        Point<float> zeroPt(0, 0);
        p.startNewSubPath( zeroPt );
        
        Point<float> endPt = m_drag - m_down + zeroPt;
        p.cubicTo( endPt * 0.25 , endPt * 0.75, endPt );
        
        Rectangle<float> testBounds = p.getBounds();
        float offsetx = ( testBounds.getX() < 0 ) ? -testBounds.getX() : 0;
        float offsety = ( testBounds.getY() < 0 ) ? -testBounds.getY() : 0;
        p.applyTransform( AffineTransform().translated(offsetx + strokeOffset, offsety + strokeOffset) );
        
        Rectangle<float> pathBounds = ( p.getBounds() - Point<float>(offsetx, offsety) ).expanded( strokeOffset );
        
        m_path.swapWithPath( p );
        setBoundsFloatRect( pathBounds + m_down );
        
    }
    */
}

void LinePathComponent::mouseMove( const MouseEvent& event )
{
    PathBaseComponent::mouseMove( event );
//    printPoint( event.position - m_down, "LinePathComponent::mouseMove" );
//    printPoint( getPosition(), "position" );

    printPoint(event.position, event.originalComponent->getComponentID() );
    
    UI_EditType mouse_mode = getMainEditMode();
    if(  mouse_mode == draw_mode )
    {
        Path p;
        float strokeOffset = strokeType.getStrokeThickness() * 0.5;

        // if m_path : m_path.currentPosition() for last point
        p.startNewSubPath( 0, 0 );
        
        Point<float> endPt = event.position - m_down;
        p.cubicTo( endPt * 0.25 , endPt * 0.75, endPt );
        
        Rectangle<float> testBounds = p.getBounds();
        float offsetx = ( testBounds.getX() < 0 ) ? -testBounds.getX() : 0;
        float offsety = ( testBounds.getY() < 0 ) ? -testBounds.getY() : 0;
        p.applyTransform( AffineTransform().translated(offsetx + strokeOffset, offsety + strokeOffset) );
        
        Rectangle<float> pathBounds = ( p.getBounds() - Point<float>(offsetx, offsety) ).expanded( strokeOffset );
        
        m_preview_path.swapWithPath( p );
        setBoundsFloatRect( pathBounds + m_down );
        
    }
}

void LinePathComponent::mouseDown(const MouseEvent& event)
{
    PathBaseComponent::mouseDown(event);

    printPoint( event.position, "LinePathComponent::mouseDown" );
    printPath(m_preview_path);
    
    if( m_path.isEmpty() )
        m_path.swapWithPath( m_preview_path );
    else
        m_path.addPath( m_preview_path );
    
    std::cout << "m_path\n";
    printPath(m_path);

    m_preview_path.clear();
}
