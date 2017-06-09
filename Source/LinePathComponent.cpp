
#include "LinePathComponent.h"

void LinePathComponent::mouseDrag( const MouseEvent& event )
{
    PathBaseComponent::mouseDrag(event);
    
    UI_EditType edit_mode = getMainEditMode();
    if(  edit_mode == draw )
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
    
}