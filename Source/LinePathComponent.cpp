
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

void LinePathComponent::newPathDrawing ()
{
    m_path.clear();
    
    auto pc = static_cast<PageComponent*>( getPageComponent() );
    pc->stealMouse();
    pc->addMouseListener(this, false);
    printf("********************************* added in %s\n", __func__);
}


void LinePathComponent::endPathDrawing ()
{
    
    // if path ended before mouse down, clear preview
    // if no path was created, delete this symbol & component
    
    if( !m_preview_path.isEmpty() )
    {
        m_preview_path.clear();
        
        auto pc = static_cast<PageComponent*>( getPageComponent() );
        pc->removeMouseListener(this);
        pc->giveBackMouse();
        
        if( !m_path.isEmpty() )
        {
//            reset bounds here
        }
        else
        {
            delete this;
        }
    }
}

void LinePathComponent::selectComponent ()
{
    PathBaseComponent::selectComponent();
    
    if ( getMainEditMode() == draw_mode ) //<< this only happens when the path is frist created
    {
        newPathDrawing ();
    }
}

void LinePathComponent::deselectComponent ()
{
    PathBaseComponent::deselectComponent();
    
    // might need to remove and give back mouse here? if deselect/cancel becomes a keyboard shortcut (esc)
}

void LinePathComponent::notifyEditModeChanged( UI_EditType current_mode )
{
    if( is_selected )
    {
        auto pc = static_cast<PageComponent*>( getPageComponent() );
        
        if( getMainEditMode() == draw_mode )
        {
            // in this case the m_path probably already exists, so we don't want to clear it
            pc->stealMouse();
            pc->addMouseListener(this, false);
            printf("********************************* added in %s\n", __func__ );
        }
        else
        {
            endPathDrawing();
        }
    }
}

/*************
 *  MOUSE UI
 ************/


void LinePathComponent::mouseDrag( const MouseEvent& event )
{

    PathBaseComponent::mouseDrag(event);
     /*
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

// NOT FUNCTIONAL PROGRAMMING, but oh well
Rectangle<float> LinePathComponent::applyTranformAndGetNewBounds( Path& p )
{
    float strokeOffset = strokeType.getStrokeThickness() * 0.5;

    Rectangle<float> testBounds = p.getBounds();
    
    float offsetx = ( testBounds.getX() < 0 ) ? -testBounds.getX() : 0;
    float offsety = ( testBounds.getY() < 0 ) ? -testBounds.getY() : 0;
    
    p.applyTransform( AffineTransform().translated(offsetx + strokeOffset, offsety + strokeOffset) );
    
    return ( p.getBounds() - Point<float>(offsetx, offsety) ).expanded( strokeOffset );
}


void LinePathComponent::mouseMove( const MouseEvent& event )
{
    PathBaseComponent::mouseMove( event );
    
//    printPoint(event.position, event.originalComponent->getComponentID() );
    printPoint(m_down, "m_down");
    printPoint(getPosition(), "current pos");
    
    if(  getMainEditMode() == draw_mode )
    {
        if (event.originalComponent == this) return;
        
        Path p;

        // if m_path : m_path.currentPosition() for last point
        
        if( m_path.isEmpty() )
            p.startNewSubPath( 0, 0 );
        else
            p = m_path;
        
        Point<float> endPt = event.position - ref_point;
        p.cubicTo( endPt * 0.25 , endPt * 0.75, endPt );
        
        Rectangle<float> pathBounds = applyTranformAndGetNewBounds( p );
    
        printRect(pathBounds + ref_point, "pathBounds");
//        printRect( getBounds().toFloat().getUnion( pathBounds + getPosition().toFloat() ), "union");

        m_preview_path.swapWithPath( p );
        
        setBoundsFloatRect( pathBounds + ref_point );
        
    }
}

void LinePathComponent::updatePathFromPreivew()
{
    if( !m_preview_path.isEmpty() )
    {
        m_path.swapWithPath( m_preview_path );
        m_preview_path.clear();
        
        Rectangle<float> pathBounds = applyTranformAndGetNewBounds( m_path );
        setBoundsFloatRect( pathBounds + ref_point );
    }
}

void LinePathComponent::mouseDown(const MouseEvent& event)
{
    PathBaseComponent::mouseDown(event);
    
    ref_point = getPosition().toFloat();
    
    updatePathFromPreivew();
}
