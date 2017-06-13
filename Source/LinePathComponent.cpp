
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
    origin = getPosition().toFloat();
    
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
    
    std::cout << "LinePathComponent::mouseDrag " << std::endl;
    
    Point<float> event_pos;
    if (event.originalComponent == this)
        event_pos = event.getEventRelativeTo( getPageComponent() ).position;
    else
        event_pos = event.position;
    
    if( is_selected && getMainEditMode() == draw_mode && event.getDistanceFromDragStart() > 10 )
    {

        Path p;
        
        if( m_path.isEmpty() )
            p.startNewSubPath( origin - ref_point );
        else
            p = m_path;

    
        p.quadraticTo( event_pos - ref_point, m_down - ref_point );

        Rectangle<float> pathBounds = applyTranformAndGetNewBounds( p );

        m_preview_path.swapWithPath( p );
        
        setBoundsFloatRect( pathBounds + ref_point );
        repaint();
    }
        
}

void LinePathComponent::mouseMove( const MouseEvent& event )
{
    PathBaseComponent::mouseMove( event );
    
    if( is_selected && getMainEditMode() == draw_mode )
    {
        Point<float> event_pos;
        if (event.originalComponent == this)
            event_pos = event.getEventRelativeTo( getParentComponent() ).position;
        else
            event_pos = event.position;
        
        Path p;
        
        if( m_path.isEmpty() )
            p.startNewSubPath( origin - ref_point );
        else
            p = m_path;
        
        p.lineTo( event_pos - ref_point );
        
        Rectangle<float> pathBounds = applyTranformAndGetNewBounds( p );
        m_preview_path.swapWithPath( p );
        
        setBoundsFloatRect( pathBounds + ref_point );
        repaint();

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
        
        std::cout << "updated path" << std::endl;
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

