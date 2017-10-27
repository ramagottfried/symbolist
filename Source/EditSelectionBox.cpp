
#include "EditSelectionBox.h"
#include "SymbolistMainComponent.h"

EditSelectionBox::Zone::Zone() noexcept
: zone (0)
{}

EditSelectionBox::Zone::Zone (const int zoneFlags) noexcept
: zone (zoneFlags)
{}

EditSelectionBox::Zone::Zone (const EditSelectionBox::Zone& other) noexcept
: zone (other.zone)
{}

EditSelectionBox::Zone& EditSelectionBox::Zone::operator= (const EditSelectionBox::Zone& other) noexcept
{
    zone = other.zone;
    return *this;
}

bool EditSelectionBox::Zone::operator== (const EditSelectionBox::Zone& other) const noexcept      { return zone == other.zone; }
bool EditSelectionBox::Zone::operator!= (const EditSelectionBox::Zone& other) const noexcept      { return zone != other.zone; }

EditSelectionBox::Zone EditSelectionBox::Zone::fromPositionOnBorder (const Rectangle<int>& totalSize,
                                                                                     const BorderSize<int>& border,
                                                                                     Point<int> position)
{
    int z = 0;
    
    if (totalSize.contains (position)
        && ! border.subtractedFrom (totalSize).contains (position))
    {
        const int minW = jmax (totalSize.getWidth() / 10, jmin (10, totalSize.getWidth() / 3));
        if (position.x < jmax (border.getLeft(), minW) && border.getLeft() > 0)
            z |= left;
        else if (position.x >= totalSize.getWidth() - jmax (border.getRight(), minW) && border.getRight() > 0)
            z |= right;
        
        const int minH = jmax (totalSize.getHeight() / 10, jmin (10, totalSize.getHeight() / 3));
        if (position.y < jmax (border.getTop(), minH) && border.getTop() > 0)
            z |= top;
        else if (position.y >= totalSize.getHeight() - jmax (border.getBottom(), minH) && border.getBottom() > 0)
            z |= bottom;
    }
    
    return Zone (z);
}

MouseCursor EditSelectionBox::Zone::getMouseCursor() const noexcept
{
    MouseCursor::StandardCursorType mc = MouseCursor::NormalCursor;
    
    switch (zone)
    {
        case (left | top):      mc = MouseCursor::TopLeftCornerResizeCursor; break;
        case top:               mc = MouseCursor::TopEdgeResizeCursor; break;
        case (right | top):     mc = MouseCursor::TopRightCornerResizeCursor; break;
        case left:              mc = MouseCursor::LeftEdgeResizeCursor; break;
        case right:             mc = MouseCursor::RightEdgeResizeCursor; break;
        case (left | bottom):   mc = MouseCursor::BottomLeftCornerResizeCursor; break;
        case bottom:            mc = MouseCursor::BottomEdgeResizeCursor; break;
        case (right | bottom):  mc = MouseCursor::BottomRightCornerResizeCursor; break;
        default:                break;
    }
    
    return mc;
}

//==============================================================================
EditSelectionBox::EditSelectionBox ( Array<SymbolistComponent*>* const selected_component_array ) :
borderSize (5), mouseZone (0)
{
    component_set = selected_component_array;
}

EditSelectionBox::~EditSelectionBox()
{
}

//==============================================================================
void EditSelectionBox::paint (Graphics& g)
{
    auto mc = ((SymbolistComponent*)getParentComponent())->getMainComponent();
    if( mc && mc->getCurrentMods()->isAltDown() )
    {
        //g.drawLine(prev_pos.getX(), prev_pos.getY(), getWidth() / 2, getWidth() / 2);
        getLookAndFeel().drawResizableFrame (g, getWidth(), getHeight(), borderSize);
    }
    else
    {
        getLookAndFeel().drawResizableFrame (g, getWidth(), getHeight(), borderSize);
    }
}

void EditSelectionBox::updateEditSelBox()
{
    if( component_set->size() == 0 )
    {
        setVisible(false);
    }
    else
    {
        setVisible(true);
        setBounds( getSelectionBounds() );
    }
}

void EditSelectionBox::mouseEnter (const MouseEvent& e)
{
    updateMouseZone (e);
}

void EditSelectionBox::mouseMove (const MouseEvent& e)
{
    updateMouseZone (e);
}

Rectangle<int> EditSelectionBox::getSelectionBounds()
{
    // get the position an bounds of the group
    int minx = getParentWidth(), maxx = 0, miny = getParentHeight(), maxy = 0;
    for( auto it = component_set->begin(); it != component_set->end(); it++ )
    {
        Rectangle<int> compBounds = (*it)->getBounds();
        minx =  min( minx, compBounds.getX() );
        miny =  min( miny, compBounds.getY() );
        maxx =  max( maxx, compBounds.getRight() );
        maxy =  max( maxy, compBounds.getBottom() );
    }
    return Rectangle<int>(minx, miny, maxx-minx, maxy-miny);
}

void EditSelectionBox::mouseDown (const MouseEvent& e)
{
    if (component_set->size() == 0)
    {
        return;
    }
    updateMouseZone (e);
    prev_pos = e.getPosition();
    
    original_bounds = getBounds();
    
    /*
    if( e.mods.isAltDown() )
    {
        cout << "check" << endl;
        auto centre = getBounds().getCentre();
        for( auto it = component_set->begin(); it != component_set->end(); it++ )
        {
            (*it)->rotateScoreComponent( 0.1, centre.getX(), centre.getY() );
        }
        setBounds( getSelectionBounds() );

    }
    */
}

void EditSelectionBox::mouseDrag (const MouseEvent& e)
{
    if (component_set->size() == 0)
    {
        return;
    }
    
    
    /*
    if( e.mods.isShiftDown() )
    {
        auto xovery = (double)getWidth() / (double)getHeight();
        constrainer.setFixedAspectRatio( xovery );
    }
    else
    {
        constrainer.setFixedAspectRatio( 0.0 );
    }
    */
    
    Point<int> mouse_delta = e.getPosition() - prev_pos;
    prev_pos = e.getPosition();
    
    if( e.mods.isAltDown() ) // drag + alt = rotate
    {
        
        auto centre = original_bounds.getCentre();
        
        auto delta = getPosition() + e.getPosition() - centre;
        //printPoint(delta, "delta");

        auto dx = delta.getX(), dy = delta.getY();
        
       // auto dist = sqrt( dx*dx + dy*dy );
        auto theta = atan2(dy, dx) - float_Pi;
        
        if( m_prev_theta == -111 )
            m_prev_theta = theta;
        
        auto delta_rad = theta - m_prev_theta;
        m_prev_theta = theta;
        
        //cout << theta << " " << delta_rad << endl;
        for( auto it = component_set->begin(); it != component_set->end(); it++ )
        {
            (*it)->rotateScoreComponent( delta_rad, centre.getX(), centre.getY() );
        }
        
        setBounds( getSelectionBounds() );
        
    }
    else
    {
        
        const Rectangle<int> scaledBounds( mouseZone.resizeRectangleBy (getBounds(), mouse_delta) );
        
        // printRect(scaledBounds, "scaledBounds");
        
        float relative_x, relative_y , relative_w, relative_h;

        if (scaledBounds.getWidth() > m_minw && scaledBounds.getHeight() > m_minh )
        {
            for( auto it = component_set->begin(); it != component_set->end(); it++ )
            {
                // printRect((*it)->getBounds(), "comp");

                relative_x = (float)((*it)->getX() - getX()) / (float)getWidth();
                relative_y = (float)((*it)->getY() - getY()) / (float)getHeight();
                relative_w = (float)(*it)->getWidth() / (float)getWidth();
                relative_h = (float)(*it)->getHeight() / (float)getHeight();

                
                int new_rel_x = roundToInt(relative_x * (float)scaledBounds.getWidth());
                int new_rel_y = roundToInt(relative_y * (float)scaledBounds.getHeight());
                int new_w = roundToInt(relative_w * (float)scaledBounds.getWidth());
                int new_h = roundToInt(relative_h * (float)scaledBounds.getHeight());
                
                //printRect(Rectangle<float>(relative_x, relative_y, relative_w, relative_h), "relative");

                Component::Positioner* const pos = (*it)->getPositioner();
                if (pos)
                {
                    pos->applyNewBounds (Rectangle<int>(getX() + new_rel_x,
                                                        getY() + new_rel_y,
                                                        new_w,
                                                        new_h));
                }
                else
                {
                  
                    (*it)->setBounds(getX() + new_rel_x, getY() + new_rel_y, new_w, new_h);
                }
                
            }
            
    //        printRect( getSelectionBounds(), "selection bounds");
            setBounds( getSelectionBounds() );
        }
    }
}

void EditSelectionBox::mouseUp (const MouseEvent&)
{
    m_prev_theta = -111;
}

bool EditSelectionBox::hitTest (int x, int y)
{
    return x < borderSize.getLeft()
    || x >= getWidth() - borderSize.getRight()
    || y < borderSize.getTop()
    || y >= getHeight() - borderSize.getBottom();
}

void EditSelectionBox::setBorderThickness (const BorderSize<int>& newBorderSize)
{
    if (borderSize != newBorderSize)
    {
        borderSize = newBorderSize;
        repaint();
    }
}

BorderSize<int> EditSelectionBox::getBorderThickness() const
{
    return borderSize;
}

void EditSelectionBox::updateMouseZone (const MouseEvent& e)
{
    Zone newZone (Zone::fromPositionOnBorder (getLocalBounds(), borderSize, e.getPosition()));
    
    if (mouseZone != newZone)
    {
        mouseZone = newZone;
        setMouseCursor (newZone.getMouseCursor());
    }
}
