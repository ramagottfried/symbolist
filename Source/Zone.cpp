#include "Zone.hpp"

Zone::Zone() noexcept
: zone (0)
{}

Zone::Zone (const int zoneFlags) noexcept
: zone (zoneFlags)
{}

Zone::Zone (const Zone& other) noexcept
: zone (other.zone)
{}

Zone& Zone::operator= (const Zone& other) noexcept
{
    zone = other.zone;
    return *this;
}

bool Zone::operator== (const Zone& other) const noexcept      { return zone == other.zone; }
bool Zone::operator!= (const Zone& other) const noexcept      { return zone != other.zone; }

Zone Zone::fromPositionOnBorder (const Rectangle<int>& totalSize,
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

MouseCursor Zone::getMouseCursor() const noexcept
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


