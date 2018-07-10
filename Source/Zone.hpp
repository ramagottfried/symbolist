#ifndef Zone_hpp
#define Zone_hpp

#include <stdio.h>
#include "JuceHeader.h"

//==============================================================================
/** Represents the different sections of a resizable border, which allow it to
 resized in different ways.
 */
class Zone
{
public:
	//==============================================================================
	enum Zones
	{
		centre  = 0,
		left    = 1,
		top     = 2,
		right   = 4,
		bottom  = 8
	};
	
	//==============================================================================
	/** Creates a Zone from a combination of the flags in \enum Zones. */
	explicit Zone (int zoneFlags) noexcept;
	
	Zone() noexcept;
	Zone (const Zone&) noexcept;
	Zone& operator= (const Zone&) noexcept;
	
	bool operator== (const Zone&) const noexcept;
	bool operator!= (const Zone&) const noexcept;
	
	//==============================================================================
	/** Given a point within a rectangle with a resizable border, this returns the
	 zone that the point lies within.
	 */
	static Zone fromPositionOnBorder (const Rectangle<int>& totalSize,
									  const BorderSize<int>& border,
									  Point<int> position);
	
	/** Returns an appropriate mouse-cursor for this resize zone. */
	MouseCursor getMouseCursor() const noexcept;
	
	/** Returns true if dragging this zone will move the enire object without resizing it. */
	bool isDraggingWholeObject() const noexcept     { return zone == centre; }
	/** Returns true if dragging this zone will move the object's left edge. */
	bool isDraggingLeftEdge() const noexcept        { return (zone & left) != 0; }
	/** Returns true if dragging this zone will move the object's right edge. */
	bool isDraggingRightEdge() const noexcept       { return (zone & right) != 0; }
	/** Returns true if dragging this zone will move the object's top edge. */
	bool isDraggingTopEdge() const noexcept         { return (zone & top) != 0; }
	/** Returns true if dragging this zone will move the object's bottom edge. */
	bool isDraggingBottomEdge() const noexcept      { return (zone & bottom) != 0; }
	
	/** Resizes this rectangle by the given amount, moving just the edges that this zone
	 applies to.
	 */
	template <typename ValueType>
	Rectangle<ValueType> resizeRectangleBy (Rectangle<ValueType> original, const Point<ValueType>& distance) const noexcept
	{
		if (isDraggingWholeObject())
			return original + distance;
		
		if (isDraggingLeftEdge())
		{
			original.setLeft (jmin (original.getRight(), original.getX() + distance.x));
		}
		else if (isDraggingRightEdge())
		{
			original.setWidth (jmax (ValueType(), original.getWidth() + distance.x));
		}
		
		if (isDraggingTopEdge())
		{
			original.setTop (jmin (original.getBottom(), original.getY() + distance.y));
		}
		else if (isDraggingBottomEdge())
		{
			original.setHeight (jmax (ValueType(), original.getHeight() + distance.y));
		}
		
		return original;
	}
	
	/** Returns the raw flags for this zone. */
	int getZoneFlags() const noexcept               { return zone; }
	
private:
	//==============================================================================
	int zone;
};

#endif /* Zone_hpp */
