#pragma once
#include "SymbolistPoint.hpp"
#include <vector>
#include <algorithm>

class SymbolistRect
{
public:
    
    SymbolistPoint pos;
    double w = 0;
    double h = 0;
    
    SymbolistRect() {}
    
    /** Creates a copy of another SymbolistRect. */
    SymbolistRect (const SymbolistRect& other) : pos (other.pos), w (other.w), h (other.h) {}
    
    /** Creates a SymbolistRect with a given position and size. */
    SymbolistRect (double initialX, double initialY, double width, double height) : pos (initialX, initialY), w (width), h (height) {}
    
    /** Creates a SymbolistRect with a given size, and a position of (0, 0). */
    SymbolistRect (double width, double height) : w (width), h (height) {}
    
    /** Creates a SymbolistRect from the positions of two opposite corners. */
    SymbolistRect (SymbolistPoint corner1, SymbolistPoint corner2)
    : pos (std::min (corner1.x, corner2.x),
           std::min (corner1.y, corner2.y)),
    w (corner1.x - corner2.x),
    h (corner1.y - corner2.y)
    {
        if (w < double()) w = -w;
        if (h < double()) h = -h;
    }
    
    /** Creates a SymbolistRect from a set of left, right, top, bottom coordinates.
     The right and bottom values must be larger than the left and top ones, or the resulting
     SymbolistRect will have a negative size.
     */
    static SymbolistRect leftTopRightBottom (double left, double top, double right, double bottom)
    {
        return { left, top, right - left, bottom - top };
    }
    
    SymbolistRect& operator= (const SymbolistRect& other)
    {
        pos = other.pos;
        w = other.w; h = other.h;
        return *this;
    }
    
    ~SymbolistRect()  {}
    
    //==============================================================================
    /** Returns true if the SymbolistRect's width or height are zero or less */
    bool isEmpty() const                                    { return w <= 0 || h <= 0; }
      
    /** Returns the x coordinate of the SymbolistRect's left-hand-side. */
    inline double getX() const                           { return pos.x; }
    
    /** Returns the y coordinate of the SymbolistRect's top edge. */
    inline double getY() const                           { return pos.y; }
    
    /** Returns the width of the SymbolistRect. */
    inline double getWidth() const                       { return w; }
    
    /** Returns the height of the SymbolistRect. */
    inline double getHeight() const                      { return h; }
    
    /** Returns the x coordinate of the SymbolistRect's right-hand-side. */
    inline double getRight() const                       { return pos.x + w; }
    
    /** Returns the y coordinate of the SymbolistRect's bottom edge. */
    inline double getBottom() const                      { return pos.y + h; }
    
    /** Returns the x coordinate of the SymbolistRect's centre. */
    double getCentreX() const                            { return pos.x + w / (double) 2; }
    
    /** Returns the y coordinate of the SymbolistRect's centre. */
    double getCentreY() const                            { return pos.y + h / (double) 2; }
    
    /** Returns the centre point of the SymbolistRect. */
    SymbolistPoint getCentre() const                      {
        return { pos.x + w / (double) 2, pos.y + h / (double) 2 };
        
    }
    
    /** Returns the aspect ratio of the SymbolistRect's width / height.
     If widthOverHeight is true, it returns width / height; if widthOverHeight is false,
     it returns height / width. */
    double getAspectRatio (bool widthOverHeight = true) const                            { return widthOverHeight ? w / h : h / w; }
    
    //==============================================================================
    /** Returns the SymbolistRect's top-left position as a Point. */
    inline SymbolistPoint getPosition() const                                             { return pos; }
    
    /** Changes the position of the SymbolistRect's top-left corner (leaving its size unchanged). */
    inline void setPosition (SymbolistPoint newPos)                                       { pos = newPos; }
    
    /** Changes the position of the SymbolistRect's top-left corner (leaving its size unchanged). */
    inline void setPosition (double newX, double newY)                                { pos.setXY (newX, newY); }
    
    /** Returns the SymbolistRect's top-left position as a Point. */
    SymbolistPoint getTopLeft() const                                                     { return pos; }
    
    /** Returns the SymbolistRect's top-right position as a Point. */
    SymbolistPoint getTopRight() const                                                    { return { pos.x + w, pos.y }; }
    
    /** Returns the SymbolistRect's bottom-left position as a Point. */
    SymbolistPoint getBottomLeft() const                                                  { return { pos.x, pos.y + h }; }
    
    /** Returns the SymbolistRect's bottom-right position as a Point. */
    SymbolistPoint getBottomRight() const                                                 { return { pos.x + w, pos.y + h }; }

    /** Changes the SymbolistRect's size, leaving the position of its top-left corner unchanged. */
    void setSize (double newWidth, double newHeight)                                  { w = newWidth; h = newHeight; }
    
    /** Changes all the SymbolistRect's coordinates. */
    void setBounds (double newX, double newY,
                    double newWidth, double newHeight)
    {
        pos.x = newX; pos.y = newY; w = newWidth; h = newHeight;
        
    }
    
    /** Changes the SymbolistRect's X coordinate */
    inline void setX (double newX)                                                       { pos.x = newX; }
    
    /** Changes the SymbolistRect's Y coordinate */
    inline void setY (double newY)                                                       { pos.y = newY; }
    
    /** Changes the SymbolistRect's width */
    inline void setWidth (double newWidth)                                               { w = newWidth; }
    
    /** Changes the SymbolistRect's height */
    inline void setHeight (double newHeight)                                             { h = newHeight; }
    
    /** Changes the position of the SymbolistRect's centre (leaving its size unchanged). */
    inline void setCentre (double newCentreX, double newCentreY)                      { pos.x = newCentreX - w / (double) 2;
        pos.y = newCentreY - h / (double) 2; }
    
    /** Changes the position of the SymbolistRect's centre (leaving its size unchanged). */
    inline void setCentre (SymbolistPoint newCentre)                                      { setCentre (newCentre.x, newCentre.y); }

        
        /** Returns a SymbolistRect with the same centre position as this one, but a new size. */
        SymbolistRect withSizeKeepingCentre (double newWidth, double newHeight) const         { return { pos.x + (w - newWidth)  / (double) 2,
            pos.y + (h - newHeight) / (double) 2, newWidth, newHeight }; }
        
        /** Moves the x position, adjusting the width so that the right-hand edge remains in the same place.
         If the x is moved to be on the right of the current right-hand edge, the width will be set to zero.
         @see withLeft
         */
        void setLeft (double newLeft)                    { w = std::max (double(), pos.x + w - newLeft); pos.x = newLeft; }
    
        /** Moves the y position, adjusting the height so that the bottom edge remains in the same place.
         If the y is moved to be below the current bottom edge, the height will be set to zero.
         @see withTop
         */
        void setTop (double newTop)                      { h = std::max (double(), pos.y + h - newTop); pos.y = newTop; }
    
        /** Adjusts the width so that the right-hand edge of the SymbolistRect has this new value.
         If the new right is below the current X value, the X will be pushed down to match it.
         @see getRight, withRight
         */
        void setRight (double newRight)                  { pos.x = std::min (pos.x, newRight); w = newRight - pos.x; }
    
        /** Adjusts the height so that the bottom edge of the SymbolistRect has this new value.
         If the new bottom is lower than the current Y value, the Y will be pushed down to match it.
         @see getBottom, withBottom
         */
        void setBottom (double newBottom)                { pos.y = std::min (pos.y, newBottom); h = newBottom - pos.y; }
    
        //==============================================================================
        /** Moves the SymbolistRect's position by adding amount to its x and y coordinates. */
        void translate (double deltaX,
                        double deltaY)
        {
            pos.x += deltaX;
            pos.y += deltaY;
        }
        
        /** Returns a SymbolistRect which is the same as this one moved by a given amount. */
        SymbolistRect translated (double deltaX, double deltaY) const
        {
            return { pos.x + deltaX, pos.y + deltaY, w, h };
        }
        
        /** Returns a SymbolistRect which is the same as this one moved by a given amount. */
        SymbolistRect operator+ (SymbolistPoint deltaPosition) const
        {
            return { pos.x + deltaPosition.x, pos.y + deltaPosition.y, w, h };
        }
        
        /** Moves this SymbolistRect by a given amount. */
        SymbolistRect& operator+= (SymbolistPoint deltaPosition)
        {
            pos += deltaPosition;
            return *this;
        }
        
        /** Returns a SymbolistRect which is the same as this one moved by a given amount. */
        SymbolistRect operator- (SymbolistPoint deltaPosition) const
        {
            return { pos.x - deltaPosition.x, pos.y - deltaPosition.y, w, h };
        }
        
        /** Moves this SymbolistRect by a given amount. */
        SymbolistRect& operator-= (SymbolistPoint deltaPosition)
        {
            pos -= deltaPosition;
            return *this;
        }
        
        /** Returns a SymbolistRect that has been scaled by the given amount, centred around the origin.
         Note that if the SymbolistRect has int coordinates and it's scaled by a
         floating-point amount, then the result will be converted back to integer
         coordinates using getSmallestIntegerContainer().
         */
    
        SymbolistRect operator* (double scaleFactor) const
        {
            SymbolistRect r (*this);
            r *= scaleFactor;
            return r;
        }
        
        /** Scales this SymbolistRect by the given amount, centred around the origin.
         Note that if the SymbolistRect has int coordinates and it's scaled by a
         floating-point amount, then the result will be converted back to integer
         coordinates using getSmallestIntegerContainer().
         */
        SymbolistRect operator*= (double scaleFactor)
        {
            SymbolistRect ( pos.x * scaleFactor,
                            pos.y * scaleFactor,
                            w * scaleFactor,
                            h * scaleFactor );
            return *this;
        }
        
        /** Scales this SymbolistRect by the given X and Y factors, centred around the origin.
         Note that if the SymbolistRect has int coordinates and it's scaled by a
         floating-point amount, then the result will be converted back to integer
         coordinates using getSmallestIntegerContainer().
         */
    
        SymbolistRect operator*= (SymbolistPoint scaleFactor)
        {
            SymbolistRect (pos.x * scaleFactor.x,
                                  pos.y * scaleFactor.y,
                                  w * scaleFactor.x,
                                  h * scaleFactor.y);
            return *this;
        }
        
        /** Scales this SymbolistRect by the given amount, centred around the origin. */
    
        SymbolistRect operator/ (double scaleFactor) const
        {
            SymbolistRect r (*this);
            r /= scaleFactor;
            return r;
        }
        
        /** Scales this SymbolistRect by the given amount, centred around the origin. */
    
        SymbolistRect operator/= (double scaleFactor)
        {
            SymbolistRect (pos.x / scaleFactor,
                                  pos.y / scaleFactor,
                                  w / scaleFactor,
                                  h / scaleFactor);
            return *this;
        }
        
        /** Scales this SymbolistRect by the given X and Y factors, centred around the origin. */
    
        SymbolistRect operator/= (SymbolistPoint scaleFactor)
        {
            SymbolistRect (pos.x / scaleFactor.x,
                                  pos.y / scaleFactor.y,
                                  w / scaleFactor.x,
                                  h / scaleFactor.y);
            return *this;
        }
    
    
    
        /** Expands the SymbolistRect by a given amount.
         
         Effectively, its new size is (x - deltaX, y - deltaY, w + deltaX * 2, h + deltaY * 2).
         @see expanded, reduce, reduced
         */
        void expand (double deltaX,
                     double deltaY)
        {
            auto nw = std::max (double(), w + deltaX * 2);
            auto nh = std::max (double(), h + deltaY * 2);
            setBounds (pos.x - deltaX, pos.y - deltaY, nw, nh);
        }
        
        /** Returns a SymbolistRect that is larger than this one by a given amount.
         
         Effectively, the SymbolistRect returned is (x - deltaX, y - deltaY, w + deltaX * 2, h + deltaY * 2).
         @see expand, reduce, reduced
         */
        SymbolistRect expanded (double deltaX,
                            double deltaY) const
        {
            auto nw = std::max (double(), w + deltaX * 2);
            auto nh = std::max (double(), h + deltaY * 2);
            return { pos.x - deltaX, pos.y - deltaY, nw, nh };
        }
        
        /** Returns a SymbolistRect that is larger than this one by a given amount.
         
         Effectively, the SymbolistRect returned is (x - delta, y - delta, w + delta * 2, h + delta * 2).
         @see expand, reduce, reduced
         */
        SymbolistRect expanded (double delta) const
        {
            return expanded (delta, delta);
        }
        
        /** Shrinks the SymbolistRect by a given amount.
         
         Effectively, its new size is (x + deltaX, y + deltaY, w - deltaX * 2, h - deltaY * 2).
         @see reduced, expand, expanded
         */
        void reduce (double deltaX,
                     double deltaY)
        {
            expand (-deltaX, -deltaY);
        }
        
        /** Returns a SymbolistRect that is smaller than this one by a given amount.
         
         Effectively, the SymbolistRect returned is (x + deltaX, y + deltaY, w - deltaX * 2, h - deltaY * 2).
         @see reduce, expand, expanded
         */
        SymbolistRect reduced (double deltaX,
                           double deltaY) const
        {
            return expanded (-deltaX, -deltaY);
        }
        
        /** Returns a SymbolistRect that is smaller than this one by a given amount.
         
         Effectively, the SymbolistRect returned is (x + delta, y + delta, w - delta * 2, h - delta * 2).
         @see reduce, expand, expanded
         */
        SymbolistRect reduced (double delta) const
        {
            return reduced (delta, delta);
        }
        
        /** Removes a strip from the top of this SymbolistRect, reducing this SymbolistRect
         by the specified amount and returning the section that was removed.
         
         E.g. if this SymbolistRect is (100, 100, 300, 300) and amountToRemove is 50, this will
         return (100, 100, 300, 50) and leave this SymbolistRect as (100, 150, 300, 250).
         
         If amountToRemove is greater than the height of this SymbolistRect, it'll be clipped to
         that value.
         */
        SymbolistRect removeFromTop (double amountToRemove)
        {
            const SymbolistRect r (pos.x, pos.y, w, std::min (amountToRemove, h));
            pos.y += r.h; h -= r.h;
            return r;
        }
        
        /** Removes a strip from the left-hand edge of this SymbolistRect, reducing this SymbolistRect
         by the specified amount and returning the section that was removed.
         
         E.g. if this SymbolistRect is (100, 100, 300, 300) and amountToRemove is 50, this will
         return (100, 100, 50, 300) and leave this SymbolistRect as (150, 100, 250, 300).
         
         If amountToRemove is greater than the width of this SymbolistRect, it'll be clipped to
         that value.
         */
        SymbolistRect removeFromLeft (double amountToRemove)
        {
            const SymbolistRect r (pos.x, pos.y, std::min (amountToRemove, w), h);
            pos.x += r.w; w -= r.w;
            return r;
        }
        
        /** Removes a strip from the right-hand edge of this SymbolistRect, reducing this SymbolistRect
         by the specified amount and returning the section that was removed.
         
         E.g. if this SymbolistRect is (100, 100, 300, 300) and amountToRemove is 50, this will
         return (250, 100, 50, 300) and leave this SymbolistRect as (100, 100, 250, 300).
         
         If amountToRemove is greater than the width of this SymbolistRect, it'll be clipped to
         that value.
         */
        SymbolistRect removeFromRight (double amountToRemove)
        {
            amountToRemove = std::min (amountToRemove, w);
            const SymbolistRect r (pos.x + w - amountToRemove, pos.y, amountToRemove, h);
            w -= amountToRemove;
            return r;
        }
        
        /** Removes a strip from the bottom of this SymbolistRect, reducing this SymbolistRect
         by the specified amount and returning the section that was removed.
         
         E.g. if this SymbolistRect is (100, 100, 300, 300) and amountToRemove is 50, this will
         return (100, 250, 300, 50) and leave this SymbolistRect as (100, 100, 300, 250).
         
         If amountToRemove is greater than the height of this SymbolistRect, it'll be clipped to
         that value.
         */
        SymbolistRect removeFromBottom (double amountToRemove)
        {
            amountToRemove = std::min (amountToRemove, h);
            const SymbolistRect r (pos.x, pos.y + h - amountToRemove, w, amountToRemove);
            h -= amountToRemove;
            return r;
        }
    
    
        //==============================================================================
        /** Returns true if the two SymbolistRects are identical. */
        bool operator== (const SymbolistRect& other) const      { return pos == other.pos && w == other.w && h == other.h; }
        
        /** Returns true if the two SymbolistRects are not identical. */
        bool operator!= (const SymbolistRect& other) const      { return pos != other.pos || w != other.w || h != other.h; }
        
        /** Returns true if this coordinate is inside the SymbolistRect. */
        bool contains (double xCoord, double yCoord) const
        {
            return xCoord >= pos.x && yCoord >= pos.y && xCoord < pos.x + w && yCoord < pos.y + h;
        }
        
        /** Returns true if this coordinate is inside the SymbolistRect. */
        bool contains (SymbolistPoint point) const
        {
            return point.x >= pos.x && point.y >= pos.y && point.x < pos.x + w && point.y < pos.y + h;
        }
        
        /** Returns true if this other SymbolistRect is completely inside this one. */
        bool contains (SymbolistRect other) const
        {
            return pos.x <= other.pos.x && pos.y <= other.pos.y
            && pos.x + w >= other.pos.x + other.w && pos.y + h >= other.pos.y + other.h;
        }
        
        /** Returns true if any part of another SymbolistRect overlaps this one. */
        bool intersects (SymbolistRect other) const
        {
            return pos.x + w > other.pos.x
            && pos.y + h > other.pos.y
            && pos.x < other.pos.x + other.w
            && pos.y < other.pos.y + other.h
            && w > double() && h > double()
            && other.w > double() && other.h > double();
        }
        
        /** Returns true if any part of the given line lies inside this SymbolistRect. */
    /*
        bool intersects (const Line<double>& line) const
        {
            return contains (line.getStart()) || contains (line.getEnd())
            || line.intersects (Line<double> (getTopLeft(),     getTopRight()))
            || line.intersects (Line<double> (getTopRight(),    getBottomRight()))
            || line.intersects (Line<double> (getBottomRight(), getBottomLeft()))
            || line.intersects (Line<double> (getBottomLeft(),  getTopLeft()));
        }
      */
        /** Returns the region that is the overlap between this and another SymbolistRect.
         If the two SymbolistRects don't overlap, the SymbolistRect returned will be empty.
         */
        SymbolistRect getIntersection (SymbolistRect other) const
        {
            auto nx = std::max (pos.x, other.pos.x);
            auto ny = std::max (pos.y, other.pos.y);
            auto nw = std::min (pos.x + w, other.pos.x + other.w) - nx;
            
            if (nw >= double())
            {
                auto nh = std::min (pos.y + h, other.pos.y + other.h) - ny;
                
                if (nh >= double())
                    return { nx, ny, nw, nh };
            }
            
            return {};
        }
        
        /** Clips a set of SymbolistRect coordinates so that they lie only within this one.
         This is a non-static version of intersectSymbolistRects().
         Returns false if the two SymbolistRects didn't overlap.
         */
        bool intersectSymbolistRect (double& otherX, double& otherY, double& otherW, double& otherH) const
        {
            auto maxX = std::max (otherX, pos.x);
            otherW = std::min (otherX + otherW, pos.x + w) - maxX;
            
            if (otherW > double())
            {
                auto maxY = std::max (otherY, pos.y);
                otherH = std::min (otherY + otherH, pos.y + h) - maxY;
                
                if (otherH > double())
                {
                    otherX = maxX; otherY = maxY;
                    return true;
                }
            }
            
            return false;
        }
        
        /** Clips a SymbolistRect so that it lies only within this one.
         Returns false if the two SymbolistRects didn't overlap.
         */
        bool intersectSymbolistRect (SymbolistRect& SymbolistRectToClip) const
        {
            return intersectSymbolistRect (SymbolistRectToClip.pos.x, SymbolistRectToClip.pos.y,
                                       SymbolistRectToClip.w,     SymbolistRectToClip.h);
        }
        
        /** Returns the smallest SymbolistRect that contains both this one and the one passed-in.
         
         If either this or the other SymbolistRect are empty, they will not be counted as
         part of the resulting region.
         */
        SymbolistRect getUnion (SymbolistRect other) const
        {
            if (other.isEmpty())  return *this;
            if (isEmpty())        return other;
            
            auto newX = std::min (pos.x, other.pos.x);
            auto newY = std::min (pos.y, other.pos.y);
            
            return { newX, newY,
                std::max (pos.x + w, other.pos.x + other.w) - newX,
                std::max (pos.y + h, other.pos.y + other.h) - newY };
        }
        
        /** If this SymbolistRect merged with another one results in a simple SymbolistRect, this
         will set this SymbolistRect to the result, and return true.
         
         Returns false and does nothing to this SymbolistRect if the two SymbolistRects don't overlap,
         or if they form a complex region.
         */
        bool enlargeIfAdjacent (SymbolistRect other)
        {
            if (pos.x == other.pos.x && getRight() == other.getRight()
                && (other.getBottom() >= pos.y && other.pos.y <= getBottom()))
            {
                auto newY = std::min (pos.y, other.pos.y);
                h = std::max (getBottom(), other.getBottom()) - newY;
                pos.y = newY;
                return true;
            }
            
            if (pos.y == other.pos.y && getBottom() == other.getBottom()
                && (other.getRight() >= pos.x && other.pos.x <= getRight()))
            {
                auto newX = std::min (pos.x, other.pos.x);
                w = std::max (getRight(), other.getRight()) - newX;
                pos.x = newX;
                return true;
            }
            
            return false;
        }
        
        /** If after removing another SymbolistRect from this one the result is a simple SymbolistRect,
         this will set this object's bounds to be the result, and return true.
         
         Returns false and does nothing to this SymbolistRect if the two SymbolistRects don't overlap,
         or if removing the other one would form a complex region.
         */
        bool reduceIfPartlyContainedIn (SymbolistRect other)
        {
            int inside = 0;
            auto otherR = other.getRight();
            if (pos.x >= other.pos.x && pos.x < otherR) inside = 1;
            auto otherB = other.getBottom();
            if (pos.y >= other.pos.y && pos.y < otherB) inside |= 2;
            auto r = pos.x + w;
            if (r >= other.pos.x && r < otherR) inside |= 4;
            auto b = pos.y + h;
            if (b >= other.pos.y && b < otherB) inside |= 8;
            
            switch (inside)
            {
                case 1 + 2 + 8:     w = r - otherR; pos.x = otherR; return true;
                case 1 + 2 + 4:     h = b - otherB; pos.y = otherB; return true;
                case 2 + 4 + 8:     w = other.pos.x - pos.x; return true;
                case 1 + 4 + 8:     h = other.pos.y - pos.y; return true;
            }
            
            return false;
        }
        
    
        
        /** Casts this SymbolistRect to a SymbolistRect<int>.
         This uses roundToInt to snap x, y, width and height to the nearest integer (losing precision).
         If the SymbolistRect already uses integers, this will simply return a copy.
         @see getSmallestIntegerContainer(), toNearestIntEdges()
         */
        std::vector<long> toNearestInt() const
        {
            return std::vector<long>({ std::lround (pos.x), std::lround (pos.y),
                    std::lround (w),     std::lround (h) });
        }
        
    
        /** Returns the smallest SymbolistRect that can contain a set of points. */
        static SymbolistRect findAreaContainingPoints (const std::vector<SymbolistPoint> points)
        {
            if (points.size() <= 0)
                return {};
            
            auto minX = points[0].x;
            auto maxX = minX;
            auto minY = points[0].y;
            auto maxY = minY;
            
            for (int i = 1; i < points.size(); ++i)
            {
                minX = std::min (minX, points[i].x);
                maxX = std::max (maxX, points[i].x);
                minY = std::min (minY, points[i].y);
                maxY = std::max (maxY, points[i].y);
            }
            
            return { minX, minY, maxX - minX, maxY - minY };
        }
        
        //==============================================================================
        /** Static utility to intersect two sets of rectangular coordinates.
         Returns false if the two regions didn't overlap.
         @see intersectSymbolistRect
         */
        static bool intersectSymbolistRects (double& x1, double& y1, double& w1, double& h1,
                                         double  x2, double  y2, double  w2, double  h2)
        {
            auto x = std::max (x1, x2);
            w1 = std::min (x1 + w1, x2 + w2) - x;
            
            if (w1 > double())
            {
                auto y = std::max (y1, y2);
                h1 = std::min (y1 + h1, y2 + h2) - y;
                
                if (h1 > double())
                {
                    x1 = x; y1 = y;
                    return true;
                }
            }
            
            return false;
        }
    
    
    /**
     * expands rectangle to fit the point at X Y
     *
     * @param x    x value
     *
     * @param y    y value
     */
    void expandToFit( double x, double y )
    {
        if( getPosition().isEmpty() )
            setPosition(x, y);
        
        if (x < getX() )
            setX(x);
        else if ( x > getRight() )
            setRight(x);
        
        if (y < getY() )
            setY(y);
        else if ( y > getBottom() )
            setBottom(y);
    }
    
        
};
            
