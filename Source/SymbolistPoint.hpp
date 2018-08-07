#pragma once

#include <cmath>

/*
 adapted from JUCE point
 */
struct SymbolistPoint
{
    double x = NAN;
    double y = NAN;
    
    SymbolistPoint() {}
    SymbolistPoint (const SymbolistPoint& other) : x (other.x), y (other.y)  {}
    SymbolistPoint (double initialX, double initialY) : x (initialX), y (initialY) {}
    
    bool isEmpty() { return ( std::isnan(x) && std::isnan(y)); }
    
    
    SymbolistPoint& operator= (const SymbolistPoint& other) { x = other.x; y = other.y; return *this; }
    
     inline bool operator== (SymbolistPoint other) const       { return x == other.x && y == other.y; }
     inline bool operator!= (SymbolistPoint other) const       { return x != other.x || y != other.y; }
    
     bool isOrigin() const                            { return x == 0 && y == 0; }
    
    /** Returns true if the coordinates are finite values. */
   //  inline bool isFinite() const                     { return juce_isfinite(x) && juce_isfinite(y); }
    
    /** Returns the SymbolistPoint's x coordinate. */
     inline double getX() const                    { return x; }
    
    /** Returns the SymbolistPoint's y coordinate. */
     inline double getY() const                    { return y; }
    
    /** Sets the SymbolistPoint's x coordinate. */
    inline void setX (double newX)                               { x = newX; }
    
    /** Sets the SymbolistPoint's y coordinate. */
    inline void setY (double newY)                               { y = newY; }
    
    /** Returns a SymbolistPoint which has the same Y position as this one, but a new X. */
     SymbolistPoint withX (double newX) const               { return SymbolistPoint (newX, y); }
    
    /** Returns a SymbolistPoint which has the same X position as this one, but a new Y. */
     SymbolistPoint withY (double newY) const               { return SymbolistPoint (x, newY); }
    
    /** Changes the SymbolistPoint's x and y coordinates. */
    void setXY (double newX, double newY)                     { x = newX; y = newY; }
    
    /** Adds a pair of coordinates to this value. */
    void addXY (double xToAdd, double yToAdd)                 { x += xToAdd; y += yToAdd; }
    
    //==============================================================================
    /** Returns a SymbolistPoint with a given offset from this one. */
     SymbolistPoint translated (double deltaX, double deltaY) const     { return SymbolistPoint (x + deltaX, y + deltaY); }
    
    /** Adds two SymbolistPoints together */
     SymbolistPoint operator+ (SymbolistPoint other) const              { return SymbolistPoint (x + other.x, y + other.y); }
    
    /** Adds another SymbolistPoint's coordinates to this one */
    SymbolistPoint& operator+= (SymbolistPoint other)                                 { x += other.x; y += other.y; return *this; }
    
    /** Subtracts one SymbolistPoints from another */
     SymbolistPoint operator- (SymbolistPoint other) const              { return SymbolistPoint (x - other.x, y - other.y); }
    
    /** Subtracts another SymbolistPoint's coordinates to this one */
    SymbolistPoint& operator-= (SymbolistPoint other)                                 { x -= other.x; y -= other.y; return *this; }
    
    /** Multiplies two SymbolistPoints together */
    SymbolistPoint operator* (SymbolistPoint other) const   { return SymbolistPoint ((double) (x * other.x), (double) (y * other.y)); }
    
    /** Multiplies another SymbolistPoint's coordinates to this one */
    SymbolistPoint& operator*= (SymbolistPoint other)                      { *this = *this * other; return *this; }
    
    /** Divides one SymbolistPoint by another */
     SymbolistPoint operator/ (SymbolistPoint other) const   { return SymbolistPoint ((double) (x / other.x), (double) (y / other.y)); }
    
    /** Divides this SymbolistPoint's coordinates by another */
    SymbolistPoint& operator/= (SymbolistPoint other)                      { *this = *this / other; return *this; }
    
    /** Returns a SymbolistPoint whose coordinates are multiplied by a given scalar value. */
     SymbolistPoint operator* (double multiplier) const     { return SymbolistPoint (x * multiplier, y * multiplier); }
    
    /** Returns a SymbolistPoint whose coordinates are divided by a given scalar value. */
    
     SymbolistPoint operator/ (double divisor) const        { return SymbolistPoint ((double) (x / divisor), (double) (y / divisor)); }
    
    /** Multiplies the SymbolistPoint's coordinates by a scalar value. */
    
    SymbolistPoint& operator*= (double multiplier)                        { x = (double) (x * multiplier); y = (double) (y * multiplier); return *this; }
    
    /** Divides the SymbolistPoint's coordinates by a scalar value. */
    
    SymbolistPoint& operator/= (double divisor)                           { x = (double) (x / divisor); y = (double) (y / divisor); return *this; }
    
    /** Returns the inverse of this SymbolistPoint. */
     SymbolistPoint operator-() const                          { return SymbolistPoint (-x, -y); }
    
    
    //==============================================================================
    /** Returns the straight-line distance between this SymbolistPoint and the origin. */
    inline double getDistanceFromOrigin() const                                { return sqrt(x*x + y*y); }
    
    /** Returns the square of the straight-line distance between this SymbolistPoint and the origin. */
     inline double getDistanceSquaredFromOrigin() const          { return x * x + y * y; }
    
    /** Returns the square of the straight-line distance between this SymbolistPoint and another one. */
    inline double getDistanceSquaredFrom (SymbolistPoint other) const    { return (*this - other).getDistanceSquaredFromOrigin(); }
    
    /** Returns the straight-line distance between this SymbolistPoint and another one. */
    inline double getDistanceFrom (SymbolistPoint other) const { return sqrt ( getDistanceSquaredFrom(other) ); }
    
    
    /** Returns the angle from this SymbolistPoint to another one.
    
     Taking this SymbolistPoint to be the centre of a circle, and the other SymbolistPoint being a position on
     the circumference, the return value is the number of radians clockwise from the 12 o'clock
     direction.
     So 12 o'clock = 0, 3 o'clock = Pi/2, 6 o'clock = Pi, 9 o'clock = -Pi/2
     */
    double getAngleToPoint (SymbolistPoint other) const
    {
        return std::atan2 ( other.x - x, y - other.y );
    }
    
    /** Returns the SymbolistPoint that would be reached by rotating this SymbolistPoint clockwise
     about the origin by the specified angle.
     */
    SymbolistPoint rotatedAboutOrigin (double angleRadians) const
    {
        return SymbolistPoint (x * std::cos (angleRadians) - y * std::sin (angleRadians),
                      x * std::sin (angleRadians) + y * std::cos (angleRadians));
    }
    
    /** Taking this SymbolistPoint to be the centre of a circle, this returns a SymbolistPoint on its circumference.
     @param radius   the radius of the circle.
     @param angle    the angle of the SymbolistPoint, in radians clockwise from the 12 o'clock position.
     */
    SymbolistPoint getPointOnCircumference (float radius, float angle) const
    {
        return SymbolistPoint ( x + radius * std::sin (angle),
                                y - radius * std::cos (angle) );
    }
    
    /** Taking this SymbolistPoint to be the centre of an ellipse, this returns a SymbolistPoint on its circumference.
     @param radiusX  the horizontal radius of the circle.
     @param radiusY  the vertical radius of the circle.
     @param angle    the angle of the SymbolistPoint, in radians clockwise from the 12 o'clock position.
     */
    SymbolistPoint getPointOnCircumference (float radiusX, float radiusY, float angle) const
    {
        return SymbolistPoint ( x + radiusX * std::sin (angle),
                                y - radiusY * std::cos (angle) );
    }
    
    /** Returns the dot-product of two SymbolistPoints (x1 * x2 + y1 * y2). */
    double getDotProduct (SymbolistPoint other) const { return x * other.x + y * other.y; }
   
    /*
    SymbolistPoint transformedBy (const AffineTransform& transform) const
    {
        return SymbolistPoint ( transform.mat00 * x + transform.mat01 * y + transform.mat02,
                                transform.mat10 * x + transform.mat11 * y + transform.mat12 );
    }
     */
    

};
