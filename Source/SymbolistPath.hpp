#pragma once

#include "SymbolistPoint.hpp"
#include "SymbolistRect.hpp"
#include "SymbolistPathSegment.hpp"

#include <string>
#include <iostream>

using namespace std;

// inserting points into the Path could also do the bound, which makes some sense, since any Path will have bounds...

// parse from SVG string
// lookup functions


class SymbolistPath
{
public:
    SymbolistPath(){}
    SymbolistPath(const string& svg_path) { fromSVG(svg_path); }
    
    ~SymbolistPath(){}
    void clear();

    void addSegment(SymbolistPoint a, SymbolistPoint b);
    void addSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c);
    void addSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c, SymbolistPoint d);

    void fromSVG(const string& svg_path);
    
    SymbolistRect getBounds() { return m_bounds; }

    void print();
    
private:
    
    SymbolistRect                   m_bounds;
    vector<SymbolistPathSegment>    m_path;
    double                          m_length = 0;
    
    /*
     *      Utility functions
     */
    
    void adjustBounds(SymbolistPoint a, SymbolistPoint b);
    void adjustBounds(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c);
    void adjustBounds(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c, SymbolistPoint d);
  
    vector<SymbolistPoint> parseSegment(string& seg, const char type, SymbolistPoint startPt);

    /**
     *  calculates a point on a quadratic bezier curve
     *
     * @param t    0 - 1 ratio to lookup
     *
     * @param a    SymbolistPoint A start anchor
     *
     * @param b    SymbolistPoint B control point
     *
     * @param c    SymbolistPoint C end anchor
     *
     * @return    SymbolistPoint point on curve
     **/
    static SymbolistPoint calcBezier( double t, SymbolistPoint a, SymbolistPoint b, SymbolistPoint c  );
    
    /**
     *  calculates a point on a cubic bezier curve
     *
     * @param t    0 - 1 ratio to lookup
     *
     * @param a    SymbolistPoint A start anchor
     *
     * @param b    SymbolistPoint B control point 1
     *
     * @param c    SymbolistPoint C control point 2
     *
     * @param d    SymbolistPoint C end anchor
     *
     * @return    SymbolistPoint point on curve
     */
    static SymbolistPoint calcBezier( double t, SymbolistPoint a, SymbolistPoint b, SymbolistPoint c, SymbolistPoint d  );
    
    /**
     * calculates roots for quadradic bezier
     *
     * @param a    SymbolistPoint A start anchor
     *
     * @param b    SymbolistPoint B control point 1
     *
     * @param c    SymbolistPoint C end anchor
     *
     * @return    vector<SymbolistPoint>
     */
    vector<SymbolistPoint> quadroots( SymbolistPoint a, SymbolistPoint b, SymbolistPoint c );
    
    /**
     *  calculates roots for quadradic bezier
     *
     * @param p1    SymbolistPoint p1 start anchor
     *
     * @param p2    SymbolistPoint p2 control point 1
     *
     * @param p3    SymbolistPoint p3 control point 2
     *
     * @param p4    SymbolistPoint p4 end anchor
     *
     * @return    vector<SymbolistPoint>
     */
    vector<SymbolistPoint> getCubicRoots(SymbolistPoint p1, SymbolistPoint p2, SymbolistPoint p3, SymbolistPoint p4);
    
    /**
     * calculates bounds for quadradic bezier
     *
     * @param a    SymbolistPoint A start anchor
     *
     * @param b    SymbolistPoint B control point 1
     *
     * @param c    SymbolistPoint C control point 2
     *
     * @return    SymbolistRect
     */
    SymbolistRect getBoundsQuadratic( SymbolistPoint a, SymbolistPoint b, SymbolistPoint c );
    
    inline SymbolistRect getBoundsQuadratic( double x1, double y1, double x2, double y2, double x3, double y3 )
    {
        return getBoundsQuadratic( SymbolistPoint(x1,y1), SymbolistPoint(x2,y2), SymbolistPoint(x3,y3) );
    }
    
    /**
     * currently unimplemented
     */
    SymbolistRect getBoundsCubic(double x0, double y0, double x1, double y1, double x2, double y2, double x3, double y3);

    
};

