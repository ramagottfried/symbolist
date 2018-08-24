#pragma once

#include "SymbolistPoint.hpp"
#include "AGG_BezierLength.hpp"

using namespace std;


class SymbolistFlatPath
{
public:
    SymbolistFlatPath(){}
    SymbolistFlatPath(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c);
    SymbolistFlatPath(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c,  SymbolistPoint d);
    
    vector<agg::point_d> getPoints();
    
    double getLength();
    
private:
    int m_type = 0;
    agg::curve3 m_curve3;
    agg::curve4 m_curve4;
};



class SymbolistPathSegment
{
public:
    SymbolistPathSegment(){}
    SymbolistPathSegment(SymbolistPoint a, SymbolistPoint b);
    SymbolistPathSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c);
    SymbolistPathSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c,  SymbolistPoint d);
    
    // //C, c, L, l, Q, q (M, m, Z, z are re-formatted to give start/end points for each segment)
    // actually only L, Q, and C, make everything absolute
    vector<SymbolistPoint> getPoints() { return pts; }
    
    
    SymbolistPathSegment& operator += ( const SymbolistPoint& delta )
    {
        for( auto& p : pts )
        {
            p += delta;
        }
        return *this;
    }
    
    
    char type;
    vector<SymbolistPoint> pts; // array of points
    double length = 0; // segment length for lookup
   
    double startTime = 0; // store start time for lookup?
    
    SymbolistFlatPath m_flat;
    
};
