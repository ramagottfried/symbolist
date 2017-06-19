#pragma once

#include "BaseComponent.h"

namespace PathInfo
{
    
    Point<float> calcBezier( float t, Point<float> a, Point<float> b, Point<float> c  );
    Point<float> calcBezier( float t, Point<float> a, Point<float> b, Point<float> c, Point<float> d  );

    std::vector<Point<float> > quadroots( Point<float> a, Point<float> b, Point<float> c );
 
    
    Rectangle<float> getBoundsQuadratic( float x1, float y1, float x2, float y2, float x3, float y3 );
    
    void accumPathBounds( Rectangle<float>& currentBounds, float x, float y);
    
}

class Sym_PathBounds : public Rectangle<float>
{
public:
    Sym_PathBounds(){}
    Sym_PathBounds(const Path& p)
    {
        getRealPathBounds( p );
    }
    
    void init();
    void adjustWithPoint(float x, float y);
    void addQuadraticSegment( const Path::Iterator& it, const float ax, const float ay);
    void addSegment( const Path::Iterator& it, const float ax = 0, const float ay = 0 );
    Rectangle<float> getRealPathBounds( const Path &p );

private:
    bool has_position = false;

};