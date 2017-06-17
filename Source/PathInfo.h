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