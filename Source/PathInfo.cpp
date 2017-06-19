
#include "PathInfo.h"

void Sym_PathBounds::init()
{
    setSize(0, 0);
    has_position = false;
}

void Sym_PathBounds::adjustWithPoint(float x, float y)
{
    if( isEmpty() )
    {
        if( !has_position )
        {
            setPosition(x, y);
            has_position = true;
        }
        else
        {
            auto newX = jmin(x, getX());
            auto newY = jmin(y, getY());
            auto newW = jmax(x, getX()) - newX;
            auto newH = jmax(y, getY()) - newY;
            
            setBounds( newX, newY, newW, newH );
        }
    }
    else
    {
        auto newX = jmin(x, getX());
        auto newY = jmin(y, getY());
        auto newW = jmax(x, getRight()) - newX;
        auto newH = jmax(y, getBottom()) - newY;
        
        setBounds( newX, newY, newW, newH );
    }
}

void Sym_PathBounds::addQuadraticSegment( const Path::Iterator& it, const float ax, const float ay)
{
    
    adjustWithPoint(ax, ay);
    adjustWithPoint(it.x2, it.y2);
    
    auto a = Point<float>(ax, ay);
    auto b = Point<float>(it.x1, it.y1);
    auto c = Point<float>(it.x2, it.y2);
    
    auto pt = PathInfo::quadroots( a,b,c );
    if( pt.size() > 0 )
    {
        auto q_r = pt.back();
        auto x =  q_r.getX();
        auto y =  q_r.getY();
        
        if( x >= 0 && x <= 1)
        {
            auto pt = PathInfo::calcBezier(x, a, b, c );
            adjustWithPoint( pt.getX(), this->getY() );
        }
        
        if( y >= 0 && y <= 1)
        {
            auto pt = PathInfo::calcBezier(y, a, b, c );
            adjustWithPoint( this->getX(), pt.getY() );
        }
    }
}


void Sym_PathBounds::addSegment( const Path::Iterator& it, const float ax, const float ay)
{
    if ( it.elementType == it.startNewSubPath )
    {
        adjustWithPoint(it.x1, it.y1);
    }
    else if (it.elementType == it.lineTo)
    {
        adjustWithPoint(it.x1, it.y1);
    }
    else if (it.elementType == it.quadraticTo)
    {
        addQuadraticSegment(it, ax, ay);
    }
}

Rectangle<float> Sym_PathBounds::getRealPathBounds( const Path &p )
{
    init();
    
    float ax =0, ay = 0;
    Path::Iterator it( p );
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            addSegment( it );
            ax = it.x1;
            ay = it.y1;
        }
        else if (it.elementType == it.lineTo)
        {
            addSegment( it );
            ax = it.x1;
            ay = it.y1;
            
        }
        else if (it.elementType == it.quadraticTo)
        {
            addSegment(it, ax, ay);
            ax = it.x2;
            ay = it.y2;
        }
        else if (it.elementType == it.cubicTo)
        {
            // addSegment() for cubic here
            ax = it.x3;
            ay = it.y3;
        }
    }
    return *this;
}



/***************
 * Utility functions
 **************/

void PathInfo::accumPathBounds( Rectangle<float>& currentBounds, float x, float y )
{
    if (x < currentBounds.getX() )
        currentBounds.setX(x);
    else if ( x > currentBounds.getRight() )
        currentBounds.setRight(x);
        
    if (y < currentBounds.getY() )
        currentBounds.setY(y);
    else if ( y > currentBounds.getBottom() )
        currentBounds.setBottom(y);
}



Point<float> PathInfo::calcBezier( float t, Point<float> a, Point<float> b, Point<float> c  )
{
    auto t2 = t * t;
    auto mt = 1.0f-t;
    auto mt2 = mt * mt;
    return (a*mt2 + b*2.0f*mt*t + c*t2);
}

Point<float> PathInfo::calcBezier( float t, Point<float> a, Point<float> b, Point<float> c, Point<float> d  )
{
    auto t2 = t * t;
    auto t3 = t2 * t;
    auto mt = 1.0f-t;
    auto mt2 = mt * mt;
    auto mt3 = mt2 * mt;
    return a*mt3 + 3.0f*b*mt2*t + 3.0f*c*mt*t2 + d*t3;
}

/*
Point<float> pointAbsSqrt( Point<float> p )
{
    return Point<float>( sqrtf( fabs(p.getX()) ), sqrtf( fabs(p.getY()) ) );
}*/

// https://github.com/Pomax/bezierjs/blob/gh-pages/lib/utils.js#L377
std::vector<Point<float> > PathInfo::quadroots( Point<float> a, Point<float> b, Point<float> c )
{
    std::vector<Point<float> > roots;
    auto d = a - 2.0f*b + c;
    if(d.getX() != 0.0f && d.getY() != 0.0f)
    {
        roots.emplace_back( (a-b) / d );
    }
    return roots;
}

Rectangle<float> PathInfo::getBoundsQuadratic( float x1, float y1, float x2, float y2, float x3, float y3 )
{
    auto a = Point<float>(x1, y1);
    auto b = Point<float>(x2, y2);
    auto c = Point<float>(x3, y3);
    
    Rectangle<float> bounds;
    
    bounds.setPosition( jmin(x1, x3), jmin(y1, y3));
    bounds.setRight(    jmax(x1, x3) );
    bounds.setBottom(   jmax(y1, y3) );
    
    auto pt = PathInfo::quadroots( a,b,c );
    if( pt.size() > 0 )
    {
        auto q_r = pt.back();
        auto x =  q_r.getX();
        auto y =  q_r.getY();
        
        if( x >= 0 && x <= 1)
        {
            auto check = PathInfo::calcBezier(x, a, b, c );
            if( check.getX() < bounds.getX() )
                bounds.setLeft( check.getX() );
            else if ( check.getX() > bounds.getRight() )
                bounds.setRight( check.getX() );
        }
        
        if( y >= 0 && y <= 1)
        {
            auto check = PathInfo::calcBezier(y, a, b, c );
            if( check.getY() < bounds.getY() )
                bounds.setTop( check.getY() );
            else if ( check.getY() > bounds.getBottom() )
                bounds.setBottom( check.getY() );
        }
    }
    return bounds;
}

/*
 // Source: how-to-calculate-bezier-curves-bounding.html
 // Original version: NISHIO Hirokazu
 // Modifications: Timo
 Rectangle<float> getBoundsOfCurve(float x0, float y0, float x1, float y1, float x2, float y2, float x3, float y3)
 //Rectangle<float> getBoundsOfCurve( Point<float> p0, Point<float> p1, Point<float> p2, Point<float> p3 )
 {
 
 vector<float> x_pts;
 vector<float> y_pts;
 vector<float> tvalues;
 
 x_pts.emplace_back( x0 );
 y_pts.emplace_back( y0 );
 x_pts.emplace_back( x3 );
 y_pts.emplace_back( y3 );
 
 
 float a, b, c, t, t1, t2, b2ac, sqrtb2ac;
 for (int i = 0; i < 2; ++i)
 {
 if (i == 0)
 {
 b = 6 * x0 - 12 * x1 + 6 * x2;
 a = -3 * x0 + 9 * x1 - 9 * x2 + 3 * x3;
 c = 3 * x1 - 3 * x0;
 }
 else
 {
 b = 6 * y0 - 12 * y1 + 6 * y2;
 a = -3 * y0 + 9 * y1 - 9 * y2 + 3 * y3;
 c = 3 * y1 - 3 * y0;
 }
 
 if (abs(a) < 1e-12) // Numerical robustness
 {
 if (abs(b) < 1e-12) // Numerical robustness
 {
 continue;
 }
 t = -c / b;
 if (0 < t && t < 1)
 {
 tvalues.emplace_back(t);
 }
 continue;
 }
 
 b2ac = b * b - 4 * c * a;
 sqrtb2ac = sqrt(b2ac);
 if (b2ac < 0)
 {
 continue;
 }
 
 t1 = (-b + sqrtb2ac) / (2 * a);
 if (0 < t1 && t1 < 1)
 {
 tvalues.emplace_back(t1);
 }
 
 t2 = (-b - sqrtb2ac) / (2 * a);
 if (0 < t2 && t2 < 1)
 {
 tvalues.emplace_back(t2);
 }
 
 }
 
 float x, y, mt;
 auto j = tvalues.size();
 
 while (j--)
 {
 t = tvalues[j];
 mt = 1 - t;
 x = (mt * mt * mt * x0) + (3 * mt * mt * t * x1) + (3 * mt * t * t * x2) + (t * t * t * x3);
 x_pts.emplace_back( x );
 
 
 y = (mt * mt * mt * y0) + (3 * mt * mt * t * y1) + (3 * mt * t * t * y2) + (t * t * t * y3);
 y_pts.emplace_back( y );
 }
 
 tvalues.emplace_back(0);
 tvalues.emplace_back(1);
 
 
 auto xminmax = minmax( x_pts.begin(), x_pts.end() );
 auto yminmax = minmax( y_pts.begin(), y_pts.end() );
 
 return Rectangle<float>(*xminmax.first, *yminmax.first, *xminmax.second, *yminmax.second );
 
 }
 
 Rectangle<float> getBoundsOfCurve(float x0, float y0, float x1, float y1, float x2, float y2 )
 {
 
 float xt = ((x0-x1) / (x0 - 2 * x1 + x3));
 float yt = ((y0-y1) / (y0 - 2 * y1 + y3));
 
 
 }
 */