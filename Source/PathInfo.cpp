
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


void Sym_PathBounds::addCubicSegment( const Path::Iterator& it, const float ax, const float ay)
{
    
    adjustWithPoint(ax, ay);
    adjustWithPoint(it.x3, it.y3);
    
    auto p1 = Point<float>(ax, ay);
    auto p2 = Point<float>(it.x1, it.y1);
    auto p3 = Point<float>(it.x2, it.y2);
    auto p4 = Point<float>(it.x3, it.y3);
    
    auto rootvec = PathInfo::getCubicRoots( p1, p2, p3, p4 );
    
    for( auto r : rootvec )
    {
        auto x =  r.getX();
        auto y =  r.getY();
        
        if( x >= 0 && x <= 1)
        {
            auto pt = PathInfo::calcBezier(x, p1, p2, p3, p4);
            adjustWithPoint( pt.getX(), this->getY() );
        }
        
        if( y >= 0 && y <= 1)
        {
            auto pt = PathInfo::calcBezier(y, p1, p2, p3, p4);
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
    else if( it.elementType == it.cubicTo )
    {
        addCubicSegment( it, ax, ay );
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
            addSegment(it, ax, ay);
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

Point<float> ptsqrt( Point<float> p )
{
    return Point<float>( sqrt(p.getX()), sqrt(p.getY()) );
}

std::vector<Point<float>> PathInfo::getCubicRoots(Point<float> p1, Point<float> p2, Point<float> p3, Point<float> p4)
{
    std::vector<Point<float> > roots;
    
    Point<float> v1 = 3.0f * (p2 - p1) ;
    Point<float> v2 = 3.0f * (p3 - p2) ;
    Point<float> v3 = 3.0f * (p4 - p3) ;
    
    Point<float> a = v1 - 2.0f * v2 + v3 ;
    Point<float> b = 2.0f * (v2 - v1) ;
    Point<float> c = v1 ;


    if ( fabs(a.getX()) < 1e-12 || fabs(a.getY()) < 1e-12 )
    {
        if ( fabs(b.getX()) < 1e-12 || fabs(b.getY()) < 1e-12 )
        {
            return roots; // no roots
        }
        
        auto t = -c / b;
        
        if ( (0 < t.getX() && t.getX() < 1) || (0 < t.getY() && t.getY() < 1) )
        {
            roots.emplace_back(t);
        }
        
        return roots;
    }
    
    auto twoa = a * 2.0f;
    auto b2 = b*b;
    
    auto t1 = (-b + ptsqrt( b2 - 4.0f*a*c )) / twoa;
    
    if ( (0 < t1.getX() && t1.getX() < 1) || (0 < t1.getY() && t1.getY() < 1) )
    {
        roots.emplace_back(t1);
    }

    auto t2 = (-b - ptsqrt( b2 - 4.0f*a*c )) / twoa;

    if ( (0 < t2.getX() && t2.getX() < 1) || (0 < t2.getY() && t2.getY() < 1) )
    {
        roots.emplace_back(t2);
    }
    
    return roots;

}