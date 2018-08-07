
#include "SymbolistPath.hpp"

void SymbolistPath::clear()
{
    m_bounds = SymbolistRect();
    m_path.clear();
}

void SymbolistPath::addSegment(SymbolistPoint a, SymbolistPoint b)
{
    m_path.emplace_back( SymbolistPathSegment(a, b) );
    m_length += m_path.back().length;
    adjustBounds(a,b);
}

void SymbolistPath::addSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c)
{
    m_path.emplace_back( SymbolistPathSegment(a, b, c) );
    m_length += m_path.back().length;
    adjustBounds(a, b, c);
}

void SymbolistPath::addSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c, SymbolistPoint d)
{
    m_path.emplace_back( SymbolistPathSegment(a, b, c, d) );
    m_length += m_path.back().length;
    adjustBounds(a, b, c, d);
}

void SymbolistPath::print()
{
    int count = 0;
    for( auto s : m_path )
    {
        cout << count++ << " type " << s.type << endl;
        int ptcount = 0;
        for( auto p : s.pts )
        {
            cout << "\t" << ptcount << " " << p.x << " " << p.y << endl;
        }
    }
}


/*
 *  line bounds
 */
void SymbolistPath::adjustBounds(SymbolistPoint a, SymbolistPoint b)
{
    m_bounds.expandToFit(a.x, a.y);
    m_bounds.expandToFit(b.x, b.y);
}

/*
 *  quadratic bounds
 */
void SymbolistPath::adjustBounds(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c)
{
    m_bounds.expandToFit(a.x, a.y);
    m_bounds.expandToFit(c.x, c.y);
    
    auto pt = quadroots( a,b,c );
    if( pt.size() > 0 )
    {
        auto q_r = pt.back();
        auto x =  q_r.getX();
        auto y =  q_r.getY();
        
        if( x >= 0 && x <= 1)
        {
            auto pt = calcBezier(x, a, b, c );
            m_bounds.expandToFit( pt.getX(), m_bounds.getY() );
        }
        
        if( y >= 0 && y <= 1)
        {
            auto pt = calcBezier(y, a, b, c );
            m_bounds.expandToFit( m_bounds.getX(), pt.getY() );
        }
    }
}

/*
 *  cubic bounds
 */
void SymbolistPath::adjustBounds(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c, SymbolistPoint d)
{
    m_bounds.expandToFit(a.x, a.y);
    m_bounds.expandToFit(d.x, d.y);
    
    auto rootvec = getCubicRoots( a, b, c, d );
    
    for( auto r : rootvec )
    {
        auto x =  r.getX();
        auto y =  r.getY();
        
        if( x >= 0 && x <= 1)
        {
            auto pt = SymbolistPath::calcBezier(x, a, b, c, d);
            m_bounds.expandToFit( pt.getX(), m_bounds.getY() );
        }
        
        if( y >= 0 && y <= 1)
        {
            auto pt = SymbolistPath::calcBezier(y, a, b, c, d);
            m_bounds.expandToFit( m_bounds.getX(), pt.getY() );
        }
    }
}


// JUCE conversion -- this will be in the View
/*
SymbolistRect SymbolistPath::getRealPathBounds( const Path &p )
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

*/



SymbolistPoint SymbolistPath::calcBezier( double t, SymbolistPoint a, SymbolistPoint b, SymbolistPoint c  )
{
    auto t2 = t * t;
    auto mt = 1.0f-t;
    auto mt2 = mt * mt;
    return (a*mt2 + b*2.0f*mt*t + c*t2);
}

SymbolistPoint SymbolistPath::calcBezier( double t, SymbolistPoint a, SymbolistPoint b, SymbolistPoint c, SymbolistPoint d  )
{
    auto t2 = t * t;
    auto t3 = t2 * t;
    auto mt = 1.0-t;
    auto mt2 = mt * mt;
    auto mt3 = mt2 * mt;
    return a*mt3 + b*3.0*mt2*t + c*3.0*mt*t2 + d*t3;
}


vector<SymbolistPoint> SymbolistPath::quadroots( SymbolistPoint a, SymbolistPoint b, SymbolistPoint c )
{
    vector<SymbolistPoint > roots;
    auto d = a - b*2.0 + c;
    if(d.getX() != 0.0 && d.getY() != 0.0)
    {
        roots.emplace_back( (a-b) / d );
    }
    return roots;
}

SymbolistRect SymbolistPath::getBoundsQuadratic( SymbolistPoint a, SymbolistPoint b, SymbolistPoint c )
{

    SymbolistRect bounds;
    
    bounds.setPosition( min(a.x, c.x), min(a.y, c.y) );
    bounds.setRight(    max(a.x, c.x) );
    bounds.setBottom(   max(a.y, c.y) );
    
    auto pt = SymbolistPath::quadroots( a,b,c );
    if( pt.size() > 0 )
    {
        auto q_r = pt.back();
        auto x =  q_r.getX();
        auto y =  q_r.getY();
        
        if( x >= 0 && x <= 1)
        {
            auto check = SymbolistPath::calcBezier(x, a, b, c );
            if( check.getX() < bounds.getX() )
                bounds.setLeft( check.getX() );
            else if ( check.getX() > bounds.getRight() )
                bounds.setRight( check.getX() );
        }
        
        if( y >= 0 && y <= 1)
        {
            auto check = SymbolistPath::calcBezier(y, a, b, c );
            if( check.getY() < bounds.getY() )
                bounds.setTop( check.getY() );
            else if ( check.getY() > bounds.getBottom() )
                bounds.setBottom( check.getY() );
        }
    }
    return bounds;
}

SymbolistPoint ptsqrt( SymbolistPoint p )
{
    return SymbolistPoint( sqrt(p.getX()), sqrt(p.getY()) );
}

vector<SymbolistPoint> SymbolistPath::getCubicRoots(SymbolistPoint p1, SymbolistPoint p2, SymbolistPoint p3, SymbolistPoint p4)
{
    vector<SymbolistPoint > roots;
    
    SymbolistPoint v1 = (p2 - p1) * 3.0;
    SymbolistPoint v2 = (p3 - p2) * 3.0;
    SymbolistPoint v3 = (p4 - p3) * 3.0;
    
    SymbolistPoint a = v1 - v2*2.0 + v3 ;
    SymbolistPoint b = (v2 - v1) * 2.0 ;
    SymbolistPoint c = v1 ;
    
    
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
    
    auto t1 = (-b + ptsqrt( b2 - a*c*4.0 )) / twoa;
    
    if ( (0 < t1.getX() && t1.getX() < 1) || (0 < t1.getY() && t1.getY() < 1) )
    {
        roots.emplace_back(t1);
    }
    
    auto t2 = (-b - ptsqrt( b2 - a*c*4.0 )) / twoa;
    
    if ( (0 < t2.getX() && t2.getX() < 1) || (0 < t2.getY() && t2.getY() < 1) )
    {
        roots.emplace_back(t2);
    }
    
    return roots;
    
}

void SymbolistPath::fromSVG(const string& svg_path)
{
    SymbolistPoint startPt;
    std::size_t prev = 0, pos;
    string seg;
    char type = '\0', nexttype = '\0';
    
    while ( (pos = svg_path.find_first_of("MmLlQqCcZ", prev) ) != std::string::npos )
    {
        nexttype = svg_path[pos];
        
        if (pos > prev)
        {
            seg = svg_path.substr(prev, pos-prev);
            
            auto pts = parseSegment(seg, type, startPt);
            if( !pts.size() )
            {
                return;
            }
            
            startPt = pts.back();
            
        }
        prev = pos+1;
        type = nexttype;
    }
    
    if (prev < svg_path.length())
    {
        seg = svg_path.substr(prev, std::string::npos);
        auto pts = parseSegment(seg, type, startPt);
        if( !pts.size() )
        {
            return;
        }
    }
    
}


vector<SymbolistPoint> SymbolistPath::parseSegment(string& seg, const char type, SymbolistPoint startPt)
{
    size_t prevnumpos = 0, numpos;
    
    vector<SymbolistPoint> pts;
    double x = 0;
    int count = 0;
    
    while ( (numpos = seg.find_first_of(", ", prevnumpos) ) != std::string::npos )
    {
        if( count % 2 == 0 )
        {
            x = stod( seg.substr(prevnumpos, numpos-prevnumpos));
        }
        else
        {
            pts.emplace_back(SymbolistPoint(x, stod( seg.substr(prevnumpos, numpos-prevnumpos) ) ) );
        }
        count++;
        prevnumpos = numpos+1;
    }
    
    if (prevnumpos < seg.length() )
    {
        if( count % 2 == 0 )
        {
            cout << "error: uneven number of points" << endl;
            return vector<SymbolistPoint>();
        }
        
        pts.emplace_back(SymbolistPoint(x, stod( seg.substr(prevnumpos, numpos-prevnumpos) ) ) );
    }
    
    switch (type) {
        case 'M':
        case 'm':
            return pts;
            break;
        case 'L':
            if( pts.size() == 1 )
            {
                addSegment(startPt, pts[0]);
            }
            else
                cout << "L parse error: wrong number of points" << endl;
            break;
        case 'l':
            if( pts.size() == 2 )
            {
                addSegment(startPt, startPt+pts[0]);
            }
            else
                cout << "l parse error: wrong number of points" << endl;
            break;
            
        case 'Q':
            if( pts.size() == 2 )
            {
                addSegment(startPt, pts[0], pts[1]);
            }
            else
                cout << "Q parse error: wrong number of points" << endl;
            break;
        case 'q':
            if( pts.size() == 2 )
            {
                addSegment(startPt, startPt+pts[0], startPt+pts[1]);
            }
            else
                cout << "q parse error: wrong number of points" << endl;
            break;
            
        case 'C':
            if( pts.size() == 3 )
            {
                addSegment(startPt, pts[0], pts[1], pts[2]);
            }
            else
                cout << "C parse error: wrong number of points" << endl;
            break;
        case 'c':
            if( pts.size() == 3 )
            {
                addSegment(startPt, startPt+pts[0], startPt+pts[1], startPt+pts[2]);
            }
            else
                cout << "c parse error: wrong number of points" << endl;
            break;
        default:
            break;
    }
    
    cout << type << " " << pts.size() << endl;
    for( auto p : pts )
    {
        cout << "x " << p.getX() << " y " << p.getY() << endl;
    }
    cout << "--" << endl;
    
    return pts;
}



/*
double SymbolistPath::getLineLength(double x1, double y1, double x2, double y2)
{
    double x = x2 - x1;
    double y = y2 - y1;
    return (sqrt((x*x)+(y*y)));
}

void SymbolistPath::deCasteljau( double t, const double *p, int pLen, double *pt)
{
    // de Casteljau recursive midpoint method
    int i;
    int qLen = pLen-2;
    double q[ qLen ];
    for( i = 0; i < qLen; i++)
    {
        q[i] = p[i] + t * (p[i+2] - p[i]);
    }
    if( qLen == 2 )
    {
        memcpy(pt, q, sizeof(double) * 2);
    }
    else
        deCasteljau(t, q, qLen, pt);
    
}

double SymbolistPath::cubicLength(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4)
{
    double pts[] = { x1, y1, x2, y2, x3, y3, x4, y4 };
    
    double midPt[2];
    double prevMidPt[2] = { pts[0], pts[1] };
    
    double t = 0;
    int steps = 1000;
    double inc = 1.0 / steps;
    double len = 0;
    for (int i = 0; i <= steps; i++)
    {
        deCasteljau( t, pts, 8, midPt );
        
        if (i > 0)
        {
            len += getLineLength(midPt[0], midPt[1], prevMidPt[0], prevMidPt[1]);
        }
        memcpy(prevMidPt, midPt, sizeof(double) * 2);
        memset(midPt, 0, sizeof(double) * 2);
        
        t += inc;
    }
    return len;
}
*/
