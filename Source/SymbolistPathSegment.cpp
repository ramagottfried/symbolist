
#include "SymbolistPathSegment.hpp"

/*******  SymbolistFlatPath ******/

SymbolistFlatPath::SymbolistFlatPath(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c)
{
    m_type = 1;
    m_curve3.init(a.x, a.y, b.x, b.y, c.x, c.y);
}

SymbolistFlatPath::SymbolistFlatPath(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c,  SymbolistPoint d)
{
    m_type = 2;
    m_curve4.init(a.x, a.y, b.x, b.y, c.x, c.y, d.x, d.y);
}

vector<agg::point_d> SymbolistFlatPath::getPoints()
{
    if( m_type == 1 )
        return m_curve3.getPoints();
    else if( m_type == 2 )
        return m_curve4.getPoints();
    else
        return vector<agg::point_d>();
}

double SymbolistFlatPath::getLength()
{
    SymbolistPoint pt, prev;
    bool first = true;
    double length = 0;
    for( auto& p : getPoints() )
    {
        if( first )
        {
            prev = SymbolistPoint(p.x, p.y);
            first = false;
        }
        else
        {
            pt = SymbolistPoint(p.x, p.y);
            length += pt.getDistanceFrom(prev);
            prev = pt;
        }
    }
    return length;
}


/*******  SymbolistPathSegment ******/

SymbolistPathSegment::SymbolistPathSegment(SymbolistPoint a, SymbolistPoint b)
{
    type = 'L';
    pts.emplace_back(a);
    pts.emplace_back(b);
    length = a.getDistanceFrom(b);
}

SymbolistPathSegment::SymbolistPathSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c)
{
    type = 'Q';
    pts.emplace_back(a);
    pts.emplace_back(b);
    pts.emplace_back(c);
    
    m_flat = SymbolistFlatPath(a,b,c);
    length = m_flat.getLength();
}

SymbolistPathSegment::SymbolistPathSegment(SymbolistPoint a, SymbolistPoint b, SymbolistPoint c,  SymbolistPoint d)
{
    type = 'C';
    pts.emplace_back(a);
    pts.emplace_back(b);
    pts.emplace_back(c);
    pts.emplace_back(d);
    
    m_flat = SymbolistFlatPath(a,b,c,d);
    length = m_flat.getLength();
}
