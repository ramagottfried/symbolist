
#include "PathBaseComponent.h"
#include "SymbolistMainComponent.h"
#include "PageComponent.h"
#include "PathHandleComponent.h"


PathBaseComponent::~PathBaseComponent()
{
    //printf("freeing path %p\n", this);
    cleanupPathArray();
    removeHandles();
}

void PathBaseComponent::cleanupPathArray()
{
    for ( int np = 0; np < m_path_array.size(); np++ ) delete m_path_array[np] ;
    m_path_array.clear();
}


void PathBaseComponent::printPath( Path p, const char* name )
{
    Path::Iterator it(p);
    int count = 0;
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            std::cout << name << " " << count++ << " " << "start point " << it.x1 << " " << it.y1 << "\n";
        }
        else if (it.elementType == it.lineTo)
        {
            std::cout << name << " " << count++ << " " << "line to " << it.x1 << " " << it.y1 << "\n";
        }
        else if (it.elementType == it.quadraticTo)
        {
            std::cout << name << " " << count++ << " " << "quadratic to " << it.x1 << " " << it.y1 << " " << it.x2 << " " << it.y2 << "\n";
        }
        else if (it.elementType == it.cubicTo)
        {
            std::cout << name << " " << count++ << " " << "cubic to " << it.x1 << " " << it.y1 << " " << it.x2 << " " << it.y2 << " " << it.x3 << " " << it.y3 << "\n";
        }
        else if (it.elementType == it.closePath)
        {
            std::cout << name << " " << count++ << " " << "close path\n";
        }
    }
}


Path PathBaseComponent::mergePathArray()
{
    Path p;
    for (int np = 0 ; np < m_path_array.size(); np++ )
    {
        p.addPath(*(m_path_array[np]));
    }
    return p;
}

void PathBaseComponent::makePathArrayFromPath(const Path &p)
{
    Path::Iterator it( p );
    cleanupPathArray();
    
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            Path *newpath = new Path();
            newpath->startNewSubPath(it.x1,it.y1);
            m_path_array.add(newpath);
        }
        else if (it.elementType == it.lineTo)
        {
            m_path_array.getLast()->lineTo(it.x1, it.y1);
        }
        else if (it.elementType == it.quadraticTo)
        {
            m_path_array.getLast()->quadraticTo(it.x1, it.y1, it.x2, it.y2);
        }
        else if (it.elementType == it.cubicTo)
        {
            m_path_array.getLast()->cubicTo(it.x1, it.y1, it.x2, it.y2, it.x3, it.y3);
        }
        else if( it.elementType == it.closePath )
        {
            m_path_array.getLast()->closeSubPath();
        }
    }
}

void PathBaseComponent::resizeToFit(int x, int y, int w, int h)
{
    // input bounds are for the visible path, not the handles...
    Rectangle<int> r = Rectangle<int>(x,y,w,h).reduced( strokeType.getStrokeThickness() );
    
    if( r.getWidth() > 0 && r.getHeight() > 0 && m_path_bounds.getWidth() > 0 && m_path_bounds.getHeight() > 0)
    {
        
        float w_scale = (float)r.getWidth() / m_path_bounds.getWidth();
        float h_scale = (float)r.getHeight() / m_path_bounds.getHeight();

        Path temp = mergePathArray();
        
        Rectangle<float> tmp_bounds = m_path_bounds.getRealPathBounds( temp ).expanded( strokeType.getStrokeThickness() );
        Point<float> newpos = tmp_bounds.getPosition();
        
        temp.applyTransform( AffineTransform().scale(w_scale, h_scale).translated( x - newpos.x, y - newpos.y ) );

        makePathArrayFromPath(temp);
        updateHandlePositions();
        updatePathBounds();
    }
}

// Juce callback
void PathBaseComponent::resized()
{
    BaseComponent::resized();
    
    if( ! in_edit_mode )
    {
        resizeToFit(getLocalBounds().getX(),
                    getLocalBounds().getY(),
                    getLocalBounds().getWidth(),
                    getLocalBounds().getHeight());
    }
}


bool PathBaseComponent::intersectRect( Rectangle<int> rect)
{
    Sym_PathBounds pb;
    
    Rectangle<int> shifted_rect = rect.translated( - getBoundsInParent().getX(), - getBoundsInParent().getY() );
    
    for ( int np = 0; np < m_path_array.size(); np++)
    {
        Path* p = m_path_array[np];
        
        Rectangle<float> r_path = pb.getRealPathBounds( *p );

        if ( (  r_path.getX() >= shifted_rect.getX() && r_path.getY() >= shifted_rect.getY()     &&
                r_path.getRight() <= shifted_rect.getRight() && r_path.getBottom() <= shifted_rect.getBottom() )
            // => he path is _inside_ the rect
            ||
            (   p->intersectsLine( Line<float>( shifted_rect.getX() , shifted_rect.getY() , shifted_rect.getRight() , shifted_rect.getY())) ||
                p->intersectsLine( Line<float>( shifted_rect.getRight() , shifted_rect.getY() , shifted_rect.getRight() , shifted_rect.getBottom())) ||
                p->intersectsLine( Line<float>( shifted_rect.getX() , shifted_rect.getBottom() , shifted_rect.getRight() , shifted_rect.getBottom())) ||
                p->intersectsLine( Line<float>( shifted_rect.getX() , shifted_rect.getY() , shifted_rect.getX() , shifted_rect.getBottom())) )
            // => the path intersects one of the vertices of the rect
            )
            return true;
    }
    return false; // no match
}



/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

int PathBaseComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = 0 ;

    messages_added += BaseComponent::addSymbolMessages( s, base_address );
    
    int n_subpaths = m_path_array.size();
    
    s->addOSCMessage( OSCMessage( base_address + "/num_sub_paths", (int)n_subpaths ));
    messages_added += 1;
    
    for ( int np = 0; np < n_subpaths ; np++ )
    {
        Path *p = m_path_array[np];
        s->addOSCMessage( OSCMessage( String(base_address) + String("/path/") + String(np) + "/str",    p->toString())  );
        s->addOSCMessage( OSCMessage( String(base_address) + String("/path/") + String(np) + "/length", p->getLength()) );

        auto sub_bounds = Sym_PathBounds( *p );
        s->addOSCMessage( OSCMessage( String(base_address) + String("/path/") + String(np) + "/time/start",    s->pixelsToTime( sub_bounds.getX() ) ) );
        s->addOSCMessage( OSCMessage( String(base_address) + String("/path/") + String(np) + "/time/duration", s->pixelsToTime( sub_bounds.getWidth() ) ) );
        
        s->addOSCMessage ((String(base_address) += "/fill") ,   m_fill   );

        s->addOSCMessage ((String(base_address) += "/stroke/thickness") ,   strokeType.getStrokeThickness()  );
        
        messages_added += 6 ;
    }
    
    return messages_added;
}

/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/

void PathBaseComponent::importFromSymbol(const Symbol &s)
{
    //cout << "PathBaseComponent::importFromSymbol" << endl;
    BaseComponent::importFromSymbol(s);

    cleanupPathArray();
    
    int n_subpaths = s.getOSCMessageValue("/num_sub_paths").getInt32();
    
    for ( int np = 0; np < n_subpaths ; np++ )
    {
        Path* subp = new Path();
        String path_str = s.getOSCMessageValue("/path/" + String(np) + "/str").getString();
        subp->restoreFromString( path_str );
        m_path_array.add(subp);
    }

    int pos = s.getOSCMessagePos("/fill");
    if( pos != -1  )
        m_fill = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(pos) );
    
    pos = s.getOSCMessagePos("/stroke/thickness");
    if( pos != -1  )
        strokeType.setStrokeThickness( Symbol::getOSCValueAsFloat( s.getOSCMessageValue(pos) ) );
    
    
    
    updatePathBounds();
}

/******************
 * MODES
 *****************/


void PathBaseComponent::updatePathBounds ()
{
    /*
    float minx, miny = MAXFLOAT;
    float maxx, maxy = 0.0;
    for( int np = 0; np < m_path_array.size(); np++ )
    {
        Rectangle<float> pb = m_path_array[np].getBounds();
        minx =  min( minx, pb.getX() );
        miny =  min( miny, pb.getY() );
        maxx =  max( maxx, pb.getRight() );
        maxy =  max( maxy, pb.getBottom() );
    }
    m_path_bounds.setBounds(minx,miny, maxx-minx, maxy-miny);
    */
    
    Path temp = mergePathArray();
    
    m_path_bounds.getRealPathBounds( temp );
    m_path_origin = m_path_bounds.getPosition();
    m_path_centroid = m_path_bounds.getCentre();
}

void PathBaseComponent::setMinimalBounds ()
{
    //float strokeOffset = strokeType.getStrokeThickness() * 0.5;
    Path m_path = mergePathArray();
    
    m_path_bounds.getRealPathBounds(m_path);
    
    Rectangle<float> symbol_bounds = m_path_bounds.expanded( strokeType.getStrokeThickness() );
    m_path.applyTransform(AffineTransform::translation(- symbol_bounds.getX(), - symbol_bounds.getY() ));
    
    makePathArrayFromPath(m_path);
    
    setBounds( 0, 0, symbol_bounds.getWidth(), symbol_bounds.getHeight() );
    setTopLeftPosition(symbol_bounds.getX(), symbol_bounds.getY());
    
    updatePathBounds();
}

void PathBaseComponent::setMaximalBounds ()
{
    Point<int> init_pos =  positionRelativeTo(getPageComponent());
    BaseComponent::setMaximalBounds();
    Path m_path = mergePathArray();
    m_path.applyTransform(AffineTransform::translation(init_pos));
    makePathArrayFromPath(m_path);
    updatePathBounds();
    
    makeHandlesFromPath();
}

void PathBaseComponent::setEditMode( bool val )
{
    in_edit_mode = val;
    if ( val == false ) // = exit
    {
        ScoreComponent* sc = ((ScoreComponent*)getParentComponent());
        sc->addToSelection(this);
        
        removeHandles();
        
        if ( m_path_array.isEmpty() )
            sc->deleteSelectedComponents();
    }
}


void PathBaseComponent::unselectAllComponents()
{
    ScoreComponent::unselectAllComponents();
    if ( rotation_handle != NULL ) removeFromSelection(rotation_handle);
}

/******************
 * HANDLES (path edit)
 *****************/

void PathBaseComponent::addHandle( PathHandle::handleType type, float x, float y )
{
    PathHandle *h = new PathHandle( type, x + getX(), y + getY() );
    addSubcomponent(h);
    path_handles.add( h );
}

void PathBaseComponent::addHandlesTo( Point<float> p, PathHandle* last )
{
    PathHandle* h2 = new PathHandle( PathHandle::anchor, p.x , p.y );
    float minx = min(p.x,(float)last->getCenter().x);
    float maxx = max(p.x,(float)last->getCenter().x);
    float miny = min(p.y,(float)last->getCenter().y);
    float maxy = max(p.y,(float)last->getCenter().y);
    PathHandle* h1 = new PathHandle( PathHandle::quadratic_control, (minx+((maxx-minx)*0.5)) , (miny+((maxy-miny)*0.5)) );
    addSubcomponent(h1);
    addSubcomponent(h2);
    path_handles.add(h1);
    path_handles.add(h2);
}

void PathBaseComponent::insertHandleBefore( PathHandle* target )
{
    int position = -1;
    for (int i = 0; i < path_handles.size(); i++) if (path_handles[i]==target) position = i;
    
    if (position >= 1)
    {
        PathHandle* previous = path_handles[position-1];
        float minx = min(previous->getCenter().x, target->getCenter().x);
        float maxx = max(previous->getCenter().x, target->getCenter().x);
        float miny = min(previous->getCenter().y, target->getCenter().y);
        float maxy = max(previous->getCenter().y, target->getCenter().y);
        PathHandle* h = new PathHandle( PathHandle::quadratic_control, (minx+((maxx-minx)*0.5)) , (miny+((maxy-miny)*0.5)) );
        addSubcomponent(h);
        path_handles.insert(position, h);
    }
}


void PathBaseComponent::makeHandlesFromPath()
{
    
    if( path_handles.size() == 0 ) // should always be the case when we call this
    {
        for ( int np = 0; np < m_path_array.size(); np++ )
        {
            Path* m_path = m_path_array[np];
            Path::Iterator it( *m_path );
            
            while( it.next() )
            {
                if (it.elementType == it.startNewSubPath)
                {
                    if ( !path_handles.isEmpty() )
                    {
                        path_handles.getLast()->setEnd(true);
                    }
                    addHandle( PathHandle::start, it.x1, it.y1 );
                }
                else if (it.elementType == it.lineTo)
                {
                    addHandle( PathHandle::anchor, it.x1, it.y1 );
                    
                }
                else if (it.elementType == it.quadraticTo)
                {
                    addHandle( PathHandle::quadratic_control, it.x1, it.y1 );
                    addHandle( PathHandle::anchor, it.x2, it.y2 );
                }
                else if (it.elementType == it.cubicTo)
                {
                    addHandle( PathHandle::cubic_control, it.x1, it.y1 );
                    addHandle( PathHandle::cubic_control, it.x2, it.y2 );
                    addHandle( PathHandle::anchor, it.x3, it.y3 );
                }
                else if( it.elementType == it.closePath )
                {
                    path_handles.getLast()->setEnd(true);
                    path_handles.getLast()->setClosing(true);
                }
                
            }
        }
        updatePathBounds();
        
        float length = max( m_path_bounds.getHeight(), m_path_bounds.getWidth() ) * 0.5 + 5;
        rotation_handle = new PathHandle( PathHandle::rotate, m_path_centroid.getX(), m_path_centroid.getY() + length );
        addAndMakeVisible( rotation_handle );
    }
}


void PathBaseComponent::removeSubcomponent(SymbolistComponent* h)
{
    for( int i = 0; i < path_handles.size(); i++ )
    {
        if( h == path_handles[i] ) path_handles.remove(i);
    }
    
    ScoreComponent::removeSubcomponent(h);
    updatePathPoints();
    updatePathBounds();
    repaint();
}


void PathBaseComponent::removeHandles()
{
    SymbolistComponent::clearAllSubcomponents();
    path_handles.clear();
    
    if ( getMainComponent() != NULL && rotation_handle != NULL )
    {
        delete rotation_handle;
    }
}




/***********************
 * Update handles after transformations on the path
 * We must assume that the _structure_ of the path is consistent with the existing handles.
 ***********************/

void PathBaseComponent::updateHandlePositions()
{
    if ( in_edit_mode && !path_handles.isEmpty() ) // in principle path_handle is not empty if we're in edit mode..
    {
        int n = 0;
        
        for ( int np = 0; np < m_path_array.size(); np++ )
        {
            Path* m_path = m_path_array[np];
            Path::Iterator it( *m_path );
            while( it.next() )
            {
                if (it.elementType == it.startNewSubPath)
                {
                    // if (n > 0 ) path_handles[n+1]->setEnd(true);
                    path_handles[n++]->setCentrePosition(it.x1, it.y1);
                }
                else if (it.elementType == it.lineTo)
                {
                    path_handles[n++]->setCentrePosition(it.x1, it.y1);
                }
                else if (it.elementType == it.quadraticTo)
                {
                    path_handles[n++]->setCentrePosition(it.x1, it.y1);
                    path_handles[n++]->setCentrePosition(it.x2, it.y2);
                }
                else if (it.elementType == it.cubicTo)
                {
                    path_handles[n++]->setCentrePosition(it.x1, it.y1);
                    path_handles[n++]->setCentrePosition(it.x2, it.y2);
                    path_handles[n++]->setCentrePosition(it.x3, it.y3);
                }
                else if( it.elementType == it.closePath )
                {
                    //path_handles[n]->setEnd(true);
                    //path_handles[n]->setClosing(true);
                }
            }
        }
    }
}



/***********************
 * Update path after user action on handles
 * also fixes the handle sequence
 ************************/
void PathBaseComponent::updatePathPoints()
{
    //Path p;
    cleanupPathArray();
    
    std::vector<PathHandle*> remove;
    
    for( int h = 0 ; h < path_handles.size() ; h++ )
    {
        PathHandle* current_handle = path_handles[h];
        
        // normal start/anchor points
        if ( current_handle->getHandleType() == PathHandle::start )
        {
            m_path_array.add(new Path());
            m_path_array.getLast()->startNewSubPath( current_handle->getBounds().toFloat().getCentre() );
        }
        else if ( current_handle->getHandleType() == PathHandle::anchor )
        {
            // deal with the case where the starting point has been erased
            if ( m_path_array.isEmpty() )
            {
                m_path_array.add(new Path());
                current_handle->setHandleType(PathHandle::start);
                m_path_array.getLast()->startNewSubPath( current_handle->getBounds().toFloat().getCentre() );
            }
            else // normal case
            {
            m_path_array.getLast()->lineTo( current_handle->getBounds().toFloat().getCentre() );
            }
        }
        
        // quadratic
        else if ( current_handle->getHandleType() == PathHandle::quadratic_control )
        {
            
            if ( ( h >= path_handles.size()-1 ) || ( path_handles[h+1]->getHandleType() == PathHandle::start ) ) // we're at the end: not normal !
            {
                //path_handles[h]->setHandleType(PathHandle::anchor); // fix the type
                //h-=1; // back on the same point next iteration (then exit)
                //SymbolistComponent::removeSubcomponent(current_handle);
                //delete current_handle;
                remove.emplace_back(current_handle);
            }
            else
            {
                PathHandle* next_handle = path_handles[h+1];
                
                if ( next_handle->getHandleType() == PathHandle::anchor) // everything ok
                {
                    m_path_array.getLast()->quadraticTo(current_handle->getBounds().toFloat().getCentre() ,
                                                       next_handle->getBounds().toFloat().getCentre() );
                    h+=1;
                }
                
                else if ( next_handle->getHandleType() == PathHandle::quadratic_control )
                {
                    // the anchor has disappeared => try the cubic transformation
                    current_handle->setHandleType(PathHandle::cubic_control);
                    next_handle->setHandleType(PathHandle::cubic_control);
                    h-=1; // back on the same point next iteration
                }
                
                else // probably a cubic: remove this handle
                {
                    SymbolistComponent::removeSubcomponent(current_handle);
                    delete current_handle;
                }
            }
        }
        
        // cubic
        else if ( current_handle->getHandleType() == PathHandle::cubic_control )
        {
            m_path_array.getLast()->cubicTo(path_handles[h]->getBounds().toFloat().getCentre(),
                                           path_handles[h+1]->getBounds().toFloat().getCentre(),
                                           path_handles[h+2]->getBounds().toFloat().getCentre() );
            h+=2;
        }
        
        
        // last point should be a 'anchor'
        if ( path_handles[h]->isClosing() )
        {
            m_path_array.getLast()->closeSubPath();
        }
    }
    
    for ( int rh = 0; rh < remove.size() ; rh++ )
    {
        SymbolistComponent::removeSubcomponent(remove[rh]);
        delete remove[rh];
    }

}





/******************
 * Mouse
 *****************/

bool PathBaseComponent::hitTest (int x, int y)
{
    if( in_edit_mode || is_selected ) return true; // why ?
    else
    {
        for ( int np = 0; np < m_path_array.size(); np++ )
        {
            if (m_path_array[np]->intersectsLine( Line<float>( x - 5, y - 5, x + 5, y + 5) ) ||
                m_path_array[np]->intersectsLine( Line<float>( x + 5, y - 5, x - 5, y + 5) ) )
                return true;
        }
        return false;
    }
}


Point<float> PathBaseComponent::shiftConstrainMouseAngle( const PathHandle* last, const MouseEvent& event )
{
    if( event.mods.isShiftDown() )
    {
        float angle = last->getCenter().getAngleToPoint( event.position );
        
        if( (fabs(angle) < M_PI/4) || (fabs(angle) > 3*M_PI/4))
        {
            return Point<float>( last->getCenter().x, event.position.y );
        }
        else
        {
            return Point<float>( event.position.x, last->getCenter().y );
        }
    }
    else return event.position;
}


/******************
 * Draw path sections
 *****************/

void PathBaseComponent::abortDrawPath (  )
{
    m_preview_path.clear();
    drawing = false;
    if ( path_handles.getLast()->getHandleType() == PathHandle::start )
    {
        removeSubcomponent(path_handles.getLast());
        updatePathPoints();
        repaint();
    }
    else
    {
        path_handles.getLast()->setEnd(true);
    }
}


// inside an existing Component we're editing the path...
void PathBaseComponent::mouseAddClick ( const MouseEvent& event )
{
    auto pt = PathBaseComponent::shiftConstrainMouseAngle( path_handles.getLast(), event );
    
    if( in_edit_mode )
    {
        if ( !drawing ) // we were NOT already in a draw process
        {
            addHandle(PathHandle::start , pt.x, pt.y);
        }
        else
        {
            path_handles.getLast()->setEnd(false);
            addHandle(PathHandle::anchor, pt.x, pt.y);
            path_handles.getLast()->setEnd(true);
            updatePathPoints();
        }
        
        updatePathBounds();
        repaint();
    }
    else if( getMainMouseMode() == UI_EditType::draw )
    {
        // pass event to page/score if there should be a new path added
        getPageComponent()->mouseAddClick( event );
    }
}


void PathBaseComponent::mouseDoubleClick(const MouseEvent& event)
{
    if( in_edit_mode && drawing )
    {
        path_handles.getLast()->setClosing(true);
        abortDrawPath();
    }
    else
    {
        BaseComponent::mouseDoubleClick(event);
    }
}


void PathBaseComponent::mouseUp(const MouseEvent& event)
{
    BaseComponent::mouseUp(event);

    if( in_edit_mode && event.mods.isCommandDown() )
    {
        drawing = true;
    }
}


void PathBaseComponent::mouseMove( const MouseEvent& event )
{
    if( in_edit_mode && drawing )
    {
        if ( event.mods.isCommandDown() )
        {
            Path p;
            p.startNewSubPath( path_handles.getLast()->getCenter() );
            p.lineTo( PathBaseComponent::shiftConstrainMouseAngle( path_handles.getLast(), event ) );
            m_preview_path.swapWithPath( p );
            repaint();
        } else
        {
            abortDrawPath();
        }
    }
}



void PathBaseComponent::mouseDrag( const MouseEvent& event )
{
    if( in_edit_mode && drawing
        && ( path_handles.size() >= 2 )
        && event.getDistanceFromDragStart() > 10 )
    {
        if ( event.mods.isCommandDown() )
        {
            PathHandle* last = path_handles.getLast();
            PathHandle* butlast = path_handles[path_handles.size()-2];
            if ( butlast->getHandleType() == PathHandle::anchor || butlast->getHandleType() == PathHandle::start )
            {
                // insert a cubic controller here
                insertHandleBefore(last);
            }
            else
            {
                // move the previous control point
                butlast->setCentrePosition(event.x, event.y);
            }
            updatePathPoints();
            updatePathBounds();
            repaint();
        }
        else
        {
            abortDrawPath();
        }
    }
    else
    {
        BaseComponent::mouseDrag(event);
    }
}


/******************
 * Transformations
 *****************/

void PathBaseComponent::h_flip()
{
    Path m_path = mergePathArray();
    m_path.applyTransform( AffineTransform().rotated( float_Pi,
                                                     m_path_centroid.getX(),
                                                     m_path_centroid.getY()  ) );
    
    m_path.applyTransform( AffineTransform().verticalFlip( m_path_bounds.getHeight() ) );
    
    auto newrect = m_path_bounds.getRealPathBounds( m_path ).expanded( strokeType.getStrokeThickness() );
    m_path.applyTransform( AffineTransform().translated( -newrect.getPosition() ) );
    
    makePathArrayFromPath(m_path);
    updateHandlePositions();
    updatePathBounds();
    repaint();
    
}

void PathBaseComponent::v_flip()
{
    Path m_path = mergePathArray();
    
    m_path.applyTransform( AffineTransform().verticalFlip( m_path_bounds.getHeight() ) );
    
    auto newrect = m_path_bounds.getRealPathBounds( m_path ).expanded( strokeType.getStrokeThickness() );
    m_path.applyTransform( AffineTransform().translated( -newrect.getPosition() ) );
 
    makePathArrayFromPath(m_path);
    updateHandlePositions();
    updatePathBounds();
    
    repaint();
}


void PathBaseComponent::rotatePath ( float theta, float ax, float ay )
{
    accumTheta(theta);
    Path m_path = mergePathArray();
    m_path.applyTransform( AffineTransform().rotation( theta, ax, ay ) );
    makePathArrayFromPath(m_path);
    updateHandlePositions();
    updatePathBounds();
    repaint();
}


void PathBaseComponent::rotatePath ( float theta, bool accum )
{
    if( accum )
        accumTheta(theta);
    
    Path m_path = mergePathArray();
    m_path.applyTransform( AffineTransform().rotation( theta, m_path_centroid.getX(), m_path_centroid.getY()  ) );
    makePathArrayFromPath(m_path);
    updateHandlePositions();
    updatePathBounds();
    repaint(); 
}



/******************
 * Paint
 *****************/

void PathBaseComponent::paint ( Graphics& g )
{
    
    BaseComponent::paint(g);
    

    g.setColour( getCurrentColor() );
    
    /*
    int cur_t,local_t = 0;
    float strok = strokeType.getStrokeThickness();
    
    if ( isTopLevelComponent() )
    {
        cur_t = getSymbolistHandler()->getCurrentTime();
        local_t =  cur_t - getScoreSymbolPointer()->getTime() ;
        
        
        if (local_t >= 0 && local_t <= getScoreSymbolPointer()->getDuration())
        {
            strok = strokeWeight * (1 + local_t) * 0.003;
            g.setColour( Colours::indianred );
        }
    }
    */
    
    // to do: add other stroke options
    //float dashes[] = {1.0, 2.0};
    //strokeType.createDashedStroke(p, p, dashes, 2 );
    
    strokeType.setStrokeThickness( strokeType.getStrokeThickness() );
    
    // workaround since we don't know which context we're in, draw and return if in palette
    if( getPageComponent() == NULL )
    {
        for ( int np = 0; np < m_path_array.size(); np++)
        {
            g.strokePath(*m_path_array[np], strokeType );
            if( m_fill )
                g.fillPath(*m_path_array[np]);
        }
    }
    else
    {
        g.setColour( getCurrentColor() );
        for ( int np = 0; np < m_path_array.size(); np++)
        {
            //std::cout << "DRAW " << getComponentID() << " -- " << np << std::endl;
            g.strokePath(*m_path_array[np], strokeType );
            if( m_fill )
                g.fillPath(*m_path_array[np]);
        }
        
        if( !m_preview_path.isEmpty() )
        {
            g.setColour( preview_stroke_color );
            g.strokePath( m_preview_path, PathStrokeType(0.5) ); // different color for preview?
        }
        
        if( in_edit_mode )
        {
            drawHandlesLines(g);
        }
    }
}


void PathBaseComponent::drawHandlesLines( Graphics& g)
{
    float dashes[2] = {2.0f, 2.0f};
    
    for ( int h = 0; h < path_handles.size(); h ++ )
    {
        PathHandle* current_handle = path_handles[h];
        
        // normal start/anchor points
        if ( (current_handle->getHandleType() == PathHandle::start) && (h > 0))
        {
            g.setColour(Colours::grey);
            g.drawDashedLine(Line<float>(path_handles[h-1]->getCenter().x, path_handles[h-1]->getCenter().y,
                                         current_handle->getCenter().x, current_handle->getCenter().y), dashes, 2 );
        }
        else if ( current_handle->getHandleType() == PathHandle::quadratic_control )
        {   // in principle if this is a quadradic there must be a handle before and after...:s
            g.setColour(Colours::grey);
            g.drawDashedLine(Line<float>(path_handles[h-1]->getCenter().x, path_handles[h-1]->getCenter().y,
                                         current_handle->getCenter().x, current_handle->getCenter().y), dashes, 2 );
            g.drawDashedLine(Line<float>(path_handles[h+1]->getCenter().x, path_handles[h+1]->getCenter().y,
                                         current_handle->getCenter().x, current_handle->getCenter().y), dashes, 2 );
        }
        else if ( current_handle->getHandleType() == PathHandle::cubic_control )
        {   // check to see if its a number 1 or number 2 cubic controller...:s
            g.setColour(Colours::grey);
      
            if ( (path_handles[h-1]->getHandleType() == PathHandle::start) ||  (path_handles[h-1]->getHandleType() == PathHandle::anchor))
            {
                g.drawDashedLine(Line<float>(path_handles[h-1]->getCenter().x, path_handles[h-1]->getCenter().y,
                                             current_handle->getCenter().x, current_handle->getCenter().y),
                                            dashes, 2 );
            }
            
            if ( path_handles[h+1]->getHandleType() == PathHandle::anchor )
            {
                g.drawDashedLine(Line<float>(current_handle->getCenter().x, current_handle->getCenter().y,
                                             path_handles[h+1]->getCenter().x, path_handles[h+1]->getCenter().y),
                                            dashes, 2 );
            }
        }
    }
    
    g.setColour(Colours::lightblue);
    g.drawRect( m_path_bounds );
    
    if ( rotation_handle != NULL )
    {
        g.drawDashedLine(Line<float>(
                m_path_centroid.getX(), m_path_centroid.getY(),
                rotation_handle->getBounds().getCentreX(), rotation_handle->getBounds().getCentreY()
                ),
                dashes, 2 );
    }
}
