
#include "PathBaseComponent.h"
#include "SymbolistMainComponent.h"
#include "PageComponent.h"
#include "PathHandleComponent.h"

PathBaseComponent::PathBaseComponent(  const Symbol& s ) : BaseComponent( s )
{
    //importFromSymbol( s ) ;
}

PathBaseComponent::~PathBaseComponent()
{
    //printf("freeing path %p\n", this);
    removeHandles();
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


void PathBaseComponent::resizeToFit(int x, int y, int w, int h)
{
    Rectangle<int> r = Rectangle<int>(x,y,w,h).reduced( strokeType.getStrokeThickness() );
    if( r.getWidth() > 0 && r.getHeight() > 0)
    {
        m_path.scaleToFit(r.getX(), r.getY(), r.getWidth(), r.getHeight(), false );
        updateHandlePositions();
    }
}

// Juce callback
void PathBaseComponent::resized()
{
    BaseComponent::resized();
    
    if( getMainEditMode() == UI_EditType::draw )
    {
        resizeToFit(getLocalBounds().getX(), getLocalBounds().getY(), getLocalBounds().getWidth(), getLocalBounds().getHeight());
        //resizeToFit(m_path_bounds.getX(), m_path_bounds.getY(), m_path_bounds.getWidth(), m_path_bounds.getHeight());
    }
}

/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

int PathBaseComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    // adds the basic messages
    int messages_added = BaseComponent::addSymbolMessages( s, base_address );
    
    float ax = -111, ay = -111, startx = -111, starty = -111;
    
    int count = 0;
    Path::Iterator it(m_path);
    
    const String seg_baseaddr = base_address + "/segment/";
    
    while( it.next() )
    {
        const String seg_addr = seg_baseaddr + String(count);
        
        if (it.elementType == it.startNewSubPath)
        {
            ax = it.x1;
            ay = it.y1;
            startx = ax;
            starty = ay;
        }
        else if (it.elementType == it.lineTo)
        {
            s->addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"line" ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  it.x1 ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  it.y1 ) );
            messages_added += 3;
            count++;

            ax = it.x1;
            ay = it.y1;

        }
        else if (it.elementType == it.quadraticTo)
        {
            s->addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"quadratic" ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  it.x1, it.x2 ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  it.y1, it.y2 ) );
            messages_added += 3;
            count++;

            ax = it.x2;
            ay = it.y2;

        }
        else if (it.elementType == it.cubicTo)
        {
            s->addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"cubic" ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  it.x1, it.x2, it.x3 ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  it.y1, it.y2, it.y3 ) );
            messages_added += 3;
            count++;

            ax = it.x3;
            ay = it.y3;
        }
        else if (it.elementType == it.closePath)
        {
            s->addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"close" ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  startx ) );
            s->addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  starty ) );
            messages_added += 3;
            count++;

            ax = startx;
            ay = starty;
        }

    }
    
    s->addOSCMessage( OSCMessage(base_address + "/num_segments", count ) );
    messages_added += 1;
 
//    internal_symbol.printBundle();
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/

void PathBaseComponent::importFromSymbol(const Symbol &s)
{

        BaseComponent::importFromSymbol(s);
    
        //std::cout << "IMPORT PATH" << std::endl;
        //std::cout << s.getOSCMessageValue(String("/type")).getString() << std::endl;
        
        int num_pos = s.getOSCMessagePos("/numSegments");
        if( symbol_parse_error( num_pos, "/numSegments" ) ) return;
    
        OSCBundle b = s.getOSCBundle();
        
        float prev_x = -1111, prev_y = -1111;
        m_path.clear();
        
        for( int i = 0; i < b[num_pos].getMessage()[0].getInt32(); i++ )
        {
            const String base_addr = "/segment/" + std::to_string(i);
            
            const String type_addr = base_addr + "/type";
            int type_pos = s.getOSCMessagePos( type_addr );
            if( symbol_parse_error( type_pos, type_addr ) ) return;
            
            const String x_addr = base_addr + "/x_points";
            int xp = s.getOSCMessagePos( x_addr );
            if( symbol_parse_error( xp, x_addr ) ) return;
            
            const String y_addr = base_addr + "/y_points";
            int yp = s.getOSCMessagePos( y_addr );
            if( symbol_parse_error( yp, y_addr ) ) return;
            
            String seg_type = b[type_pos].getMessage()[0].getString();
            OSCMessage xm = b[xp].getMessage();
            OSCMessage ym = b[yp].getMessage();
            
            if (xm.size() != ym.size() )
            {
                std::cout << "x and y point lists must be the same length!\n";
                return;
            }
            
            float x0 = Symbol::getOSCValueAsFloat(xm[0]);
            float y0 = Symbol::getOSCValueAsFloat(ym[0]);
            
            if( x0 != prev_x || y0 != prev_y )
            {
                m_path.startNewSubPath( x0, y0 );
            }
            
            if( seg_type == "line" && xm.size() == 2 )
            {
                m_path.lineTo( Symbol::getOSCValueAsFloat(xm[1]), Symbol::getOSCValueAsFloat(ym[1]) );
                prev_x = Symbol::getOSCValueAsFloat(xm[1]);
                prev_y = Symbol::getOSCValueAsFloat(ym[1]);
            }
            else if( seg_type == "quadratic" && xm.size() == 3 )
            {
                m_path.quadraticTo( Symbol::getOSCValueAsFloat(xm[1]), Symbol::getOSCValueAsFloat(ym[1]),
                                   Symbol::getOSCValueAsFloat(xm[2]), Symbol::getOSCValueAsFloat(ym[2]) );
                prev_x = Symbol::getOSCValueAsFloat(xm[2]);
                prev_y = Symbol::getOSCValueAsFloat(ym[2]);
            }
            else if( seg_type == "cubic" && xm.size() == 4 )
            {
                m_path.cubicTo(Symbol::getOSCValueAsFloat(xm[1]), Symbol::getOSCValueAsFloat(ym[1]),
                               Symbol::getOSCValueAsFloat(xm[2]), Symbol::getOSCValueAsFloat(ym[2]),
                               Symbol::getOSCValueAsFloat(xm[3]), Symbol::getOSCValueAsFloat(ym[3]) );
                prev_x = Symbol::getOSCValueAsFloat(xm[3]);
                prev_y = Symbol::getOSCValueAsFloat(ym[3]);
            }
        }
        
    }

/******************
 * MODES
 *****************/


void PathBaseComponent::updatePathBounds ()
{
    m_path_bounds.getRealPathBounds( m_path );
    m_path_centroid = m_path_bounds.getCentre();
    m_path_origin = m_path_bounds.getPosition();
}

void PathBaseComponent::setMinimalBounds ()
{
    Rectangle<float> pathBounds = tranformAndGetBoundsInParent( m_path );
    setBoundsFloatRect( pathBounds );
    updatePathBounds();
}

void PathBaseComponent::setMaximalBounds ()
{
    Point<int> init_pos =  positionRelativeTo(getPageComponent());
    BaseComponent::setMaximalBounds();
    m_path.applyTransform(AffineTransform::translation(init_pos));
    updatePathBounds();
}

void PathBaseComponent::setEditMode( bool val )
{
    in_edit_mode = val;
    if ( val ) makeHandlesFromPath();
    else removeHandles();
}



/******************
 * HANDLES (path edit)
 *****************/

void PathBaseComponent::addHandle( PathHandle::handleType type, float x, float y )
{
    PathHandle *h = new PathHandle( type, x + getX(), y + getY() );
    addAndMakeVisible( h );
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
    addAndMakeVisible(h1);
    addAndMakeVisible(h2);
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
        addAndMakeVisible(h);
        path_handles.insert(position, h);
    }
}


void PathBaseComponent::makeHandlesFromPath()
{

    if( path_handles.size() == 0 ) // should always be the case when we call this
    {
        float ax =0, ay = 0;
        
        Path::Iterator it( m_path );
        
        while( it.next() )
        {
            if (it.elementType == it.startNewSubPath)
            {
                if ( !path_handles.isEmpty() )
                {
                    path_handles.getLast()->setEnd(true);
                }
                addHandle( PathHandle::start, it.x1, it.y1 );
                ax = it.x1;
                ay = it.y1;
            }
            else if (it.elementType == it.lineTo)
            {
                addHandle( PathHandle::anchor, it.x1, it.y1 );
                ax = it.x1;
                ay = it.y1;
                
            }
            else if (it.elementType == it.quadraticTo)
            {
                addHandle( PathHandle::quadratic_control, it.x1, it.y1 );
                addHandle( PathHandle::anchor, it.x2, it.y2 );
                ax = it.x2;
                ay = it.y2;
            }
            else if (it.elementType == it.cubicTo)
            {
                addHandle( PathHandle::cubic_control, it.x1, it.y1 );
                addHandle( PathHandle::cubic_control, it.x2, it.y2 );
                addHandle( PathHandle::anchor, it.x3, it.y3 );
                
                ax = it.x3;
                ay = it.y3;
            }
            else if( it.elementType == it.closePath )
            {
                path_handles.getLast()->setEnd(true);
                path_handles.getLast()->setClosing(true);
            }

        }
        
        updatePathBounds();
        
        float length = max( m_path_bounds.getHeight(), m_path_bounds.getWidth() ) * 0.5 + 5;
        rotation_handle = new PathHandle( PathHandle::rotate, m_path_centroid.getX(), m_path_centroid.getY() + length );
        addAndMakeVisible( rotation_handle );
    }
}


void PathBaseComponent::removeHandle(PathHandle* h)
{
    for( int i = 0; i < path_handles.size(); i++ )
    {
        if( h == path_handles[i] ) path_handles.remove(i);
    }
    h->getParentComponent()->removeChildComponent( h );
    delete h;
}


void PathBaseComponent::removeHandles()
{
    for ( PathHandle* h : path_handles )
    {
        h->getParentComponent()->removeChildComponent( h );
        delete h;
    }
    path_handles.clear();
    
    delete rotation_handle;
}



/***********************
 * Update handles after transformations on the path
 * We must assume that the _structure_ of the path is consistent with the existing handles.
 ***********************/

void PathBaseComponent::updateHandlePositions()
{
    if ( in_edit_mode && !path_handles.isEmpty() ) // in principle path_handle is not empty if we're in edit mode..
    {
        Path::Iterator it( m_path );
        int n = 0;
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



/***********************
 * Update path after user action on handles
 ************************/
void PathBaseComponent::updatePathPoints()
{
    Path p;
    for( int h = 0 ; h < path_handles.size() ; h++ )
    {
        if ( path_handles[h]->getHandleType() == PathHandle::start )
        {
            p.startNewSubPath( path_handles[h]->getBounds().toFloat().getCentre() );
        }
        else if ( path_handles[h]->getHandleType() == PathHandle::anchor )
        {
            p.lineTo( path_handles[h]->getBounds().toFloat().getCentre() );
        }
        else if ( path_handles[h]->getHandleType() == PathHandle::quadratic_control )
        {
            p.quadraticTo(path_handles[h]->getBounds().toFloat().getCentre() ,
                          path_handles[h+1]->getBounds().toFloat().getCentre() );
            h+=1;
        }
        else if ( path_handles[h]->getHandleType() == PathHandle::cubic_control )
        {
            p.cubicTo(path_handles[h]->getBounds().toFloat().getCentre(),
                      path_handles[h+1]->getBounds().toFloat().getCentre(),
                      path_handles[h+2]->getBounds().toFloat().getCentre() );
            h+=2;
        }
        
        if ( path_handles[h]->isClosing() )
        {
            p.closeSubPath();
        }
    }    
    m_path.swapWithPath( p );
}



// used to be called at exiting edit mode...
// !!! redo THIS
Rectangle<float> PathBaseComponent::tranformAndGetBoundsInParent( Path& p )
{
    float strokeOffset = strokeType.getStrokeThickness() * 0.5;
    
    m_path_bounds.getRealPathBounds( m_path );
    Rectangle<float> abs_bounds = m_path_bounds;
    
    // NOT FUNCTIONAL PROGRAMMING, but oh well
    p.applyTransform( AffineTransform().translated( -abs_bounds.getX() + strokeOffset, -abs_bounds.getY() + strokeOffset ) );
    return abs_bounds.expanded( strokeOffset );
}


/******************
 * Mouse
 *****************/

bool PathBaseComponent::hitTest (int x, int y)
{
    if( in_edit_mode || is_selected ) return true; // why ?
    else return (
                 m_path.intersectsLine( Line<float>( x - 5, y - 5, x + 5, y + 5) ) ||
                 m_path.intersectsLine( Line<float>( x + 5, y - 5, x - 5, y + 5) )
                 );
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
        removeHandle(path_handles.getLast());
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
    if( in_edit_mode )
    {
        if ( !drawing ) // we were NOT already in a draw process
        {
            addHandle(PathHandle::start , event.x, event.y);
        }
        else
        {
            path_handles.getLast()->setEnd(false);
            addHandle(PathHandle::anchor, event.x, event.y);
            path_handles.getLast()->setEnd(true);
            updatePathPoints();
        }
        repaint();
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
    
    reportModification();
}


void PathBaseComponent::mouseMove( const MouseEvent& event )
{
    if( in_edit_mode && drawing )
    {
        if ( event.mods.isCommandDown() )
        {
            Path p;
            p.startNewSubPath( path_handles.getLast()->getCenter() );
            p.lineTo( shiftConstrainMouseAngle( path_handles.getLast(), event ) );
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
    m_path.applyTransform( AffineTransform().rotated( float_Pi,
                                                     m_path_centroid.getX(),
                                                     m_path_centroid.getY()  ) );
    
    auto actualBounds = tranformAndGetBoundsInParent( m_path );
    m_path.applyTransform( AffineTransform().verticalFlip( actualBounds.getHeight() ) );
    m_path.applyTransform( AffineTransform().translated( m_path_origin ) );
    
    updateHandlePositions();
    updatePathBounds();
    repaint();
    
}

void PathBaseComponent::v_flip()
{

    auto actualBounds = tranformAndGetBoundsInParent(m_path);
    m_path.applyTransform( AffineTransform().verticalFlip( actualBounds.getHeight() ) );
    m_path.applyTransform( AffineTransform().translated( m_path_origin ) );
    updateHandlePositions();
    updatePathBounds();
    repaint();
}


void PathBaseComponent::rotatePath ( float theta, float ax, float ay )
{
    m_path.applyTransform( AffineTransform().rotation( theta, ax, ay ) );
    updateHandlePositions();
    updatePathBounds();
    repaint();
}


void PathBaseComponent::rotatePath ( float theta )
{
    m_path.applyTransform( AffineTransform().rotation( theta, m_path_centroid.getX(), m_path_centroid.getY()  ) );
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
    
    int cur_t,local_t = 0;
    g.setColour( getCurrentColor() );
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
    
    // to do: add other stroke options
    //float dashes[] = {1.0, 2.0};
    //strokeType.createDashedStroke(p, p, dashes, 2 );
    
    strokeType.setStrokeThickness( strokeType.getStrokeThickness() );
    
    // workaround since we don't know which context we're in, draw and return if in palette
    if( getPageComponent() == NULL )
    {
        g.strokePath(m_path, strokeType );
    }
    else
    {
        g.setColour( getCurrentColor() );
        g.strokePath(m_path, strokeType );
        
        if( !m_preview_path.isEmpty() )
        {
            g.setColour( preview_stroke_color );
            g.strokePath(m_preview_path, PathStrokeType(0.5) ); // different color for preview?
        }
        
        
        if( fill ) // preview fill also?
        {
            // will need to check for selection color
            // getFillColor()
            // g.setColour( fill_color );
            g.fillPath(m_path);
        }
        
        if( in_edit_mode )
        {
            drawHandlesLines(g);
        }
    }
}


void PathBaseComponent::drawHandlesLines( Graphics& g)
{
    float ax = -1, ay = -1;
    float dashes[2] = {2.0f, 2.0f};
    
    Path::Iterator it( m_path );
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            ax = it.x1;
            ay = it.y1;
        }
        else if (it.elementType == it.lineTo)
        {
            ax = it.x1;
            ay = it.y1;
        }
        else if (it.elementType == it.quadraticTo)
        {
            g.drawDashedLine(Line<float>(ax, ay, it.x1, it.y1), dashes, 2 );
            ax = it.x2;
            ay = it.y2;
        }
        else if (it.elementType == it.cubicTo)
        {
            g.drawDashedLine(Line<float>(ax, ay, it.x1, it.y1), dashes, 2 );
            g.drawDashedLine(Line<float>(it.x2, it.y2, it.x3, it.y3), dashes, 2 );
            ax = it.x3;
            ay = it.y3;
        }
        else if (it.elementType == it.closePath)
        {
            
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
