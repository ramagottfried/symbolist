
#include "PathBaseComponent.h"
#include "SymbolistMainComponent.h"
#include "PageComponent.h"
#include "PathHandleComponent.h"
#include "StringTools.hpp"

PathBaseComponent::~PathBaseComponent()
{
    BaseComponent::~BaseComponent();
    removeHandles();
}


void PathBaseComponent::removeHandles()
{
    SymbolistComponent::clearAllSubcomponents();
    path_handles.clear(); // are they deleted ?
    
    if ( getMainComponent() != NULL && rotation_handle != NULL )
    {
        delete rotation_handle;
        rotation_handle = NULL;
    }
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


/******************
 * Paint
 *****************/

void PathBaseComponent::paint ( Graphics& g )
{
    
    // printPath( m_path ) ;
    
    BaseComponent::paint(g);
    
    /*
     int cur_t,local_t = 0;
     float strok = m_stroke_type.getStrokeThickness();
     
     if ( isTopLevelComponent() )
     {
     cur_t = getSymbolistHandler()->getCurrentTime();
     local_t =  cur_t - getScoreSymbolPointer()->getTime() ;
     
     
     if (local_t >= 0 && local_t <= getScoreSymbolPointer()->getDuration())
     {
     strok = m_stroke_type.getStrokeThickness() * (1 + local_t) * 0.003;
     g.setColour( Colours::indianred );
     }
     }
     */
    
    // to do: add other stroke options
    //float dashes[] = {1.0, 2.0};
    //m_stroke_type.createDashedStroke(p, p, dashes, 2 );
    
    g.setColour( getCurrentColor() );
    
    float penSize = m_stroke_type.getStrokeThickness() ;
    Path tmp_path = m_path;
    tmp_path.applyTransform( AffineTransform().translation( penSize, penSize ) );
    g.strokePath( tmp_path , m_stroke_type );
    if( m_fill ) g.fillPath( tmp_path );
    
    
    // EDIT PREVIEW
    if( !m_preview_path.isEmpty() )
    {
        g.setColour( preview_stroke_color );
        g.strokePath( m_preview_path, PathStrokeType(0.5) ); // different color for preview?
    }
    
    if( in_edit_mode )
    {
        drawHandlesLines(g);
    }
    
    /*
     auto c = getLocalBounds().getCentre();
     g.drawEllipse(c.getX()-2, c.getY()-2, 4, 4, 1);
     */
    g.setColour( Colour::greyLevel(0.5) );
    g.drawRect( getLocalBounds() );
    
}


/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

void PathBaseComponent::addSymbolMessages(Symbol* s )
{
    BaseComponent::addSymbolMessages(s);
    
    s->addMessage ( "/fill" , m_fill );
    
    if ( type == PATH ) {
        
        s->addMessage( "/length", m_path.getLength() );
        s->addMessage( "/str",    m_path.toString().getCharPointer() );
    
    }
        
    s->addMessage ( "/fill" ,               m_fill   );
    s->addMessage ( "/stroke/thickness" ,   m_stroke_type.getStrokeThickness()   );
    s->addMessage ( "/rotation" ,           m_rotation   );
    
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/

void PathBaseComponent::importFromSymbol(const Symbol &s)
{
    cout << "IMPORTING PATH FROM SYMBOL " << Symbol::stringFromSymType( type ) << " " << this << endl ;
    
    // rotation and stroke thickness need to be imported first to correctly compute the bounds etc.
    m_rotation = s.getMessage("/rotation").getFloat();
    float tmpSW = s.getMessage("/stroke/thickness").getInt();
    m_stroke_type.setStrokeThickness( (tmpSW == 0) ? 2 : tmpSW );
    
    // import other path-specific attributes while we're at it
    m_fill = s.getMessage("/fill").getInt();
    
    BaseComponent::importFromSymbol(s);
}


// This will be called by BaseComponent::importFromSymbol(s);
// in the case of BasicShapes, w & h are the *pre-rotated values*
void PathBaseComponent::setComponentFromSymbol(const Symbol &s, float x, float y , float w , float h)
{
    switch ( type ) {
            
        case PATH:
            
            m_path.restoreFromString( s.getMessage("/str").getString().c_str() );
            // Drawable::parseSVGPath( ... ) << change to this
            
            break;
            
        case CIRCLE:
            
            //m_path.addEllipse( x , y - (h * 0.5), w, h);
            m_path.addEllipse( 0 , 0,  w, h);
            m_component_center = Point<float>( 0.0 , h * 0.5 );
            break;
            
        case RECTANGLE:
            
            m_path.addRectangle(0 , 0, w, h);
            m_component_center = Point<float>( 0.0 , h * 0.5 );
            break;
            
        case TRIANGLE:
            
            m_path.addTriangle( w * 0.5 , 0 , 0 , h , w , h);
            m_component_center = Point<float>( 0.0 , h * 0.5 );
            break;
            
        default: break;
    }
    
    
    // APPLY THE ROTATION
    m_path.applyTransform( AffineTransform().rotation( m_rotation, m_component_center.getX(), m_component_center.getY()) );
    
    //m_path.applyTransform( AffineTransform().translation( m_stroke_type.getStrokeThickness(), m_stroke_type.getStrokeThickness() ) );
    
    float penSize = m_stroke_type.getStrokeThickness();
    
    // SET BOUNDS: THE BOUNDS ARE EXTENDED ACCORDING TO PENSIZE.
    // THE PATH COORDINATES ARE NOT AFFECTED
    Point<float> compPos = computePositionFromSymbolValues(x,y,w,h);
    setBounds( m_path.getBounds().translated( compPos.getX() - penSize, compPos.getY()  - penSize ).expanded( penSize ).toNearestInt() );
}


Point<float> PathBaseComponent::computePositionFromSymbolValues(float x, float y, float w, float h)
{
    return Point<float>( x - m_component_center.getX() , y - m_component_center.getY() );
}


Point<float> PathBaseComponent::computeSymbolPosition(float x, float y, float w, float h)
{
    return Point<float>( x + m_component_center.getX() , y + m_component_center.getY() );
}







/******************
 * MODES
 *****************/

Rectangle<float> PathBaseComponent::symbol_export_bounds() 
{
    // initially for simple path:
    //return getBounds().toFloat();
    
    auto b = getBounds().toFloat();
    
    Sym_PathBounds pbounds( m_path );
    
    // rotate backwards and to get size values
    m_path.applyTransform( AffineTransform().rotation( -m_rotation, pbounds.getCentreX(), pbounds.getCentreY()  ) );
    auto pb = pbounds.getRealPathBounds( m_path ).expanded( m_stroke_type.getStrokeThickness() );
    
    return Rectangle<float>( b.getX(), b.getCentreY(), pb.getWidth(), pb.getHeight() );
}









void PathBaseComponent::updatePathBounds ()
{
    m_path_bounds.getRealPathBounds( m_path );
    //m_path_centroid = m_path_bounds.getCentre();
}

Rectangle<float> PathBaseComponent::drawAndRotateShape(float cx, float cy, float w, float h)
{
    auto area = Rectangle<float>(0,0,w,h).reduced( m_stroke_type.getStrokeThickness() );
    
    if ( type == RECTANGLE ) {
        m_path.addRectangle(area);
        updatePathBounds();
        rotateScoreComponent(m_rotation, cx, cy);
    } else if ( type == CIRCLE ) {
        m_path.addEllipse(area);
        updatePathBounds();
        rotatePath(m_rotation, false);
    } else if ( type == TRIANGLE ) {
        m_path.addTriangle( area.getBottomLeft(), Point<float>(area.getCentreX(), area.getY()), area.getBottomRight());
        updatePathBounds();
        rotateScoreComponent(m_rotation, cx, cy);
    } else {
       // m_path.restoreFromString()
    }

    return m_path_bounds; // return bounds post rotation
}







// this shoudl be easy to simplify...
bool PathBaseComponent::intersectRect( Rectangle<int> rect)
{
    Sym_PathBounds pb;
    
    Rectangle<int> shifted_rect = rect.translated( - getBoundsInParent().getX(), - getBoundsInParent().getY() );
    
    Rectangle<float> r_path = pb.getRealPathBounds( m_path );
    
    return (
            (  r_path.getX() >= shifted_rect.getX() && r_path.getY() >= shifted_rect.getY()     &&
             r_path.getRight() <= shifted_rect.getRight() && r_path.getBottom() <= shifted_rect.getBottom() )
            // => he path is _inside_ the rect
            ||
            (   m_path.intersectsLine( Line<float>( shifted_rect.getX() , shifted_rect.getY() , shifted_rect.getRight() , shifted_rect.getY())) ||
             m_path.intersectsLine( Line<float>( shifted_rect.getRight() , shifted_rect.getY() , shifted_rect.getRight() , shifted_rect.getBottom())) ||
             m_path.intersectsLine( Line<float>( shifted_rect.getX() , shifted_rect.getBottom() , shifted_rect.getRight() , shifted_rect.getBottom())) ||
             m_path.intersectsLine( Line<float>( shifted_rect.getX() , shifted_rect.getY() , shifted_rect.getX() , shifted_rect.getBottom())) )
            // => the path intersects one of the vertices of the rect
            ) ;
}



/************************************************
 ************************************************
 ************************************************
 ************************************************
 * EDIT MODE
 ************************************************
 ************************************************
 ************************************************
 ************************************************/

void PathBaseComponent::setEditMode( bool val )
{
    in_edit_mode = val;
    if ( val == false ) // = exit
    {
        ScoreComponent* sc = dynamic_cast<ScoreComponent*>(getParentComponent());
        
        // Checks the downcast result.
        if (sc != NULL)
        {
            sc->addToSelection(this);
            
            removeHandles();
            
            // sc->deleteSelectedComponents();
        }
        
    }
}


void PathBaseComponent::setMinimalBounds ()
{
    m_path_bounds.getRealPathBounds(m_path);
    Rectangle<float> symbol_bounds = m_path_bounds.expanded( m_stroke_type.getStrokeThickness() );
    m_path.applyTransform(AffineTransform::translation(- symbol_bounds.getX(), - symbol_bounds.getY() ));
    
    setBounds( 0, 0, symbol_bounds.getWidth(), symbol_bounds.getHeight() );
    setTopLeftPosition(symbol_bounds.getX(), symbol_bounds.getY());
    
    updatePathBounds();
}

void PathBaseComponent::setMaximalBounds ()
{
    Point<int> init_pos =  positionRelativeTo(getPageComponent());
    BaseComponent::setMaximalBounds();
    m_path.applyTransform(AffineTransform::translation(init_pos));
    updatePathBounds();
    
    // cout << "PathBaseComponent::setMaximalBounds" << endl;
    makeHandlesFromPath();
}



void PathBaseComponent::unselectAllComponents()
{
    ScoreComponent::unselectAllComponents();
    if ( rotation_handle != NULL )
        removeFromSelection(rotation_handle);
}

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

string PathBaseComponent::exportSVG()
{
    string path_d;
    Path::Iterator it( m_path );
    
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            path_d += "M" + trimStringzeros(to_string(it.x1)) + "," + trimStringzeros(to_string(it.y1));
        }
        else if (it.elementType == it.lineTo)
        {
            path_d += "L" + trimStringzeros(to_string(it.x1)) + "," + trimStringzeros(to_string(it.y1));
        }
        else if (it.elementType == it.quadraticTo)
        {
            path_d += "Q" + trimStringzeros(to_string(it.x1)) + "," + trimStringzeros(to_string(it.y1)) +" " + trimStringzeros(to_string(it.x2)) + "," + trimStringzeros(to_string(it.y2));
        }
        else if (it.elementType == it.cubicTo)
        {
            path_d += "C" + trimStringzeros(to_string(it.x1)) + "," + trimStringzeros(to_string(it.y1)) +
            " " + trimStringzeros(to_string(it.x2)) + "," + trimStringzeros(to_string(it.y2)) +
            " " + trimStringzeros(to_string(it.x3)) + "," + trimStringzeros(to_string(it.y3));
        }
        else if( it.elementType == it.closePath )
        {
            path_d += "Z";
        }
        else
        {
            cout << "undefined JUCE Path type -- (probalby not possible) " << it.elementType << endl;
        }
    }
    return path_d;
}


void PathBaseComponent::makeHandlesFromPath()
{
    
    if( path_handles.size() == 0 ) // should always be the case when we call this
    {
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
    
    if ( m_path_bounds.getWidth() > 0 || m_path_bounds.getHeight() > 0 )
    {
        updateRotationHandle();
    }
}

void PathBaseComponent::updateRotationHandle()
{
    updatePathBounds();
    
    if( !rotation_handle )
    {
        float length = max( m_path_bounds.getHeight(), m_path_bounds.getWidth() ) * 0.5 + 5;
        rotation_handle = new PathHandle( PathHandle::rotate, m_path_centroid.getX(), m_path_centroid.getY() + length );
        addAndMakeVisible( rotation_handle );
    }
    else
    {
        float length = max( m_path_bounds.getHeight(), m_path_bounds.getWidth() ) * 0.5 + 5;
        rotation_handle->setCentrePosition( m_path_centroid.getX(), m_path_centroid.getY() + length );
    }
}


void PathBaseComponent::subtractHandle( int i )
{
    PathHandle *h = NULL, *prev = NULL;
    
    if ( in_edit_mode && !path_handles.isEmpty() ) // in principle path_handle is not empty if we're in edit mode..
    {
        int n = 0;
        
        Path::Iterator it( m_path );
        
        while( it.next() )
            {
                if (it.elementType == it.startNewSubPath)
                {
                    // if (n > 0 ) path_handles[n+1]->setEnd(true);
                    h = path_handles[n++];
                    
                    prev = h;
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
    
    path_handles.remove(i);
}


void PathBaseComponent::removeSubcomponent(SymbolistComponent* h)
{
    // note: paths do not have subcomponents, only groups or staves
    
    for( int i = 0; i < path_handles.size(); i++ )
    {
        if( h == path_handles[i] )
        {
            path_handles.remove(i);
            
            // we should to do subtraction a little more cleanly
            
            // one problem is that the JUCE Path only allows access to the points via the Path iterator, and that doesn't give you a prev(), so it's difficult to deal with the segment as a whole, probably we need to make our own version of the JUCE Path class.
            
            //            subtractHandle( i );
        }
    }
    
    ScoreComponent::removeSubcomponent( h );
    updatePathPoints();
    updatePathBounds();
    repaint();
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
        
        Path::Iterator it( m_path );
        
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
 * also fixes the handle sequence
 ************************/
void PathBaseComponent::updatePathPoints()
{
    
    std::vector<PathHandle*> remove;
    
    for( int h = 0 ; h < path_handles.size() ; h++ )
    {
        PathHandle* current_handle = path_handles[h];
        
        // normal start/anchor points
        if ( current_handle->getHandleType() == PathHandle::start )
        {
            m_path.startNewSubPath( current_handle->getBounds().toFloat().getCentre() );
        }
        else if ( current_handle->getHandleType() == PathHandle::anchor )
        {
            // deal with the case where the starting point has been erased
            if ( m_path.isEmpty() )
            {
                current_handle->setHandleType(PathHandle::start);
                m_path.startNewSubPath( current_handle->getBounds().toFloat().getCentre() );
            }
            else // normal case
            {
                m_path.lineTo( current_handle->getBounds().toFloat().getCentre() );
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
                    m_path.quadraticTo(current_handle->getBounds().toFloat().getCentre() ,
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
                    SymbolistComponent::removeSubcomponent( current_handle );
                    delete current_handle;
                }
            }
        }
        
        // cubic
        else if ( current_handle->getHandleType() == PathHandle::cubic_control )
        {
            m_path.cubicTo(path_handles[h]->getBounds().toFloat().getCentre(),
                                            path_handles[h+1]->getBounds().toFloat().getCentre(),
                                            path_handles[h+2]->getBounds().toFloat().getCentre() );
            h+=2;
        }
        
        
        // last point should be a 'anchor'
        if ( path_handles[h]->isClosing() )
        {
            m_path.closeSubPath();
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
    if ( in_edit_mode || is_selected ) return true;

    else return ( m_path.intersectsLine( Line<float>( x - 5, y - 5, x + 5, y + 5) ) ||
                  m_path.intersectsLine( Line<float>( x + 5, y - 5, x - 5, y + 5) ) ) ;
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
    
    if( in_edit_mode )
    {
        
        auto pt =  path_handles.size() > 0 ? PathBaseComponent::shiftConstrainMouseAngle( path_handles.getLast(), event ) : event.getPosition().toFloat();
        
        if ( !drawing ) // we were NOT already in a draw process
        {
            addHandle(PathHandle::start , pt.x, pt.y);
        }
        else
        {
            m_preview_path.clear();
            
            path_handles.getLast()->setEnd(false);
            addHandle(PathHandle::anchor, pt.x, pt.y);
            path_handles.getLast()->setEnd(true);
            
            updatePathPoints();
            updateRotationHandle();
        }
        
        updatePathBounds();
        repaint();
    }
    else if( getMainMouseMode() == UI_EditType::DRAW )
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

void PathBaseComponent::h_flip(float ax, float ay)
{
    // cout << this << " h_flip " << ax - getX() << " " <<  ay - getY() << endl;
    
    accumTheta(float_Pi);
    
    m_path.applyTransform( AffineTransform().verticalFlip( round(ay - getY()) * 2.0 ) );
    m_path.applyTransform( AffineTransform().rotation( float_Pi, round(ax - getX()), round(ay - getY())) );
    
    auto newrect = m_path_bounds.getRealPathBounds( m_path ).toNearestInt().expanded( m_stroke_type.getStrokeThickness() );
    
    m_path.applyTransform( AffineTransform().translated( -newrect.getPosition() ) );
    
    updatePathBounds();
    
    //printRect(m_path_bounds, "m_path_bounds 3");
    //printPoint(newrect.getPosition(), "newpos");
    
    auto newBounds = newrect + getPosition();
    if( getBounds() != newBounds )
    {
        setBounds( newBounds );
    }
    else
    {
        reportModification();
        repaint();
    }
    
}

void PathBaseComponent::v_flip(float ax, float ay)
{
    
    m_path.applyTransform( AffineTransform().verticalFlip( round(ay - getY()) * 2.0 ) );
    
    auto newrect = m_path_bounds.getRealPathBounds( m_path ).toNearestInt().expanded( m_stroke_type.getStrokeThickness() );
    
    m_path.applyTransform( AffineTransform().translated( -newrect.getPosition() ) );
    
    updatePathBounds();
    
    //printRect(m_path_bounds, "m_path_bounds 3");
    //printPoint(newrect.getPosition(), "newpos");
    
    auto newBounds = newrect + getPosition();
    if( getBounds() != newBounds )
    {
        setBounds( newBounds );
    }
    else
    {
        reportModification();
        repaint();
    }
}


void PathBaseComponent::rotatePath ( float theta, float ax, float ay )
{
    accumTheta(theta);
    m_path.applyTransform( AffineTransform().rotation( theta, ax, ay ) );
    updateHandlePositions();
    updatePathBounds();
    repaint();
}


void PathBaseComponent::rotatePath ( float theta, bool accum )
{
    if( accum )
        accumTheta(theta);
    
    m_path.applyTransform( AffineTransform().rotation( theta, m_path_centroid.getX(), m_path_centroid.getY()  ) );
    updateHandlePositions();
    updatePathBounds();
    repaint();
}

// note: rotateScoreComponent uses the coordinate system of the parent)
void PathBaseComponent::rotateScoreComponent( float theta, float ax, float ay )
{
    
    cout << this << " path rotate ref point" << " " << ax - getX() << " " << ay - getY() << endl;
    
    //printRect(m_path_bounds, "m_path_bounds 1");
    
    accumTheta(theta);
    
    // apply rotation
    m_path.applyTransform( AffineTransform().rotation( theta, ax - getX(), ay - getY()) );
    
    updatePathBounds();
    
    //printRect(m_path_bounds, "m_path_bounds 2");
    
    Rectangle<int> symbol_bounds = m_path_bounds.expanded( m_stroke_type.getStrokeThickness() ).toNearestInt();
    m_path.applyTransform(AffineTransform::translation(-symbol_bounds.getX(), -symbol_bounds.getY() ));
    updatePathBounds();
    
    //printRect(m_path_bounds, "m_path_bounds 3");
    auto temp = in_edit_mode;
    in_edit_mode = true;
    setBounds( symbol_bounds + getPosition() );
    in_edit_mode = temp;
    
}

void PathBaseComponent::scaleScoreComponent(float scale_w, float scale_h)
{
    // resize is for the total bounds including the stroke offset
    
    if( scale_w > 0 && scale_h > 0 && getWidth() && getHeight() )
    {
        
        //BaseComponent::scaleScoreComponent(scale_w, scale_h); // << prb
        cout << "PathBaseComponent::scaleScoreComponent " << this << endl;
        printRect(getBounds(), "compo bounds");
        cout << "scale_w " << scale_w << " scale_h " << scale_h << endl;
        cout << "target w " << scale_w * getWidth() << " target h " << scale_h * getHeight() << endl;
        
        float sw = 2.0 * m_stroke_type.getStrokeThickness();
        
        // cout << "target w- " << scale_w * getWidth() - sw << " target h- " << scale_h * getHeight() - sw << endl;
        
        
        float new_w = round( scale_w * getWidth() );
        float new_h = round( scale_h * getHeight() );
        float new_path_w = round(new_w - sw);
        float new_path_h = round(new_h - sw);
        
        if( new_path_w < 1 ) new_path_w = 1;
        if( new_path_h < 1 ) new_path_h = 1;
        
        //float adj_scale_w = (new_path_w / (m_path_bounds.getWidth()  == 0 ? 1 : m_path_bounds.getWidth() ) );
        //float adj_scale_h = (new_path_h / (m_path_bounds.getHeight() == 0 ? 1 : m_path_bounds.getHeight()) );
        
        float adj_scale_w = ( new_path_w / m_path.getBounds().getWidth() );
        float adj_scale_h = ( new_path_h / m_path.getBounds().getHeight() );
        
        printRect(m_path_bounds, "1 m_path_bounds");
        
        cout << "new_path wh " << new_path_w << " " << new_path_h << " sw " << sw << endl;
        cout << "adj scale " << adj_scale_w << " " << adj_scale_h << endl;
        
        m_path.applyTransform( AffineTransform().scale(adj_scale_w, adj_scale_h) );
        
        // updatePathBounds();
        
        printRect(m_path_bounds, "2 m_path_bounds");
        
        //Rectangle<float> symbol_bounds = m_path_bounds.expanded( m_stroke_type.getStrokeThickness() );
        Rectangle<float> symbol_bounds = m_path.getBounds().expanded( m_stroke_type.getStrokeThickness() );
        m_path.applyTransform( AffineTransform::translation( -symbol_bounds.getPosition() ) );
        
        updateHandlePositions();
        //updatePathBounds();
        
        printRect(m_path_bounds, "3 m_path_bounds");
        
        cout << "new size " << new_w << " " << new_h << endl;
        auto temp = in_edit_mode;
        in_edit_mode = true;  // :s
        setSize(new_w, new_h );
        // printRect(symbol_bounds.toNearestInt() + getPosition(), "new symb bounds");
        in_edit_mode = temp;
        
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




