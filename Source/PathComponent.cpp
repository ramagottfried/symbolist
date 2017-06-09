
#include "PathComponent.h"
#include "PageComponent.h"

void PathComponent::printPath( Path p )
{
    Path::Iterator it(p);
    int count = 0;
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            std::cout << count++ << " " << "start point " << it.x1 << " " << it.y1 << "\n";
        }
        else if (it.elementType == it.lineTo)
        {
            std::cout << count++ << " " << "line to " << it.x1 << " " << it.y1 << "\n";
        }
        else if (it.elementType == it.quadraticTo)
        {
            std::cout << count++ << " " << "quadratic to " << it.x1 << " " << it.y1 << " " << it.x2 << " " << it.y2 << "\n";
        }
        else if (it.elementType == it.cubicTo)
        {
            std::cout << count++ << " " << "cubic to " << it.x1 << " " << it.y1 << " " << it.x2 << " " << it.y2 << " " << it.x3 << " " << it.y3 << "\n";
        }
        else if (it.elementType == it.closePath)
        {
            std::cout << count++ << " " << "close path\n";
        }
    }
}

/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

int PathComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    // adds the basic messages
    int messages_added = BaseComponent::addSymbolMessages(s, base_address);
    
    String x_address = String(base_address) += "/x-points" ;
    String y_address = String(base_address) += "/y-points" ;
    
    OSCMessage x_mess(x_address);
    OSCMessage y_mess(y_address);
    
    Path::Iterator it(m_path);
    while( it.next() ) {
        if ( it.elementType != it.closePath )
        {
            x_mess.addFloat32( it.x1 );
            y_mess.addFloat32( it.y1 );
        }
    }
    
    internal_symbol.addOSCMessage(x_mess);
    internal_symbol.addOSCMessage(y_mess);
    messages_added += 2;

    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/

bool symbol_parse_error( int p, const String& address )
{
    if( p == -1 )
    {
        std::cout << "failed to parse symbol:\t" << address << std::endl;
        return true; // there is an error
    }
    return false;
}

float getFloatValue(OSCArgument a)
{
    if( a.isFloat32() )
        return a.getFloat32();
    else if( a.isInt32() )
        return (float)a.getInt32();
    else
        return 0.0f;
}

void PathComponent::importFromSymbol( const Symbol* s )
{
    int num_pos = s->getOSCMessagePos("/numSegments");
    if( symbol_parse_error( num_pos, "/numSegments" ) ) return;
    
    OSCBundle b = s->getOSCBundle();
    
    float prev_x = -1111, prev_y = -1111;
    m_path.clear();
    
    for( int i = 0; i < b[num_pos].getMessage()[0].getInt32(); i++ )
    {
        const String base_addr = "/segment/" + std::to_string(i);
        
        const String type_addr = base_addr + "/type";
        int type_pos = s->getOSCMessagePos( type_addr );
        if( symbol_parse_error( type_pos, type_addr ) ) return;

        const String x_addr = base_addr + "/x_points";
        int xp = s->getOSCMessagePos( x_addr );
        if( symbol_parse_error( xp, x_addr ) ) return;
        
        const String y_addr = base_addr + "/y_points";
        int yp = s->getOSCMessagePos( y_addr );
        if( symbol_parse_error( yp, y_addr ) ) return;
        
        String seg_type = b[type_pos].getMessage()[0].getString();
        OSCMessage xm = b[xp].getMessage();
        OSCMessage ym = b[yp].getMessage();
        
        if (xm.size() != ym.size() )
        {
            std::cout << "x and y point lists must be the same length!\n";
            return;
        }
        
        float x0 = getFloatValue(xm[0]);
        float y0 = getFloatValue(ym[0]);
        
        if( x0 != prev_x || y0 != prev_y )
            m_path.startNewSubPath( x0, y0 );

        if( seg_type == "line" && xm.size() == 2 )
        {
            m_path.lineTo( getFloatValue(xm[1]), getFloatValue(ym[1]) );
        }
        else if( seg_type == "cubic" && xm.size() == 3 )
        {
            m_path.cubicTo( getFloatValue(xm[0]), getFloatValue(ym[0]), getFloatValue(xm[1]), getFloatValue(ym[1]), getFloatValue(xm[2]), getFloatValue(ym[2]) );
        }
        
        repaint();
    }
    
}
/******************
 * MOUSE INTERACTIONS
 *****************/

void PathComponent::addHandle( float x, float y )
{
    PathHandle *h = new PathHandle( x + getX(), y + getY(), this );
    auto *p = static_cast<PageComponent*>( getPageComponent() ) ;
    p->addAndMakeVisible( h );
    path_handles.emplace_back( h );
}


void PathComponent::makeHandles()
{
    if( is_selected && path_handles.size() == 0 )
    {
        Path::Iterator it( m_path );
        while( it.next() )
        {
            if (it.elementType == it.startNewSubPath)
            {
                addHandle( it.x1, it.y1 );
            }
            else if (it.elementType == it.cubicTo)
            {
                addHandle( it.x1, it.y1 );
                addHandle( it.x2, it.y2 );
                addHandle( it.x3, it.y3 );
            }
        }
    }
    repaint();
}

void PathComponent::removeHandles()
{
    auto *sc = getPageComponent();
    
    for ( auto h : path_handles )
    {
        if( sc )
            sc->removeChildComponent( h );
        
        delete h;
    }
    path_handles.clear();
}

void PathComponent::updatePathPoints()
{
    auto position = getPosition().toFloat();
    
    Path p;
    auto handle = path_handles.begin();

    Path::Iterator it( m_path );
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            p.startNewSubPath( (*(handle++))->getBounds().toFloat().getCentre() - position );
        }
        else if (it.elementType == it.cubicTo)
        {
            p.cubicTo((*(handle++))->getBounds().toFloat().getCentre() - position,
                      (*(handle++))->getBounds().toFloat().getCentre() - position,
                      (*(handle++))->getBounds().toFloat().getCentre() - position );
        }
    }
    
    // not sure if this offset stuff is necessary, the jumping problem is still there...

    Rectangle<float> testBounds = p.getBounds();
    float offsetx = ( testBounds.getX() < 0 ) ? -testBounds.getX() : 0;
    float offsety = ( testBounds.getY() < 0 ) ? -testBounds.getY() : 0;
    p.applyTransform( AffineTransform().translated(offsetx, offsety) );
    
    Rectangle<float> pathBounds = ( p.getBounds() - Point<float>(offsetx, offsety) ).expanded( strokeType.getStrokeThickness() * 0.5 );
    
    m_path.swapWithPath( p );
    setBoundsFloatRect( pathBounds + position );
    
    symbol_debug_function(__func__);
    printPath(m_path);
    
    repaint();
}

void PathComponent::deselectComponent()
{
    removeHandles();
    BaseComponent::deselectComponent();
}


void PathComponent::mouseDown( const MouseEvent& event )
{
    BaseComponent::mouseDown(event);
}

void PathComponent::mouseMove( const MouseEvent& event )
{
    
    /*
    UI_EditType edit_mode = getMainEditMode();
    if( edit_mode == draw )
    {
        Path p;

        float strokeOffset = strokeType.getStrokeThickness() * 0.5;
        Point<float> zeroPt(0, 0);
        p.startNewSubPath( zeroPt );

        Point<float> endPt = event.position - m_down + zeroPt;
        p.cubicTo( endPt * 0.25 , endPt * 0.75, endPt );

        m_path.swapWithPath( p );
        m_path.applyTransform( AffineTransform().translated (strokeOffset, strokeOffset) );

        Rectangle<float> bb = (m_path.getBounds() + m_down).expanded( strokeOffset ) ;
        setBoundsFloatRect( bb );
    }
    */
}

void PathComponent::mouseDrag( const MouseEvent& event )
{
    symbol_debug_function(__func__);

    BaseComponent::mouseDrag(event);
    
    UI_EditType edit_mode = getMainEditMode();
    if(  edit_mode == draw )
    {
        if( event.mods.isShiftDown() )
        {
            float angle = event.position.getAngleToPoint( m_down );
            if( fabs(angle) < quarter_pi )
                m_drag = Point<float>( m_down.getX(), event.position.getY() );
            else
                m_drag = Point<float>( event.position.getX(), m_down.getY() );
        }
        else
            m_drag = event.position;

        //Point<float>(event.position.getX(), m_down.getY());
        
        Path p;
        float strokeOffset = strokeType.getStrokeThickness() * 0.5;
        Point<float> zeroPt(0, 0);
        p.startNewSubPath( zeroPt );
        
        Point<float> endPt = m_drag - m_down + zeroPt;
        p.cubicTo( endPt * 0.25 , endPt * 0.75, endPt );
        
        Rectangle<float> testBounds = p.getBounds();
        float offsetx = ( testBounds.getX() < 0 ) ? -testBounds.getX() : 0;
        float offsety = ( testBounds.getY() < 0 ) ? -testBounds.getY() : 0;
        p.applyTransform( AffineTransform().translated(offsetx + strokeOffset, offsety + strokeOffset) );
        
        Rectangle<float> pathBounds = ( p.getBounds() - Point<float>(offsetx, offsety) ).expanded( strokeOffset );
        
        m_path.swapWithPath( p );
        setBoundsFloatRect( pathBounds + m_down );
      
    }
    else if ( edit_mode == edit && path_handles.size() > 0 )
    {
        for (auto h : path_handles )
        {
            h->setTopLeftPosition ( h->getPosition() + (event.position - m_down).toInt() );
        }
    }
}

void PathComponent::mouseUp( const MouseEvent& event )
{
    makeHandles();
}

 


/******************
 * Paint callback subroutine
 *****************/

void PathComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    
    // to do: add other stroke options
    //float dashes[] = {1.0, 2.0};
    //strokeType.createDashedStroke(p, p, dashes, 2 );
    
    strokeType.setStrokeThickness( strokeWeight );
    g.strokePath(m_path, strokeType );
    
    /* test
     g.setColour( Colours::red );
     float strokeOffset = strokeType.getStrokeThickness() * 0.5;
     Point<float> zeroPt(strokeOffset, strokeOffset);
     g.drawLine( Line<float>(m_down - m_down + zeroPt, m_drag - m_down + zeroPt) );
    */
    
    g.setColour( current_color );
    for (auto it = path_handles.begin(); it != path_handles.end(); it++ )
    {
        Point<float> start = getLocalPoint( getParentComponent(), (*it++)->getBounds().getCentre().toFloat() );
        Point<float> end = getLocalPoint( getParentComponent(), (*it)->getBounds().getCentre().toFloat() );
        
        Line<float> linea( start, end );
        g.drawLine( linea, 1 );
    }
    /*
    if( !m_preview_path.isEmpty() && getMainEditMode() == draw && is_selected )
    {
        g.setColour( Colours::blue );
        g.strokePath(m_preview_path, strokeType );
    }*/
    
}

