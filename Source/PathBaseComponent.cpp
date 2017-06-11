
#include "PathBaseComponent.h"

void PathBaseComponent::printPath( Path p )
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

int PathBaseComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    // adds the basic messages
    int messages_added = BaseComponent::addSymbolMessages(s, base_address);
    
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
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"line" ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  it.x1 ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  it.y1 ) );
            messages_added += 3;
            count++;

            ax = it.x1;
            ay = it.y1;

        }
        else if (it.elementType == it.quadraticTo)
        {
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"quadratic" ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  it.x1, it.x2 ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  it.y1, it.y2 ) );
            messages_added += 3;
            count++;

            ax = it.x2;
            ay = it.y2;

        }
        else if (it.elementType == it.cubicTo)
        {
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"cubic" ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  it.x1, it.x2, it.x3 ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  it.y1, it.y2, it.y3 ) );
            messages_added += 3;
            count++;

            ax = it.x3;
            ay = it.y3;
        }
        else if (it.elementType == it.closePath)
        {
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/type",      (String)"close" ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/x_points",  ax,  startx ) );
            internal_symbol.addOSCMessage( OSCMessage( seg_addr + "/y_points",  ay,  starty ) );
            messages_added += 3;
            count++;

            ax = startx;
            ay = starty;
        }

    }
    
    internal_symbol.addOSCMessage( OSCMessage("/numSegments", count ) );
    messages_added += 1;
 
    internal_symbol.printBundle();
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/

void PathBaseComponent::parseSymbolPath()
{
    Symbol *s = &internal_symbol;
    
    std::cout << "IMPORT PATH BASE" << std::endl;
    std::cout << s->getOSCMessageValue(String("/type")).getString() << std::endl;
    std::cout << internal_symbol.getOSCMessageValue(String("/type")).getString() << std::endl;

    
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
        
        float x0 = s->getFloatValue(xm[0]);
        float y0 = s->getFloatValue(ym[0]);
        
        if( x0 != prev_x || y0 != prev_y )
        {
            m_path.startNewSubPath( x0, y0 );
        }
        
        if( seg_type == "line" && xm.size() == 2 )
        {
            m_path.lineTo( s->getFloatValue(xm[1]), s->getFloatValue(ym[1]) );
            prev_x = s->getFloatValue(xm[1]);
            prev_y = s->getFloatValue(ym[1]);
        }
        else if( seg_type == "quadratic" && xm.size() == 3 )
        {
            m_path.quadraticTo( s->getFloatValue(xm[1]), s->getFloatValue(ym[1]), s->getFloatValue(xm[2]), s->getFloatValue(ym[2]) );
            prev_x = s->getFloatValue(xm[2]);
            prev_y = s->getFloatValue(ym[2]);
        }
        else if( seg_type == "cubic" && xm.size() == 4 )
        {
            m_path.cubicTo( s->getFloatValue(xm[1]), s->getFloatValue(ym[1]), s->getFloatValue(xm[2]), s->getFloatValue(ym[2]), s->getFloatValue(xm[3]), s->getFloatValue(ym[3]) );
            prev_x = s->getFloatValue(xm[3]);
            prev_y = s->getFloatValue(ym[3]);
        }
    }
    // force repaint ?
    
}

/******************
 * MOUSE INTERACTIONS
 *****************/

void PathBaseComponent::addHandle( float x, float y )
{
    PathHandle *h = new PathHandle( x + getX(), y + getY(), this );
    auto *p = static_cast<Component*>( getPageComponent() ) ;
    p->addAndMakeVisible( h );
    path_handles.emplace_back( h );
}


void PathBaseComponent::makeHandles()
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

void PathBaseComponent::removeHandles()
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

void PathBaseComponent::updatePathPoints()
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

void PathBaseComponent::deselectComponent()
{
    removeHandles();
    BaseComponent::deselectComponent();
}


void PathBaseComponent::mouseDown( const MouseEvent& event )
{
    BaseComponent::mouseDown(event);
}

void PathBaseComponent::mouseMove( const MouseEvent& event )
{
///    printPoint(event.position, "PathBaseComponent::mouseMove" );
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

void PathBaseComponent::mouseDrag( const MouseEvent& event )
{
    
    BaseComponent::mouseDrag(event);
    
    UI_EditType edit_mode = getMainEditMode();
    if(  edit_mode == draw_mode )
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
        
        
               
    }
    else if ( edit_mode == select_mode && path_handles.size() > 0 )
    {
        for (auto h : path_handles )
        {
            h->setTopLeftPosition ( h->getPosition() + (event.position - m_down).toInt() );
        }
    }
}

void PathBaseComponent::mouseUp( const MouseEvent& event )
{
}


void PathBaseComponent::drawHandles( Graphics& g)
{
    float ax = -1, ay = -1;
    Path::Iterator it(m_path);
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
            g.drawLine(ax, ay, it.x1, it.y1);
            ax = it.x2;
            ay = it.y2;
        }
        else if (it.elementType == it.cubicTo)
        {
            g.drawLine(ax, ay, it.x1, it.y1);
            g.drawLine(it.x2, it.y2, it.x3, it.y3);
            ax = it.x3;
            ay = it.y3;
        }
        else if (it.elementType == it.closePath)
        {

        }
    }

}

/******************
 * Paint callback subroutine
 *****************/

void PathBaseComponent::paint ( Graphics& g )
{
    
    printRect(getBounds(), "check "+symbol_type);
    g.setColour( getCurrentColor() );
    
    // to do: add other stroke options
    //float dashes[] = {1.0, 2.0};
    //strokeType.createDashedStroke(p, p, dashes, 2 );
    
    strokeType.setStrokeThickness( strokeWeight );
    
    if(!m_preview_path.isEmpty() )
    {
        g.strokePath(m_preview_path, strokeType );
    }
    else
    {
        g.strokePath(m_path, strokeType );
    }
    
    if( is_selected && getMainEditMode() == select_alt_mode )
    {
        if( path_handles.size() == 0)
            makeHandles();
        
        drawHandles(g);
    }
    else if (path_handles.size() > 0 )
        removeHandles();
}
