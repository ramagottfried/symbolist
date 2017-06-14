
#include "PathBaseComponent.h"
#include "MainComponent.h"

PathBaseComponent::PathBaseComponent(  const Symbol& s ) : BaseComponent( s )
{
    // has its own method for that
    importPathFromSymbol( s );
}


PathBaseComponent::~PathBaseComponent()
{
    printf("freeing path %p\n", this);
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
    
    s->addOSCMessage( OSCMessage(base_address + "/numSegments", count ) );
    messages_added += 1;
 
//    internal_symbol.printBundle();
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/

void PathBaseComponent::importPathFromSymbol(const Symbol &s)
{
    
    // BaseComponent::importFromSymbol( s );
    
    std::cout << "IMPORT PATH" << std::endl;
    std::cout << s.getOSCMessageValue(String("/type")).getString() << std::endl;
    
    
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
            m_path.cubicTo( Symbol::getOSCValueAsFloat(xm[1]), Symbol::getOSCValueAsFloat(ym[1]),
                            Symbol::getOSCValueAsFloat(xm[2]), Symbol::getOSCValueAsFloat(ym[2]),
                            Symbol::getOSCValueAsFloat(xm[3]), Symbol::getOSCValueAsFloat(ym[3]) );
            prev_x = Symbol::getOSCValueAsFloat(xm[3]);
            prev_y = Symbol::getOSCValueAsFloat(ym[3]);
        }
    }
    // force repaint ?
    
}

/******************
 * MOUSE INTERACTIONS
 *****************/

void PathBaseComponent::enterPathEdit ()
{
    in_edit_mode = true;
    
    m_path_origin = getPosition().toFloat();
    
    auto pc = static_cast<PageComponent*>( getPageComponent() );
    setBounds( pc->getLocalBounds() );
    pc->stealMouse();
    
    if( !m_path.isEmpty() )
        m_path.applyTransform( AffineTransform().translated( m_path_origin ) );
    
    if( getMainEditMode() == select_alt_mode && path_handles.size() == 0)
        makeHandles();
}

void PathBaseComponent::exitPathEdit ()
{
    if( !m_preview_path.isEmpty() )
        m_preview_path.clear();
    
    if( !m_path.isEmpty() )
    {
        Rectangle<float> pathBounds = tranformAndGetBoundsInParent( m_path );
        setBoundsFloatRect( pathBounds );
        ((PageComponent*)getPageComponent() )->giveBackMouse();
    }
    else
    {
        delete this;
    }
    
    if (path_handles.size() > 0 )
        removeHandles();
    
    in_edit_mode = false;
}

void PathBaseComponent::deselectComponent()
{
    BaseComponent::deselectComponent();
}

void PathBaseComponent::selectComponent()
{
    BaseComponent::selectComponent();
}

void PathBaseComponent::updatePathFromPreivew()
{
    if( !m_preview_path.isEmpty() )
    {
        m_path.swapWithPath( m_preview_path );
        m_preview_path.clear();
    }
}

void PathBaseComponent::notifyEditModeChanged( UI_EditType current_mode )
{
    if( is_selected )
    {
        UI_EditType ed = getMainEditMode();
        if( ed == draw_mode || ed == select_alt_mode )
        {
            enterPathEdit();
        }
        else
        {
            if( in_edit_mode )
                exitPathEdit();
        }
    }
}

/******************
 * Mouse interaction
 *****************/

void PathBaseComponent::mouseDown( const MouseEvent& event )
{
    BaseComponent::mouseDown(event);
}

void PathBaseComponent::mouseMove( const MouseEvent& event )
{
}

void PathBaseComponent::mouseDrag( const MouseEvent& event )
{
    BaseComponent::mouseDrag(event);
    
    if ( getMainEditMode() == select_mode && path_handles.size() > 0 )
    {
        for (auto h : path_handles )
        {
            h->setTopLeftPosition ( h->getPosition() + (event.position - m_down).toInt() );
        }
    }
}

void PathBaseComponent::mouseUp( const MouseEvent& event )
{
    BaseComponent::mouseUp(event);
}

/******************
 * preview routine
 *****************/
void PathBaseComponent::drawHandles( Graphics& g)
{
    float ax = -1, ay = -1;
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



void PathBaseComponent::addHandle( float x, float y )
{
    PathHandle *h = new PathHandle( x + getX(), y + getY(), this );
    auto *p = static_cast<Component*>( getPageComponent() ) ;
    p->addAndMakeVisible( h );
    path_handles.emplace_back( h );
}


void PathBaseComponent::makeHandles()
{
    // log origin before editing
    m_path_origin = getPosition().toFloat();
    
    if( is_selected && path_handles.size() == 0 )
    {
        Path::Iterator it( m_path );
        while( it.next() )
        {
            if (it.elementType == it.startNewSubPath)
            {
                addHandle( it.x1, it.y1 );
            }
            else if (it.elementType == it.lineTo)
            {
                addHandle( it.x1, it.y1 );
            }
            else if (it.elementType == it.quadraticTo)
            {
                addHandle( it.x1, it.y1 );
                addHandle( it.x2, it.y2 );
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

// NOT FUNCTIONAL PROGRAMMING, but oh well
Rectangle<float> PathBaseComponent::tranformAndGetBoundsInParent( Path& p )
{
    float strokeOffset = strokeType.getStrokeThickness() * 0.5;
    
    Rectangle<float> abs_bounds = p.getBounds();
    p.applyTransform( AffineTransform().translated( -abs_bounds.getX() + strokeOffset, -abs_bounds.getY() + strokeOffset ) );
    return abs_bounds.expanded( strokeOffset );
}


void PathBaseComponent::updatePathPoints()
{
    Path p;
    auto handle = path_handles.begin();
    
    Path::Iterator it( m_path );
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            p.startNewSubPath( (*(handle++))->getBounds().toFloat().getCentre() );
        }
        else if (it.elementType == it.lineTo)
        {
            p.lineTo( (*(handle++))->getBounds().toFloat().getCentre() );
        }
        else if (it.elementType == it.quadraticTo)
        {
            p.quadraticTo(  (*(handle++))->getBounds().toFloat().getCentre() ,
                          (*(handle++))->getBounds().toFloat().getCentre() );
        }
        else if (it.elementType == it.cubicTo)
        {
            p.cubicTo((*(handle++))->getBounds().toFloat().getCentre(),
                      (*(handle++))->getBounds().toFloat().getCentre(),
                      (*(handle++))->getBounds().toFloat().getCentre() );
        }
    }
    
    m_path.swapWithPath( p );
    repaint();
}


/******************
 * Paint callback subroutine
 *****************/

void PathBaseComponent::paint ( Graphics& g )
{
    int cur_t,local_t = 0;
    g.setColour( getCurrentColor() );
    float strok = strokeType.getStrokeThickness();
    
    //printRect(getBounds(), "paint " + getSymbolTypeStr() );
    if ( isTopLevelComponent() )
    {
        cur_t = ((SymbolistMainComponent*)getMainComponent())->getCurrentTime();
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
    
    if( getMainComponent() == NULL ) // workaround since we don't know which context we're in, draw and return if in palette
    {
        g.strokePath(m_path, strokeType );
        return;
    }
    
    UI_EditType ed = getMainEditMode();

    if( in_edit_mode && (ed == draw_mode || ed == select_alt_mode) )
    {
        g.setColour( Colour::fromFloatRGBA(0.0f,0.0f,0.0f,0.2f)  );
        g.fillRect( getLocalBounds() );

    }

    
    g.setColour( getCurrentColor() );

    if( !m_preview_path.isEmpty() )
    {
        g.strokePath(m_preview_path, strokeType );
    }
    else
    {
        g.strokePath(m_path, strokeType );
    }

    if( in_edit_mode && ed == select_alt_mode )
    {
        drawHandles(g);
    }
    
}

