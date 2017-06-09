
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

int PathComponent::addSymbolMessages(const String &base_address)
{
    // adds the basic messages
    int messages_added = BaseComponent::addSymbolMessages(base_address);
    
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
    
    getSymbol()->addOSCMessage(x_mess);
    getSymbol()->addOSCMessage(y_mess);
    messages_added += 2;

    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/

void PathComponent::importFromSymbol()
{
    BaseComponent::importFromSymbol(); // do nothing special
    // import the points
    int xp = getSymbol()->getOSCMessagePos("/x-points");
    int yp = getSymbol()->getOSCMessagePos("/y-points");
   
    
    if ( xp >= 0 && yp >= 0 )
    {
        OSCMessage xm = getSymbol()->getOSCBundle()[xp].getMessage();
        OSCMessage ym = getSymbol()->getOSCBundle()[yp].getMessage();
    
        m_path.clear();
        m_path.startNewSubPath( xm[0].getFloat32() , ym[0].getFloat32() );
        
        for (int i = 1; i < xm.size(); i++) {
            m_path.lineTo( xm[i].getFloat32() , ym[i].getFloat32() );
        }
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
    
    g.setColour( Colours::red );
    g.drawRect( getLocalBounds() );
    
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

