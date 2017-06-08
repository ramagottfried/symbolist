
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

void PathComponent::importFromSymbol()
{
    BaseComponent::importFromSymbol(); // do nothing special
    // import the points
    int xp = internal_symbol.getOSCMessagePos("/x-points");
    int yp = internal_symbol.getOSCMessagePos("/y-points");
   
    
    if ( xp >= 0 && yp >= 0 )
    {
        OSCMessage xm = internal_symbol.getOSCBundle()[xp].getMessage();
        OSCMessage ym = internal_symbol.getOSCBundle()[yp].getMessage();
    
        m_path.clear();
        m_path.startNewSubPath( xm[0].getFloat32() , ym[0].getFloat32() );
        
        for (int i = 1; i < xm.size(); i++) {
            m_path.lineTo( xm[i].getFloat32() , ym[i].getFloat32() );
        }
    }
}


/******************
 * Paint callback subroutine
 *****************/

void PathComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
   
    auto localB = getLocalBounds();
    m_path.scaleToFit( localB.getX(), localB.getY(), localB.getWidth(), localB.getHeight(), false );
    
    //float dashes[] = {1.0, 2.0};
    //strokeType.createDashedStroke(p, p, dashes, 2 );
   
    strokeType.setStrokeThickness( strokeWeight );
    g.strokePath(m_path, strokeType );
    
    
    for (auto it = path_handles.begin(); it != path_handles.end(); it++ )
    {
        Point<float> start = getLocalPoint( getParentComponent(), (*it++)->getBounds().getCentre().toFloat() );
        Point<float> end = getLocalPoint( getParentComponent(), (*it)->getBounds().getCentre().toFloat() );
        Line<float> linea( start, end );
        g.drawLine( linea, 1 );
    }
    
    if( !m_preview_path.isEmpty() && getMainEditMode() == draw && is_selected )
    {
        g.setColour( Colours::blue );
        g.strokePath(m_preview_path, strokeType );
    }
    
}



/******************
 * MOUSE INTERACTIONS
 *****************/

void PathComponent::addHandle( float x, float y, int index)
{
    PathHandle *h = new PathHandle( x + getX(), y + getY(), this );
    auto *p = static_cast<PageComponent*>( getPageComponent() ) ;
    p->addAndMakeVisible( h );
//    p->addItemToSelection( h );
    path_handles.emplace_back( h );
}


void PathComponent::makeHandles()
{
    
    std::cout << is_selected << " " << (path_handles.size() == 0) << "\n";
    
    if( is_selected && path_handles.size() == 0 )
    {
        //        ScoreComponent *sc = static_cast<ScoreComponent*>( getScoreComponent() );
        //        sc->deselectAllSelected();
        //        is_selected = false;
        
        int count = 0;
        Path::Iterator it( m_path );
        while( it.next() )
        {
            if (it.elementType == it.startNewSubPath)
            {
                printf("start\n");
                addHandle( it.x1, it.y1, count++ );
            }
            else if (it.elementType == it.cubicTo)
            {
                printf("cubic\n");
                addHandle( it.x1, it.y1, count++ );
                addHandle( it.x2, it.y2, count++ );
                addHandle( it.x3, it.y3, count++ );
            }
        }
    }
}

void PathComponent::removeHandles()
{
    auto *sc = getPageComponent();
    
    for ( size_t i = 0; i < path_handles.size(); i++ )
    {
        if( sc )
            sc->removeChildComponent( path_handles[i] );
        
        delete path_handles[i];
    }
    path_handles.clear();
}

void PathComponent::updatePathPoints()
{
    Path p;
    auto handle = path_handles.begin();

    Path::Iterator it( m_path );
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            p.startNewSubPath( (*(handle++))->getBounds().getCentre().toFloat() );
        }
        else if (it.elementType == it.cubicTo)
        {
            p.cubicTo((*(handle++))->getBounds().getCentre().toFloat(),
                      (*(handle++))->getBounds().getCentre().toFloat(),
                      (*(handle++))->getBounds().getCentre().toFloat() );
        }
    }
    
    Rectangle<float> pathBounds = p.getBounds();
    setBounds( pathBounds.getX(), pathBounds.getY(), pathBounds.getWidth(), pathBounds.getHeight() );
    
    m_path.swapWithPath( p );
    
    // might be able to not repaint if setBounds is different than last time
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
    std::cout << is_selected << " " << (path_handles.size() == 0) << "\n";

}

void PathComponent::mouseMove( const MouseEvent& event )
{
    /*
    printf("check mouse move\n");
    if( getMainEditMode() == draw )
    {
        m_preview_path.clear();
        
        m_preview_path.startNewSubPath( m_path.getCurrentPosition() );
        m_preview_path.cubicTo( event.position * 0.3 , event.position * 0.6, event.position );

        Rectangle<int> pathBounds = m_preview_path.getBounds().toNearestInt();

        setBounds( pathBounds.getUnion( getBounds() ) );
        printRect( pathBounds.getUnion( getBounds() ) );
    }
     */
}

void PathComponent::mouseDrag( const MouseEvent& event )
{
    BaseComponent::mouseDrag(event);
    
    UI_EditType edit_mode = getMainEditMode();
    if(  edit_mode == draw )
    {
        m_drag = event.position;
        
        Path p;
        // paths are relative to the z
        Point<float> zeroPt = {0.0, 0.0};
        
        p.startNewSubPath( zeroPt );
        Point<float> endPt = m_drag - m_down;
        
        p.cubicTo( endPt * 0.3 , endPt * 0.6, endPt );
            
        Rectangle<float> pathBounds = p.getBounds();
        setBounds( m_down.getX() + pathBounds.getX(), m_down.getY() + pathBounds.getY(), pathBounds.getWidth(), pathBounds.getHeight() );
        
        m_path.swapWithPath( p );
        
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

 
