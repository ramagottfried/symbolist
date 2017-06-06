
#include "PathComponent.h"
#include "PageComponent.h"

/*
PathComponent::PathComponent( Point<float> startPT ) : BaseComponent("path", startPT), strokeType( strokeWeight )
{
    setComponentID ( "Path" );
}

PathComponent::PathComponent() : PathComponent( Point<float>(0,0) ) {}
*/

PathComponent::~PathComponent()
{
    printf("freeing path %p\n", this);
    removeHandles();
}


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
    
}



/******************
 * MOUSE INTERACTIONS
 *****************/

void PathComponent::addHandle( float x, float y, int index)
{
    PathHandle *h = new PathHandle( x + getX(), y + getY(), this );
    auto *p = static_cast<PageComponent*>( getPageComponent() ) ;
    p->addAndMakeVisible( h );
    p->addItemToSelection( h );
    path_handles.emplace_back( h );
}


void PathComponent::makeHandles()
{
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

    for ( int i = 0; i < path_handles.size(); i++ )
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
    Point<float> offset( (*handle)->getWidth() / 2., (*handle)->getHeight() / 2. );

    Path::Iterator it( m_path );
    while( it.next() )
    {
        if (it.elementType == it.startNewSubPath)
        {
            p.startNewSubPath( (*(handle++))->getPosition().toFloat() + offset);
        }
        else if (it.elementType == it.cubicTo)
        {
            p.cubicTo((*(handle++))->getPosition().toFloat() + offset,
                      (*(handle++))->getPosition().toFloat() + offset,
                      (*(handle++))->getPosition().toFloat() + offset );
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
}


void PathComponent::mouseDrag( const MouseEvent& event )
{
    BaseComponent::mouseDrag(event);
    
    if( is_being_edited )
    {
        m_drag = event.position;
        
        Path p;
        // paths are relative to the z
        Point<float> zeroPt = {0.0, 0.0};
        
        p.startNewSubPath( zeroPt );
        Point<float> endPt = m_drag - m_down;
        
        p.cubicTo( endPt * 0.3 , endPt * 0.6, endPt );
            
        Rectangle<float> pathBounds = p.getBounds();
//        printRect( pathBounds, "path bounds");
        
        setBounds( m_down.getX() + pathBounds.getX(), m_down.getY() + pathBounds.getY(), pathBounds.getWidth(), pathBounds.getHeight() );
        
        m_path.swapWithPath( p );
        //printPath( m_path );
        
    }
}

void PathComponent::mouseUp( const MouseEvent& event )
{
    makeHandles();
}

 
