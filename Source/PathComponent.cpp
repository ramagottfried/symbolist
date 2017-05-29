
#include "PathComponent.h"

PathComponent::PathComponent() : strokeType(1.0)
{
    setComponentID ( "Path" );
}

PathComponent::PathComponent( Point<float> startPT ) : BaseComponent(startPT), strokeType(1.0)
{
    setComponentID ( "Path" );
}

PathComponent::~PathComponent()
{
    printf("freeing Path %p\n", this);
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

void PathComponent::symbol_paint ( Graphics& g )
{
    g.setColour( current_color );
    
    auto localB = getLocalBounds();
    
    m_path.scaleToFit( localB.getX(), localB.getY(), localB.getWidth(), localB.getHeight(), false );
    /*
     float dashes[] = {1.0, 2.0};
     strokeType.createDashedStroke(p, p, dashes, 2 );
     */
    
    
    g.strokePath(m_path, strokeType );
    
}

void PathComponent::symbol_moved ()  {}
void PathComponent::symbol_resized ()
{
    printPath(m_path);
}

void PathComponent::symbol_mouseEnter( const MouseEvent& event )  {}
void PathComponent::symbol_mouseMove( const MouseEvent& event )  {}
void PathComponent::symbol_mouseDown( const MouseEvent& event )
{
    Path::Iterator it( m_path );
    int count = 0;
    while( it.next() )
    {
        
    }
}


void PathComponent::symbol_mouseDrag( const MouseEvent& event )
{
    if( is_being_edited )
    {
        m_drag = event.position;
        
        {
            Path p;
            //            PathStrokeType strokeType(1.0);
            
            
            // paths are relative to the z
            Point<float> zeroPt = {0.0, 0.0};
            p.startNewSubPath( zeroPt  );
            
            Point<float> endPt = m_drag - m_down;
            
            p.cubicTo(zeroPt, endPt, endPt );
            
            Rectangle<float> pathBounds = p.getBounds();
            printRect( pathBounds, "path bounds");
            
            //                std::cout << newB.getX() << " " << newB.getY() << " " << newB.getWidth() << " " << newB.getHeight() << "\n";
            
            
            setBounds( m_down.getX() + pathBounds.getX(), m_down.getY() + pathBounds.getY(), pathBounds.getWidth(), pathBounds.getHeight() );
            
            // printf("test %f %f %f %f\n", m_down.getX() + pathBounds.getX(), m_down.getY() + pathBounds.getY(), pathBounds.getWidth(), pathBounds.getHeight() );
            
            m_path.swapWithPath( p );
            
            
            
        }
    }
}

void PathComponent::symbol_mouseUp( const MouseEvent& event ){}
void PathComponent::symbol_mouseExit( const MouseEvent& event ) {}
void PathComponent::symbol_mouseDoubleClick( const MouseEvent& event ) {}

