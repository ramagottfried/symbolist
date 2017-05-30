
#include "BaseComponent.h"
#include "ScoreComponent.h"


BaseComponent::BaseComponent(String type, Point<float> pos )
{
    symbol_type = type;
    m_down = pos;
}

BaseComponent::BaseComponent() : BaseComponent("symbol", Point<float>(10 , 10)) {}

BaseComponent::~BaseComponent() {}


void BaseComponent::select()
{
    is_selected = true;
    repaint();
}

void BaseComponent::deselect()
{
    is_selected = false;
    is_being_edited = false;
    repaint();
}

void BaseComponent::moved ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getTPLScoreComponent() );
    // sc can be null if the symbol is moved when not yet on screen
    // best would be to call this from the moving action
    if (sc != NULL) { sc->scoreSymbolModified( this ); }
    
    symbol_moved();
}

void BaseComponent::resized ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getTPLScoreComponent() );
    if (sc != NULL) { sc->scoreSymbolModified( this ); }
    
    symbol_resized();

    if( !resizableBorder )
    {
        addChildComponent( resizableBorder = new ResizableBorderComponent(this, nullptr) );
        resizableBorder->setBorderThickness( BorderSize<int>(1) );
    }
    
//    printRect( getLocalBounds(), "resizableBorder" );
    
    resizableBorder->setBounds( getLocalBounds() );
}


void BaseComponent::paint ( Graphics& g )
{
    current_color = is_selected ? sel_color : sym_color;
    
    symbol_paint( g );
    
}


void BaseComponent::mouseEnter( const MouseEvent& event )
{
    symbol_mouseEnter(event);
}

void BaseComponent::mouseMove( const MouseEvent& event )
{
    resizableBorder->setVisible( is_selected && !is_being_edited );

    symbol_mouseMove(event);
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;

    symbol_mouseDown(event);
    
    if( is_selected )
    {
        is_being_edited = true;
        repaint();
    }

}

template <typename T>
void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}

void BaseComponent::mouseDrag( const MouseEvent& event )
{
    if( is_selected && !is_being_edited )
    {
        Point<float> mouseoffset = event.getEventRelativeTo( getParentComponent() ).position - m_down;
        
        printPoint<float>(m_down, "mdrag m_down");
        printPoint<float>(event.position, "mdrag event");
        printPoint<float>(mouseoffset, "mdrag offset");

        setBounds ( mouseoffset.getX(), mouseoffset.getY(), getWidth(), getHeight() );
    }

    symbol_mouseDrag(event);
}

void BaseComponent::mouseExit( const MouseEvent& event )
{
    resizableBorder->setVisible( is_selected && !is_being_edited  );

    symbol_mouseExit(event);
}

void BaseComponent::mouseUp( const MouseEvent& event )
{
    symbol_mouseUp(event);
}


void BaseComponent::mouseDoubleClick( const MouseEvent& event )
{
    symbol_mouseDoubleClick(event);
}
