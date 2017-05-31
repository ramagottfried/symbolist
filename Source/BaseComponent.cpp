
#include "BaseComponent.h"
#include "ScoreComponent.h"
#include "MainComponent.h"


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
    ScoreComponent* sc = static_cast<ScoreComponent*>( getScoreComponent() );
    // sc can be null if the symbol is moved when not yet on screen
    // best would be to call this from the moving action
    if (sc != NULL) { sc->scoreSymbolModified( this ); }
    
    symbol_moved();
}

void BaseComponent::resized ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getScoreComponent() );
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
//    resizableBorder->setVisible( is_selected && !is_being_edited );

    symbol_mouseMove(event);
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;

    symbol_mouseDown(event);
    

}

template <typename T>
void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}

void BaseComponent::mouseDrag( const MouseEvent& event )
{
    if( is_selected )
    {
        ScoreComponent* sc = static_cast<ScoreComponent*>( getScoreComponent() );
        sc->translateSelected( (event.position - m_down).toInt() );
    }

    symbol_mouseDrag(event);
}

void BaseComponent::mouseExit( const MouseEvent& event )
{
    symbol_mouseExit(event);
}

void BaseComponent::mouseUp( const MouseEvent& event )
{
    
    symbol_mouseUp(event);

    if( is_selected )
    {
        is_being_edited = true;
        resizableBorder->setVisible( true );

        repaint();
    }
}


void BaseComponent::mouseDoubleClick( const MouseEvent& event )
{
    symbol_mouseDoubleClick(event);
}
