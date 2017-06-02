
#include "BaseComponent.h"
#include "ScoreComponent.h"
#include "MainComponent.h"


// do we need this ?
template <typename T> void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}



BaseComponent::BaseComponent(const String &type, const Point<float> & pos )
{
    symbol_type = type;
    m_down = pos;
}

BaseComponent::BaseComponent() : BaseComponent("symbol", Point<float>(10 , 10)) {}

BaseComponent::~BaseComponent() {}


void BaseComponent::selectComponent()
{
    is_selected = true;
    repaint();
}

void BaseComponent::deselectComponent()
{
    is_selected = false;
    is_being_edited = false;
    resizableBorder->setVisible( false );
    repaint();
}


void BaseComponent::paint ( Graphics& g )
{
    current_color = is_selected ? sel_color : sym_color;
    symbol_paint( g );
}


/************************
 * Component modification callbacks
 ************************/

void BaseComponent::moved ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getScoreComponent() );
    // sc can be null if the symbol is moved when not yet on screen
    // best would be to call this from the moving action
    if (sc != NULL) { sc->modifySymbolInScore( this ); }
    
    symbol_moved();
}

void BaseComponent::resized ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getScoreComponent() );
    if (sc != NULL) { sc->modifySymbolInScore( this ); }
    
    symbol_resized();

    if( !resizableBorder )
    {
        addChildComponent( resizableBorder = new ResizableBorderComponent(this, nullptr) );
        resizableBorder->setBorderThickness( BorderSize<int>(1) );
    }
    
    // printRect( getLocalBounds(), "resizableBorder" );
    resizableBorder->setBounds( getLocalBounds() );
}


/************************
 * MOUSE INTERACTIONS
 ************************/
void BaseComponent::mouseMove( const MouseEvent& event )
{
    // resizableBorder->setVisible( is_selected && !is_being_edited );
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
}


void BaseComponent::mouseDrag( const MouseEvent& event )
{
    if( is_selected )
    {
        ScoreComponent* sc = static_cast<ScoreComponent*>( getScoreComponent() );
        sc->translateSelected( (event.position - m_down).toInt() );
    }
}

void BaseComponent::mouseUp( const MouseEvent& event )
{
    if( is_selected )
    {
        is_being_edited = true;
        resizableBorder->setVisible( true );

        repaint();
    }
}


