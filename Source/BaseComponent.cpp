
#include "BaseComponent.h"
#include "ScoreComponent.h"
#include "MainComponent.h"


BaseComponent::BaseComponent() : BaseComponent("symbol", Point<float>(10 , 10)) {}
BaseComponent::BaseComponent(const String &type, const Point<float> & pos ) : symbol_type(type) , m_down(pos) {}
BaseComponent::~BaseComponent() {}


/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

int BaseComponent::addSymbolMessages( String base_address )
{
    int messages_added = 0;

    getSymbol()->addOSCMessage ((String(base_address) += "/type") , getSymbolType());
    getSymbol()->addOSCMessage ((String(base_address) += "/x") , symbol_getX());
    getSymbol()->addOSCMessage ((String(base_address) += "/y") , symbol_getY());
    getSymbol()->addOSCMessage ((String(base_address) += "/w") , getWidth());
    getSymbol()->addOSCMessage ((String(base_address) += "/h") , getHeight());
    messages_added += 5;
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        String base = base_address << "/sub_" << String(i) ;
        messages_added += getSubcomponent(i)->addSymbolMessages( base );
    }
    
    return messages_added;
}


/******************
 * Imports components' data from teh symbol's OSC bundle
 *****************/
void BaseComponent::importFromSymbol() { }


/******************
 * Called by selection mechanism
 *****************/
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


/******************
 * Paint callback: calls a symbol-specific suroutine
 *****************/
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
    if (sc != NULL && symbol_type != "UI_only") { sc->modifySymbolInScore( this ); }
    
    symbol_moved();
}

void BaseComponent::resized ()
{
    ScoreComponent* sc = static_cast<ScoreComponent*>( getScoreComponent() );
    if (sc != NULL && symbol_type != "UI_only") { sc->modifySymbolInScore( this ); }
    
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


