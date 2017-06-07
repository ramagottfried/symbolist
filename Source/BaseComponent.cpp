
#include "BaseComponent.h"
#include "PageComponent.h"
#include "MainComponent.h"

// do we need this ?
template <typename T> void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}

BaseComponent::BaseComponent(const String &type,
                             float x, float y, float w, float h,
                             float stroke,
                             Colour color) :
                    symbol_type(type),
                    strokeWeight(stroke),
                    sym_color(color),
                    m_down(Point<float>(x,y))
{
    setBounds( x , y , w , h);
}

BaseComponent::~BaseComponent() {}


bool BaseComponent::isTopLevelComponent()
{
    return ( getParentComponent() == getPageComponent() );
}


void BaseComponent::setSymbol(Symbol s)
{
    // score_symbol = s;
    internal_symbol = s;
}

Symbol* BaseComponent::getSymbol()
{
    return &internal_symbol ;
}


/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

int BaseComponent::addSymbolMessages( const String &base_address )
{
    int messages_added = 0;

    getSymbol()->addOSCMessage ((String(base_address) += "/type") , getSymbolType());
    getSymbol()->addOSCMessage ((String(base_address) += "/x") , symbol_getX());
    getSymbol()->addOSCMessage ((String(base_address) += "/y") , symbol_getY());
    getSymbol()->addOSCMessage ((String(base_address) += "/w") , (float) getWidth());
    getSymbol()->addOSCMessage ((String(base_address) += "/h") , (float) getHeight());
    messages_added += 5;
    
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
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
    PageComponent* p = static_cast<PageComponent*>( getPageComponent() );
    // sc can be null if the symbol is moved when not yet on screen
    // best would be to call this from the moving action
    if (p != NULL && symbol_type != "UI_only") { p->modifySymbolInScore( this ); }
    symbol_moved();
}

void BaseComponent::resized ()
{
    PageComponent* p = static_cast<PageComponent*>( getPageComponent() );
    if (p != NULL && symbol_type != "UI_only") { p->modifySymbolInScore( this ); }
    
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
        PageComponent* p = static_cast<PageComponent*>( getPageComponent() );
        p->translateSelected( (event.position - m_down).toInt() );
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


