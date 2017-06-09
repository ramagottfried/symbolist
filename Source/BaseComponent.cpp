
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


String BaseComponent::getSymbolType()
{
    return internal_symbol.getOSCMessageValue( String("/type") ).getString() ;
}


void BaseComponent::updateInternalSymbol()
{
    internal_symbol.clearOSCBundle();
    addSymbolMessages( &internal_symbol, String("") );
    
    if ( getParentComponent() != NULL ) // we're in the score..
    {
        if ( isTopLevelComponent() )
        {
            assert(score_symbol != NULL);
            score_symbol->clearOSCBundle();
            addSymbolMessages( score_symbol , String("") );
            ((SymbolistMainComponent*) getMainComponent())->notifySymbolChange( score_symbol );
        }
        else
        {
            ((BaseComponent*) getParentComponent())->updateInternalSymbol() ;
        }
    }
}


void BaseComponent::addSymbolToScore ()
{
    score_symbol = new Symbol(internal_symbol);
    ((SymbolistMainComponent*) getMainComponent())->notifyNewSymbol( score_symbol );
}

void BaseComponent::removeSymbolFromScore ()
{
   ((SymbolistMainComponent*) getMainComponent())->notifySymbolRemoved( score_symbol );
}



bool BaseComponent::isTopLevelComponent()
{
    return ( getParentComponent() != NULL && getParentComponent() == getPageComponent() );
}


Symbol* BaseComponent::getInternalSymbol( )
{
    return &internal_symbol ;
}


/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

int BaseComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = 0;
    
    s->addOSCMessage ("/type" , symbol_type);
    s->addOSCMessage ((String(base_address) += "/x") , symbol_getX());
    s->addOSCMessage ((String(base_address) += "/y") , symbol_getY());
    s->addOSCMessage ((String(base_address) += "/w") , (float) getWidth());
    s->addOSCMessage ((String(base_address) += "/h") , (float) getHeight());
    messages_added += 5;
    
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/
void BaseComponent::importFromSymbol( const Symbol* s) {}


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
    updateInternalSymbol();
}


void BaseComponent::resized ()
{
    updateInternalSymbol();

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


