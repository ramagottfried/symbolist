
#include "BaseComponent.h"
#include "PageComponent.h"
#include "MainComponent.h"

// do we need this ?
template <typename T> void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}

BaseComponent::BaseComponent(const Symbol &s)
{
    std::cout << "BASE" << std::endl;
    internal_symbol = s ;
    importFromSymbol() ;
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


void BaseComponent::setScoreSymbolPointer (Symbol* s)
{
    score_symbol = s;
}

void BaseComponent::addSymbolToScore ()
{
    setScoreSymbolPointer( new Symbol(internal_symbol) );
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
void BaseComponent::importFromSymbol()
{
    int typeMessagePos = internal_symbol.getOSCMessagePos("/type");
    
    if ( typeMessagePos == -1 ) {
        
        cout << "Could not find '/type' message in OSC Bundle.. (size=" << internal_symbol.getOSCBundle().size() << ")" << endl;
        
    } else {
        
        String typeStr = internal_symbol.getOSCMessageValue(typeMessagePos).getString();
        cout << "Importing component from Symbol: " << typeStr << endl;
        
        float x = internal_symbol.getOSCMessageValue("/x").getFloat32();
        float y = internal_symbol.getOSCMessageValue("/y").getFloat32();;
        float w = internal_symbol.getOSCMessageValue("/w").getFloat32();
        float h = internal_symbol.getOSCMessageValue("/h").getFloat32();
        setBounds( x , y , w , h);
    }
}


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


