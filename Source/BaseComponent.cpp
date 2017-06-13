
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
    importFromSymbol( s ) ;
}

BaseComponent::~BaseComponent() {}


bool BaseComponent::isTopLevelComponent()
{
    return ( getParentComponent() != NULL && getParentComponent() == getPageComponent() );
}



// This is the function to call when we want to update the score after a modification
void BaseComponent::reportModification()
{
    //symbol_debug_function(__func__);
    
    if ( getParentComponent() != NULL ) // we're in the score..
    {
        if ( isTopLevelComponent() )
        {
            ((SymbolistMainComponent*) getMainComponent())->modifySymbolInScore( this );
        }
        else
        {
            ((BaseComponent*) getParentComponent())->reportModification() ;
        }
    }
    
}



/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

int BaseComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = 0;
    
    s->addOSCMessage ((String(base_address) += "/type") , getSymbolTypeStr());
    s->addOSCMessage ((String(base_address) += "/x") , symbol_getX());
    s->addOSCMessage ((String(base_address) += "/y") , symbol_getY());
    s->addOSCMessage ((String(base_address) += "/w") , (float) getWidth());
    s->addOSCMessage ((String(base_address) += "/h") , (float) getHeight());
    
    s->addOSCMessage ((String(base_address) += "/offset") , symbol_getX() * 10.0f);
    s->addOSCMessage ((String(base_address) += "/duration") , 500.0f);
    
    messages_added += 5;
    
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle
 *****************/
void BaseComponent::importFromSymbol( const Symbol &s )
{
    int typeMessagePos = s.getOSCMessagePos("/type");
    
    if ( typeMessagePos == -1 ) {
        
        cout << "BaseComponent import: Could not find '/type' message in OSC Bundle.. (size=" << s.getOSCBundle().size() << ")" << endl;
        
    } else {
        
        String typeStr = s.getOSCMessageValue(typeMessagePos).getString();
        cout << "Importing BaseComponent from Symbol: " << typeStr << endl;
        
        float x = s.getOSCMessageValue("/x").getFloat32();
        float y = s.getOSCMessageValue("/y").getFloat32();;
        float w = s.getOSCMessageValue("/w").getFloat32();
        float h = s.getOSCMessageValue("/h").getFloat32();
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
    resizableBorder->setVisible( false );
    repaint();
}

/************************
 * Component modification callbacks
 ************************/

void BaseComponent::moved ()
{
    reportModification(); // shoudl be smarter : call this when the move is over (mouse up)
}


void BaseComponent::resized ()
{
    reportModification(); // shoudl be smarter : call this when the move is over (mouse up)

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
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;
}


void BaseComponent::mouseDrag( const MouseEvent& event )
{
    if( is_selected )
    {
        PageComponent* p = ( (PageComponent*) getPageComponent() );
        p->translateSelected( (event.position - m_down).toInt() );
    }
}

void BaseComponent::mouseUp( const MouseEvent& event )
{
    if( is_selected )
    {
        resizableBorder->setVisible( true );
        repaint();
    }
}


