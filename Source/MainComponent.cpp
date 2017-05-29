
#include "MainComponent.h"
#include "MainWindow.h"


SymbolistMainComponent::SymbolistMainComponent()
{
    score = new Score();
    
    setComponentID("MainComponent");
    setSize (600, 400);
    
    addAndMakeVisible(scoreGUI);
    addAndMakeVisible(palette);
    
    setWantsKeyboardFocus(true);
    addKeyListener(this);
    
}

SymbolistMainComponent::~SymbolistMainComponent()
{
    delete score;
}


void SymbolistMainComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
}

void SymbolistMainComponent::resized()
{
//    scoreGUI.setBounds( 50, 50, getWidth()-100, getHeight()-100 );
    scoreGUI.setBounds( 50, 0, getWidth(), getHeight() );
    palette.setBounds( 0, 0, 50, getHeight() );
    
    printf("main resized\n");
}



// CONTROLLER METHODS

SymbolistMainComponent* SymbolistMainComponent::createWindow()
{
    SymbolistEditorWindow *w = new SymbolistEditorWindow ();
    return w->getSymbolistMainComponent();
}

Component* SymbolistMainComponent::getWindow () { return getParentComponent(); }
void SymbolistMainComponent::windowToFront() { getWindow()->toFront(true); }
void SymbolistMainComponent::windowSetName(String name) { getWindow()->setName(name); }

void SymbolistMainComponent::registerUpdateCallback(symbolistUpdateCallback c) { myUpdateCallback = c; }
void SymbolistMainComponent::registerCloseCallback(symbolistCloseCallback c) { myCloseCallback = c; }

void SymbolistMainComponent::closeWindow()
{
    delete getWindow();
    delete this;
}

void SymbolistMainComponent::executeCloseCallback()
{
    if (myCloseCallback) { myCloseCallback( this ); }
}

void SymbolistMainComponent::executeUpdateCallback(int arg)
{
    if (myUpdateCallback) { myUpdateCallback( this, arg ); }
}



//=================================
// INTERFACE DATA<=>VIEW
//=================================

//=================================
// => MODIFY VIEW FROM DATA
//=================================


BaseComponent* SymbolistMainComponent::makeComponentFromSymbol(Symbol* s)
{
    //BaseComponent *c;
    float x = 0.0;
    float y = 0.0;
    float w = 10.0;
    float h = 10.0;
    
    int xMessagePos = s->getOSCMessagePos("/x");
    int yMessagePos = s->getOSCMessagePos("/y");
    int wMessagePos = s->getOSCMessagePos("/w");
    int hMessagePos = s->getOSCMessagePos("/h");
    int typeMessagePos = s->getOSCMessagePos("/type");
    
    if (xMessagePos != -1) x = s->getOSCMessageValue(xMessagePos).getFloat32();
    if (yMessagePos != -1) y = s->getOSCMessageValue(yMessagePos).getFloat32();
    if (wMessagePos != -1) w = s->getOSCMessageValue(wMessagePos).getFloat32();
    if (hMessagePos != -1) h = s->getOSCMessageValue(hMessagePos).getFloat32();
    
    
    if ( typeMessagePos == -1 ) {
        
        return NULL;
        
    } else {
        
        String typeStr = s->getOSCMessageValue(typeMessagePos).getString();
        
        if (typeStr.equalsIgnoreCase(String("circle"))) {
            
            CircleComponent *c = new CircleComponent( x, y, w, h );
            c->setSymbol(s);
            return c;
            
        } else {
            
            return NULL;
            
        }
    }
}


void SymbolistMainComponent::clearScoreView()
{
    scoreGUI.removeAllSymbolComponents();
}

void SymbolistMainComponent::setContentFromScore ()
{
    for (int i = 0; i < score->getSize(); i++)
    {
        //cout << "Symbol: " << i << endl;
        BaseComponent *c = makeComponentFromSymbol( score->getSymbol(i) );
        //cout << "Component: " << c << endl;
        if ( c != NULL ) scoreGUI.addScoreChildComponent( c );
    }
}

//=================================
// <= MODIFY DATA FROM VIEW
//=================================

// can be overriden / completed by class-specific messages
int SymbolistMainComponent::addSymbolMessages( BaseComponent *c, OSCBundle *b, String base_address)
{
    int messages_added = 0;
    OSCMessage typeMessage = OSCMessage(OSCAddressPattern( String(base_address) += "/type" ), c->getSymbolType());
    OSCMessage xMessage = OSCMessage(OSCAddressPattern( String(base_address) += "/x"), static_cast<float>(c->symbol_getX()));
    OSCMessage yMessage = OSCMessage(OSCAddressPattern( String(base_address) += "/y"), static_cast<float>(c->symbol_getY()));
    OSCMessage wMessage = OSCMessage(OSCAddressPattern( String(base_address) += "/w"), static_cast<float>(c->getWidth()));
    OSCMessage hMessage = OSCMessage(OSCAddressPattern( String(base_address) += "/h"), static_cast<float>(c->getHeight()));
    b->addElement(typeMessage);
    b->addElement(xMessage);
    b->addElement(yMessage);
    b->addElement(wMessage);
    b->addElement(hMessage);
    messages_added += 5;
    
    for (int i = 0; i < c->getNumSubcomponents(); i++)
    {
        String base = base_address << "/sub_" << String(i) ;
        messages_added += addSymbolMessages( c->getSubcomponent(i), b, base);
    }
    
    return messages_added;
}

void SymbolistMainComponent::setComponentSymbol( BaseComponent *c )
{
    OSCBundle b;
    addSymbolMessages( c , &b , String("") );
    c->getSymbol()->setOSCBundle(b);
}

void SymbolistMainComponent::handleComponentAdded ( BaseComponent* c )
{
    Symbol *s = new Symbol();
    c->setSymbol( s );
    setComponentSymbol( c );
    score->addSymbol( s );
    executeUpdateCallback( -1 );
}

void SymbolistMainComponent::handleComponentRemoved ( BaseComponent* c )
{
    score->removeSymbol( c->getSymbol() );
    executeUpdateCallback( -1 );
}

void SymbolistMainComponent::handleComponentModified ( BaseComponent* c )
{
    setComponentSymbol( c );
    executeUpdateCallback( score->getSymbolPosition( c->getSymbol() ) );
}




bool SymbolistMainComponent::keyPressed (const KeyPress& key, Component* originatingComponent)
{
    //std::cout << "key " << key.getTextDescription() << "\n";
    String desc = key.getTextDescription();
    if( desc            == "command + G" ) {
        scoreGUI.groupSymbols();
    } else if ( desc    == "backspace" ) {
        scoreGUI.deleteSelectedSymbolComponents();
        //BaseComponent *selected_comp = scoreGUI.getNthSymbolComponent(0);
        //handleComponentRemoved( selected_comp );
    }
    return false;
}





