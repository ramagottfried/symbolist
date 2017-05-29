
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
    float size = 10.0;
    
    int xMessagePos = s->getOSCMessagePos("/x");
    int yMessagePos = s->getOSCMessagePos("/y");
    int sizeMessagePos = s->getOSCMessagePos("/size");
    int typeMessagePos = s->getOSCMessagePos("/type");
    
    if (xMessagePos != -1) x = s->getOSCMessageValue(xMessagePos).getFloat32();
    if (yMessagePos != -1) y = s->getOSCMessageValue(yMessagePos).getFloat32();
    if (sizeMessagePos != -1) size = s->getOSCMessageValue(sizeMessagePos).getFloat32();
    
    
    if ( typeMessagePos == -1 ) {
        
        return NULL;
        
    } else {
        
        String typeStr = s->getOSCMessageValue(typeMessagePos).getString();
        
        if (typeStr.equalsIgnoreCase(String("circle"))) {
            
            CircleComponent *c = new CircleComponent( x, y, (size * .5) );
            c->setSymbol(s);
            return c;
            
        } else {
            
            return NULL;
            
        }
    }
}


void SymbolistMainComponent::clearScoreView()
{
    scoreGUI.removeAllChildren();
}

void SymbolistMainComponent::setContentFromScore ()
{
    for (int i = 0; i < score->getSize(); i++)
    {
        BaseComponent *c = makeComponentFromSymbol( score->getSymbol(i) );
        if ( c != NULL ) scoreGUI.addScoreChildComponent( c );
    }
}

//=================================
// <= MODIFY DATA FROM VIEW
//=================================

void SymbolistMainComponent::setComponentSymbol( BaseComponent *c )
{
    OSCBundle b;
    OSCMessage typeMessage = OSCMessage(OSCAddressPattern("/type"), String("circle"));
    OSCMessage sizeMessage = OSCMessage(OSCAddressPattern("/size"), static_cast<float>(c->getWidth()));
    OSCMessage xMessage = OSCMessage(OSCAddressPattern("/x"), static_cast<float>(c->getX()));
    OSCMessage yMessage = OSCMessage(OSCAddressPattern("/y"), static_cast<float>(c->getY()));
    b.addElement(typeMessage);
    b.addElement(sizeMessage);
    b.addElement(xMessage);
    b.addElement(yMessage);
    
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
        scoreGUI.deleteSelectedSymbols();
        //BaseComponent *selected_comp = scoreGUI.getNthSymbolComponent(0);
        //handleComponentRemoved( selected_comp );
    }
    return false;
}





