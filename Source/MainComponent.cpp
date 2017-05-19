
#include "MainComponent.h"
#include "MainWindow.h"


MainComponent::MainComponent()
{
    setComponentID("MainComponent");
    setSize (600, 400);
    addAndMakeVisible(scoreGUI);
}


MainComponent::MainComponent( Score *s )
{
    // setup score components here
    setComponentID("MainComponent");
    setSize (600, 400);
    
    // will populate scoreGUI
    setContentFromScore(s);
    
    addAndMakeVisible(scoreGUI);
    setWantsKeyboardFocus(true);
    addKeyListener(this);
}

MainComponent::~MainComponent() {}


void MainComponent::paint (Graphics& g)
{
    g.fillAll ( Colours::white );
}

void MainComponent::resized()
{
    scoreGUI.setBounds( 50, 50, getWidth()-100, getHeight()-100 );
}

Component* MainComponent::getWindow ()
{
    return getParentComponent();
}


bool MainComponent::keyPressed (const KeyPress& key, Component* originatingComponent)
{
    // std::cout << "key " << key.getTextDescription() << "\n";
    
    if( key.getTextDescription() == "command + G" ) {
        scoreGUI.groupSymbols();
    } else if ( key.getTextDescription() == "command + Z" ) {
        clearScoreView();
    }
    return false;
    
}



//=================================
// CONTROLLER FUNCTIONS (INTERFACE DATA<=>VIEW)
//=================================


//=================================
// MODIFY VIEW FROM DATA
//=================================


BaseComponent* MainComponent::makeComponentFromSymbol(Symbol* s)
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


void MainComponent::clearScoreView()
{
    scoreGUI.removeAllChildren();
}

void MainComponent::setContentFromScore ( Score* s )
{
    for (int i = 0; i < s->getSize(); i++)
    {
        BaseComponent *c = makeComponentFromSymbol( s->getSymbol(i) );
        if ( c != NULL ) scoreGUI.addScoreChildComponent( c );
    }
}

//=================================
// MODIFY DATA FROM VIEW
//=================================

Symbol* MainComponent::makeSymbolFromComponent(BaseComponent *c)
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
    
    Symbol* s = new Symbol(b);
    c->setSymbol(s);
    return s;
}

void MainComponent::handleNewComponent ( BaseComponent* c )
{
    Symbol* s = makeSymbolFromComponent(c);
    SymbolistMainWindow* win = static_cast<SymbolistMainWindow*>( getWindow() );
    
    win->getScore()->addSymbol(s);
    win->notifyUpdate();
}


