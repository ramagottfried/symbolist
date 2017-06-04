
#include "MainComponent.h"
#include "MainWindow.h"
#include "ScoreComponent.h"


SymbolistMainComponent::SymbolistMainComponent()
{
    // score = std::unique_ptr<Score>(new Score());
    
    setComponentID("MainComponent");
    setSize (600, 400);
    
    // create two default items
    Symbol* s1 = new Symbol();
    Symbol* s2 = new Symbol();
    
    float symbol_size = 30.0;
    
    s1->addOSCMessage( String("/type"), String("circle"));
    s1->addOSCMessage(String("/x"), float(20.0));
    s1->addOSCMessage(String("/y"), float(20.0));
    s1->addOSCMessage(String("/w"), symbol_size);
    s1->addOSCMessage(String("/h"), symbol_size);
    
    palette.addPaletteItem(s1);
    
    
    s2->addOSCMessage(String("/type"), String("path"));
    s2->addOSCMessage(String("/x"), float(20.0));
    s2->addOSCMessage(String("/y"), float(20.0));
    s2->addOSCMessage(String("/w"), symbol_size);
    s2->addOSCMessage(String("/h"), symbol_size);
    
    OSCMessage x_mess("/x-points");
    OSCMessage y_mess("/y-points");
    x_mess.addFloat32( 0.0 );
    y_mess.addFloat32( 2.0 );
    x_mess.addFloat32( 1.5 );
    y_mess.addFloat32( 0.0 );
    x_mess.addFloat32( 3.0 );
    y_mess.addFloat32( 1.5 );
    x_mess.addFloat32( 3.5 );
    y_mess.addFloat32( 0.5 );
    x_mess.addFloat32( 5.0 );
    y_mess.addFloat32( 2.0 );
    s2->addOSCMessage(x_mess);
    s2->addOSCMessage(y_mess);
    
    palette.addPaletteItem(s2);
    
    paletteView.buildFromPalette(&palette);
    paletteView.selectPaletteButton(0);
    
    addAndMakeVisible(scoreGUI);
    addAndMakeVisible(paletteView);
    
    // the main component will receive key events from the subviews
    paletteView.addKeyListener(this);
    scoreGUI.addKeyListener(this);
    setWantsKeyboardFocus(true);
    addKeyListener(this);
}


SymbolistMainComponent::~SymbolistMainComponent() { }


void SymbolistMainComponent::resized()
{
    scoreGUI.setBounds( 50, 0, getWidth(), getHeight() );
    paletteView.setBounds( 0, 0, 50, getHeight() );
}


void SymbolistMainComponent::setCurrentSymbol(int n)
{
    palette.setSelectedItem(n);
    paletteView.selectPaletteButton(n);
}

int SymbolistMainComponent::getCurrentSymbolIndex()
{
    return palette.getSelectedItem();
}

Symbol* SymbolistMainComponent::getCurrentSymbol()
{
    return palette.getPaletteItem(palette.getSelectedItem());
}

void SymbolistMainComponent::setEditMode( UI_EditType m )
{
    mouse_mode = m;
    scoreGUI.repaint();
}

UI_EditType SymbolistMainComponent::getEditMode()
{
    return mouse_mode ;
}



bool SymbolistMainComponent::keyPressed (const KeyPress& key, Component* originatingComponent)
{
    String desc = key.getTextDescription();
    std::cout << "key " << desc << "\n";
    if( desc            == "command + G" ) {
        scoreGUI.groupSymbols();
    } else if ( desc    == "backspace" ) {
        scoreGUI.deleteSelectedSymbols();
    } else if ( desc    == "C") {
        setCurrentSymbol(0);
    } else if ( desc    == "P") {
        setCurrentSymbol(1);
    }
    return true;
}


void SymbolistMainComponent::modifierKeysChanged (const ModifierKeys& modifiers)
{
    if ( modifiers.isCommandDown() )
    {
        setEditMode( UI_EditType::draw );
    }
    else
    {
        setEditMode( UI_EditType::edit );
    }
    
    // todo : better way to deal with modiyer keys
    shift_down = modifiers.isShiftDown() ;
}





/*********************************************
 * CONTROLLER METHODS CALLED FROM THE LIB API
 *********************************************/

// This is a static method called to create a window
// return the new SymbolistMainComponent within this window
SymbolistMainComponent* SymbolistMainComponent::symbolistAPI_createWindow()
{
    SymbolistEditorWindow *w = new SymbolistEditorWindow ();
    return w->getSymbolistMainComponent();
}

void SymbolistMainComponent::symbolistAPI_closeWindow()
{
    delete this->getTopLevelComponent(); // = the window
    delete this;
}

void SymbolistMainComponent::symbolistAPI_windowToFront()
{
    getTopLevelComponent()->toFront(true);
}

void SymbolistMainComponent::symbolistAPI_windowSetName(String name)
{
    getTopLevelComponent()->setName(name);
}

void SymbolistMainComponent::symbolistAPI_registerUpdateCallback(symbolistUpdateCallback c)
{
    myUpdateCallback = c;
}

void SymbolistMainComponent::symbolistAPI_registerCloseCallback(symbolistCloseCallback c)
{
    myCloseCallback = c;
}

int SymbolistMainComponent::symbolistAPI_getNumSymbols()
{
    return static_cast<int>( getScore()->getSize() );
}

odot_bundle* SymbolistMainComponent::symbolistAPI_getSymbol(int n)
{
    return getScore()->getSymbol(n)->exportToOSC();
}

void SymbolistMainComponent::symbolistAPI_setSymbols(int n, odot_bundle **bundle_array)
{
    // Will lock the MainLoop until out of scope..
    const MessageManagerLock mmLock;
    
    // clear the view
    scoreGUI.clearAllSymbolComponents();
    
    // update score
    getScore()->importScoreFromOSC(n, bundle_array);
    
    // recreate and add components from score symbols
    for (int i = 0; i < score.getSize(); i++) {
        scoreGUI.addChildToScoreComponent( makeComponentFromSymbol( score.getSymbol(i) ) ) ;
    }
}


// these two methods shall be called from symbolist to notify the host environment
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
    
    int typeMessagePos = s->getOSCMessagePos("/type");

    if ( typeMessagePos == -1 ) {
        
        cout << "Could not find '/type' message in OSC Bundle.. (size=" << s->getOSCBundle().size() << ")" << endl;
        return NULL;
        
    } else {
        
        String typeStr = s->getOSCMessageValue(typeMessagePos).getString();
        
        float x = s->getOSCMessageValue("/x").getFloat32();
        float y = s->getOSCMessageValue("/y").getFloat32();;
        float w = s->getOSCMessageValue("/w").getFloat32();
        float h = s->getOSCMessageValue("/h").getFloat32();
        
        BaseComponent *c;

        if (typeStr.equalsIgnoreCase(String("circle"))) {
            c = new CircleComponent( x, y, w, h );
        } else if (typeStr.equalsIgnoreCase(String("path"))) {
            c = new PathComponent( Point<float>(x, y) );
        } else {
            c = new BaseComponent(typeStr, Point<float>(x, y) );
        }
        
        c->setSymbol(s);
        c->importFromSymbol();
        return c;
    }
}


//=================================
// <= MODIFY DATA FROM VIEW
//=================================

void SymbolistMainComponent::updateComponentSymbol( BaseComponent *c )
{
    c->getSymbol()->clearOSCBundle();
    c->addSymbolMessages( String("") );
}

/********************************
 * CALLBACKS FROM USER ACTIONS
 ********************************/

void SymbolistMainComponent::handleComponentAdded ( BaseComponent* c )
{
    score.addSymbol( c->getSymbol() );
    executeUpdateCallback( -1 );
}

void SymbolistMainComponent::handleComponentRemoved ( BaseComponent* c )
{
    score.removeSymbol( c->getSymbol() );
    executeUpdateCallback( -1 );
}

void SymbolistMainComponent::handleComponentModified ( BaseComponent* c )
{
    updateComponentSymbol( c );
    executeUpdateCallback( score.getSymbolPosition( c->getSymbol() ) );
}








