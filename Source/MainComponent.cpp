
#include "MainComponent.h"
#include "MainWindow.h"
#include "PageComponent.h"
#include "SymbolGroupComponent.h"


SymbolistMainComponent::SymbolistMainComponent()
{
    // score = std::unique_ptr<Score>(new Score());
    
    setComponentID("MainComponent");
    setSize (600, 400);
    
    // create two default items
    float symbol_size = 30.0;
    
    Symbol* s1 = new Symbol("circle", 20.0, 20.0, symbol_size, symbol_size);
    palette.addPaletteItem(s1);
    
    Symbol* s3 = new Symbol("circle", 20.0, 20.0, symbol_size/2, symbol_size);
    palette.addPaletteItem(s3);
    
    Symbol* s2 = new Symbol("path", 20.0, 20.0, symbol_size, symbol_size);
    
    OSCMessage numSeg_mess( "/numSegments",         (int32)3 );

    OSCMessage type_mess0(  "/segment/0/type",      (String)"line"                                  );
    OSCMessage x_mess0(     "/segment/0/x_points",  (float)0.,  (float)20                           );
    OSCMessage y_mess0(     "/segment/0/y_points",  (float)0,   (float)20.                          );
    
    OSCMessage type_mess1(  "/segment/1/type",      (String)"cubic"                                 );
    OSCMessage x_mess1(     "/segment/1/x_points",  (float)20., (float)7., (float)15.,  (float)20   );
    OSCMessage y_mess1(     "/segment/1/y_points",  (float)20., (float)5.,  (float)5.,  (float)0    );

    OSCMessage type_mess2(  "/segment/2/type",      (String)"line"                                  );
    OSCMessage x_mess2(     "/segment/2/x_points",  (float)20,  (float)10.                          );
    OSCMessage y_mess2(     "/segment/2/y_points",  (float)00., (float)5                            );

    s2->addOSCMessage(numSeg_mess);
    s2->addOSCMessage(type_mess0);
    s2->addOSCMessage(x_mess0);
    s2->addOSCMessage(y_mess0);
    s2->addOSCMessage(type_mess1);
    s2->addOSCMessage(x_mess1);
    s2->addOSCMessage(y_mess1);
    s2->addOSCMessage(type_mess2);
    s2->addOSCMessage(x_mess2);
    s2->addOSCMessage(y_mess2);
    
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
        scoreGUI.groupSelectedSymbols();
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
        if( modifiers.isAltDown() )
            setEditMode( UI_EditType::draw_alt_mode );
        else
            setEditMode( UI_EditType::draw_mode );
        
    }
    else
    {
        if( modifiers.isAltDown() )
            setEditMode( UI_EditType::select_alt_mode );
        else
            setEditMode( UI_EditType::select_mode );
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
    // Will lock the MainLoop until out of scope
    const MessageManagerLock mmLock;
    
    // clear the view
    scoreGUI.clearAllSubcomponents();
    
    // update score
    getScore()->importScoreFromOSC(n, bundle_array);
    
    // recreate and add components from score symbols
    for (int i = 0; i < score.getSize(); i++) {
        Symbol *s = score.getSymbol(i);
        BaseComponent* c = makeComponentFromSymbol( s );
        c->setScoreSymbolPointer( s );
        scoreGUI.addSubcomponent( c ) ;
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

BaseComponent* SymbolistMainComponent::makeComponentFromSymbol(const Symbol* s)
{
    
    int typeMessagePos = s->getOSCMessagePos("/type");

    if ( typeMessagePos == -1 ) {
        
        cout << "Could not find '/type' message in OSC Bundle.. (size=" << s->getOSCBundle().size() << ")" << endl;
        return NULL;
        
    } else {
        
        String typeStr = s->getOSCMessageValue(typeMessagePos).getString();
        cout << "Creating component from Symbol: " << typeStr << endl;
        
        BaseComponent *c;

        if (typeStr.equalsIgnoreCase(String("circle"))) {
            c = new CirclePathComponent( *s );
        } else if (typeStr.equalsIgnoreCase(String("path"))) {
            c = new LinePathComponent( *s );
        } else if (typeStr.equalsIgnoreCase(String("group"))) {
            c = new SymbolGroupComponent( *s );
        } else {
            // ??
            c = new BaseComponent( *s );
        }
        return c;
    }
}


/*=================================
 *  MODIFY DATA FROM VIEW
 * CALLBACKS FROM USER ACTIONS
 ********************************/

void SymbolistMainComponent::notifyNewSymbol ( Symbol* s )
{
    score.addSymbol( s );
    executeUpdateCallback( -1 );
}

void SymbolistMainComponent::notifySymbolRemoved ( Symbol* s )
{
    score.removeSymbol( s );
    executeUpdateCallback( -1 );
}

void SymbolistMainComponent::notifySymbolChange ( Symbol* s )
{
    executeUpdateCallback( score.getSymbolPosition( s ) );
}








