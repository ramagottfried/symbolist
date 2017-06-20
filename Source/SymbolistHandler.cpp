//
//  SymbolistHandler.cpp
//  symbolist
//
//  Created by Jean Bresson on 19/06/2017.
//
//

#include "SymbolistHandler.h"

#include "SymbolistMainWindow.h"
#include "SymbolGroupComponent.h"
#include "PrimitiveIncludes.h"


SymbolistHandler::SymbolistHandler()
{
    // create two default items
    float symbol_size = 30.0;
    float symbol_pos = 0.0;
    
    Symbol* s1 = new Symbol("circle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette.addPaletteItem(s1);
    
    Symbol* s2 = new Symbol("rectangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette.addPaletteItem(s2);
    
    Symbol* s3 = new Symbol("triangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette.addPaletteItem(s3);
    
    Symbol* s4 = new Symbol("path", symbol_pos, symbol_pos, symbol_size, symbol_size);
    
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
    
    s4->addOSCMessage(numSeg_mess);
    s4->addOSCMessage(type_mess0);
    s4->addOSCMessage(x_mess0);
    s4->addOSCMessage(y_mess0);
    s4->addOSCMessage(type_mess1);
    s4->addOSCMessage(x_mess1);
    s4->addOSCMessage(y_mess1);
    s4->addOSCMessage(type_mess2);
    s4->addOSCMessage(x_mess2);
    s4->addOSCMessage(y_mess2);
    
    palette.addPaletteItem(s4);
    
    Symbol* s5 = new Symbol("text", 20.0, 20.0, symbol_size, symbol_size);
    palette.addPaletteItem(s5);
    
    main_component = NULL;
}

SymbolistHandler::~SymbolistHandler()
{
    if (main_component != NULL ) symbolistAPI_closeWindow();
}

/*********************************************
 * CONTROLLER METHODS CALLED FROM THE LIB API
 *********************************************/

// This is a static method called to create a window
// return the new SymbolistHandler
SymbolistHandler* SymbolistHandler::symbolistAPI_newSymbolist()
{
    return new SymbolistHandler ();
}

void SymbolistHandler::symbolistAPI_freeSymbolist()
{
    delete this;
}


void SymbolistHandler::symbolistAPI_openWindow()
{
    const MessageManagerLock mml;
    SymbolistMainWindow *w = new SymbolistMainWindow (this);
    main_component = w->getMainComponent();
    addComponentsFromScore();
}

void SymbolistHandler::symbolistAPI_closeWindow()
{
    if ( main_component != NULL)
    {
        std::cout << "delete Symbolist window: " << this << std::endl;
        MessageManagerLock mml;
        delete main_component->getTopLevelComponent(); // = the window
        main_component = NULL;
    }
}

void SymbolistHandler::symbolistAPI_windowToFront()
{
    if ( main_component != NULL)
    {
        const MessageManagerLock mml;
        main_component->getTopLevelComponent()->toFront(true);
    }
}

void SymbolistHandler::symbolistAPI_windowSetName(String name)
{
    if ( main_component != NULL)
    {
        const MessageManagerLock mml;
        main_component->getTopLevelComponent()->setName(name);
    }
}

void SymbolistHandler::symbolistAPI_registerUpdateCallback(symbolistUpdateCallback c)
{
    myUpdateCallback = c;
}

void SymbolistHandler::symbolistAPI_registerCloseCallback(symbolistCloseCallback c)
{
    myCloseCallback = c;
}

void SymbolistHandler::symbolistAPI_registerTransportCallback(symbolistTransportCallback c)
{
    myTransportCallback = c;
}


int SymbolistHandler::symbolistAPI_getNumSymbols()
{
    return static_cast<int>( score.getSize() );
}

odot_bundle* SymbolistHandler::symbolistAPI_getSymbol(int n)
{
    return score.getSymbol(n)->exportToOSC();
}

void SymbolistHandler::symbolistAPI_setSymbols(int n, odot_bundle **bundle_array)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope

    score.importScoreFromOSC(n, bundle_array);
    
    if ( main_component != NULL)
    {
        main_component->clearScoreView();
        addComponentsFromScore();
    }
}

void SymbolistHandler::symbolistAPI_setTime(int time_ms)
{
    const MessageManagerLock mmLock;
    current_time = time_ms;
    if ( main_component != NULL) main_component->repaint();
}


/*******************************
 * these methods are called from symbolist to notify the host environment
 *******************************/
void SymbolistHandler::executeCloseCallback()
{
    if (myCloseCallback) { myCloseCallback( this ); }
}

void SymbolistHandler::executeUpdateCallback(int arg)
{
    if (myUpdateCallback) { myUpdateCallback( this, arg ); }
}

void SymbolistHandler::executeTransportCallback(int arg)
{
    if (myTransportCallback) { myTransportCallback( this, arg ); }
}


//=================================
// PALETTE
//=================================

void SymbolistHandler::setCurrentSymbol(int n)
{
    palette.setSelectedItem(n);
}

int SymbolistHandler::getCurrentSymbolIndex()
{
    return palette.getSelectedItem();
}

Symbol* SymbolistHandler::getCurrentSymbol()
{
    return palette.getPaletteItem(palette.getSelectedItem());
}

//=================================
// MODIFY VIEW FROM DATA
//=================================

// Component factory
BaseComponent* SymbolistHandler::makeComponentFromSymbol(const Symbol* s)
{
    cout << "Creating component from Symbol: " ;
    //s->printBundle();
    
    int typeMessagePos = s->getOSCMessagePos("/type");
    
    if ( typeMessagePos == -1 ) {
        
        cout << "Could not find '/type' message in OSC Bundle.. (size=" << s->getOSCBundle().size() << ")" << endl;
        return NULL;
        
    } else {
        
        String typeStr = s->getOSCMessageValue(typeMessagePos).getString();
        cout << typeStr << std::endl;
        BaseComponent *c;
        
        if (typeStr.equalsIgnoreCase(String("circle"))) {
            c = new CirclePathComponent( *s );
        } else if (typeStr.equalsIgnoreCase(String("rectangle"))) {
            c = new RectanglePathComponent( *s );
        } else if (typeStr.equalsIgnoreCase(String("triangle"))) {
            c = new TrianglePathComponent( *s );
        } else if (typeStr.equalsIgnoreCase(String("path"))) {
            c = new LinePathComponent( *s );
        } else if (typeStr.equalsIgnoreCase(String("text"))) {
            c = new TextGlphComponent( *s );
        } else if (typeStr.equalsIgnoreCase(String("group"))) {
            c = new SymbolGroupComponent( *s );
        } else {
            cout << "Unknown symbol type : " << typeStr << endl;
            c = NULL;
        }
        if (c != NULL) c->importFromSymbol( *s ) ;
        return c;
    }
}

void SymbolistHandler::addComponentsFromScore ( )
{
    // recreate and add components from score symbols
    std::cout << "ADDING " << score.getSize() << " SYMBOLS" << std::endl;
    for (int i = 0; i < score.getSize(); i++) {
        Symbol *s = score.getSymbol(i);
        BaseComponent* c = makeComponentFromSymbol( s );
        c->setScoreSymbolPointer( s );
        main_component->addSymbolComponent(c);
    }
}

/*=================================
 * MODIFY DATA FROM VIEW
 * (CALLBACKS FROM USER ACTIONS)
 ********************************/

void SymbolistHandler::addSymbolToScore ( BaseComponent* c )
{
    //cout << "ADDING SYMBOL FOR " << c << " " << c->getSymbolTypeStr() << " [ " << c->getScoreSymbolPointer() << " ]" << std::endl;
    Symbol *s = new Symbol();
    c->addSymbolMessages( s , String("") );
    c->setScoreSymbolPointer( s );
    score.addSymbol( s );
    executeUpdateCallback( -1 );
}


void SymbolistHandler::removeSymbolFromScore ( BaseComponent* c )
{
    assert ( c->getScoreSymbolPointer() != NULL ) ;
    //cout << "REMOVING SYMBOL OF " << c << " " << c->getSymbolTypeStr() << " [ " << c->getScoreSymbolPointer() << " ]" << std::endl;
    score.removeSymbol( c->getScoreSymbolPointer() );
    c->setScoreSymbolPointer( NULL );
    executeUpdateCallback( -1 );
}


void SymbolistHandler::modifySymbolInScore( BaseComponent* c )
{
    Symbol *s = c->getScoreSymbolPointer();
    assert ( s != NULL ) ; // that's not normal
    s->clearOSCBundle();
    c->addSymbolMessages( s , String("") );
    executeUpdateCallback( score.getSymbolPosition( s ) );
}


