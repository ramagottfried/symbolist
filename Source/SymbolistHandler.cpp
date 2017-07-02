//
//  SymbolistHandler.cpp
//  symbolist
//
//  Created by Jean Bresson on 19/06/2017.
//
//

#include "SymbolistHandler.h"

#include "SymbolistMainWindow.h"
#include "InspectorWindow.h"

#include "SymbolGroupComponent.h"
#include "BasicShapePathComponents.h"
#include "PathBaseComponent.h"
#include "TextGlyphComponent.h"


SymbolistHandler::SymbolistHandler()
{
    // create two default items
    float symbol_size = 30.0;
    float symbol_pos = 0.0;
    
    Symbol* s1 = new Symbol("circle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette.addDefaultItem(s1);
    
    Symbol* s2 = new Symbol("rectangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette.addDefaultItem(s2);
    
    Symbol* s3 = new Symbol("triangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette.addDefaultItem(s3);
    
    Symbol* s5 = new Symbol("text", 20.0, 20.0, symbol_size, symbol_size);
    palette.addDefaultItem(s5);
}

SymbolistHandler::~SymbolistHandler()
{
    cout << "deleting symbolist handler, main comp pointer:" << main_component_ptr << " inspector " << inspector << " window " << main_window << endl;
    if ( main_component_ptr != NULL )
        symbolistAPI_closeWindow();
    
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
    main_window = new SymbolistMainWindow (this);
    main_component_ptr = main_window->getMainComponent();

    addComponentsFromScore();
    main_component_ptr->grabKeyboardFocus();

}


void SymbolistHandler::symbolistAPI_closeWindow()
{
    cout << "symbolistAPI_closeWindow" << endl;
    MessageManagerLock mml;

    if ( main_component_ptr != NULL)
    {
 //       delete main_component->getTopLevelComponent(); // = the window
        //deleteAndZero( main_component );
        cout << "nulling main component pointer " << main_component_ptr << endl;
        main_component_ptr = nullptr;
    }

    if ( inspector != NULL)
    {
        cout << "deleting inspector component " << inspector << endl;

        delete inspector->getTopLevelComponent();
        //deleteAndZero( inspector );
        inspector = nullptr;
    }
    
    if( main_window )
    {
        cout << "nulling main window " << main_window << endl;
        main_window = nullptr;
    }
}

void SymbolistHandler::symbolistAPI_toggleInspectorWindow()
{
    if( inspector == NULL )
        symbolistAPI_openInspectorWindow();
    else
        symbolistAPI_closeInspectorWindow();

}


void SymbolistHandler::symbolistAPI_closeInspectorWindow()
{
    const MessageManagerLock mml;
    if ( inspector != NULL)
    {
        delete inspector->getTopLevelComponent();
        inspector = NULL;
        
        if( main_component_ptr != NULL )
            main_component_ptr->grabKeyboardFocus();
    }
}

void SymbolistHandler::symbolistAPI_openInspectorWindow()
{
    const MessageManagerLock mml;
    
    if ( inspector == NULL)
    {
        InspectorWindow *iw = new InspectorWindow (this);
        inspector = iw->getMainComponent();
        inspector->grabKeyboardFocus();
        if ( main_component_ptr )
        {
            auto sel = main_component_ptr->getPageComponent()->getSelectedItems();
            addToInspector( (BaseComponent *)sel.getLast() );
        }
    }
    else
    {
        inspector->getTopLevelComponent()->toFront(true);
    }
}

void SymbolistHandler::symbolistAPI_windowToFront()
{
    const MessageManagerLock mml;

    if ( main_component_ptr != NULL)
    {
        main_component_ptr->getTopLevelComponent()->toFront(true);
    }

    if ( inspector != NULL)
    {
        inspector->getTopLevelComponent()->toFront(true);
    }
}

void SymbolistHandler::symbolistAPI_windowSetName(String name)
{
    if ( main_component_ptr != NULL)
    {
        const MessageManagerLock mml;
        main_component_ptr->getTopLevelComponent()->setName(name);
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

void SymbolistHandler::symbolistAPI_setOneSymbol( odot_bundle *bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    Symbol *s = new Symbol();
    s->importFromOSC( bundle );
    score.addSymbol(s);
    
    if ( main_component_ptr != NULL )
    {
        BaseComponent* c = makeComponentFromSymbol( s , false);
        main_component_ptr->getPageComponent()->addSubcomponent(c);
        c->setScoreSymbolPointer( s );
    }
    else
    {
        cout << "main component is NULL" << endl;
    }
}

void SymbolistHandler::symbolistAPI_setSymbols(int n, odot_bundle **bundle_array)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    score.importScoreFromOSC(n, bundle_array);
    
    if ( main_component_ptr != NULL)
    {
        main_component_ptr->getPageComponent()->clearAllSubcomponents();
        addComponentsFromScore();
    }
}


int SymbolistHandler::symbolistAPI_getNumPaletteSymbols()
{
    return static_cast<int>( palette.getPaletteNumUserItems() );
}

odot_bundle* SymbolistHandler::symbolistAPI_getPaletteSymbol(int n)
{
    return palette.getPaletteUserItem(n)->exportToOSC();
}

void SymbolistHandler::symbolistAPI_setOnePaletteSymbol( odot_bundle *bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    Symbol *s = new Symbol();
    s->importFromOSC( bundle );
    palette.addUserItem(s);
    
}

void SymbolistHandler::symbolistAPI_setPaletteSymbols(int n, odot_bundle **bundle_array)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    for (int i = 0; i < n ; i++) {
        Symbol *s = new Symbol();
        s->importFromOSC( bundle_array[i] );
        palette.addUserItem(s);
    }
    if ( main_component_ptr != NULL )
    {
        main_component_ptr->updatePaletteView();
    }
}


void SymbolistHandler::symbolistAPI_setTime(float time_ms)
{
    const MessageManagerLock mmLock;
    current_time = time_ms;
    
    if ( main_component_ptr != NULL)
    {
        main_component_ptr->getPageComponent()->setTimePoint( time_ms );
        main_component_ptr->repaint();
    }
}

odot_bundle* SymbolistHandler::symbolistAPI_getSymbolsAtTime( float t )
{
    return score.getSymbolsAtTime(t);
}

odot_bundle* SymbolistHandler::symbolistAPI_getScoreBundle()
{
    return score.getScoreBundle();
}

void SymbolistHandler::symbolistAPI_clearScore()
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope

    if ( main_component_ptr != NULL )
    {
        main_component_ptr->getPageComponent()->clearAllSubcomponents();
    }
    score.removeAllSymbols();
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
    int num_def_symbols = palette.getPaletteNumDefaultItems();
    int sel = palette.getSelectedItem();
    
    if ( sel < num_def_symbols )
        return palette.getPaletteDefaultItem(sel);
    else
        return palette.getPaletteUserItem(sel - num_def_symbols);
}

//=================================
// MODIFY VIEW FROM DATA
//=================================

// Component factory
BaseComponent* SymbolistHandler::makeComponentFromSymbol(Symbol* s, bool attach_the_symbol)
{
    cout << "Creating component from Symbol: " ;
    //s->printBundle();
    
    int typeMessagePos = s->getOSCMessagePos("/type");
    
    if ( typeMessagePos == -1 ) {
        
        cout << "Could not find '/type' message in OSC Bundle.. (size=" << s->getOSCBundle()->size() << ")" << endl;
        return NULL;
        
    } else {
        
        String typeStr = s->getOSCMessageValue(typeMessagePos).getString();
        cout << typeStr << std::endl;
        BaseComponent *c;
        
        // allocates component based on type, all are derived from the BaseComponent
        if (typeStr.equalsIgnoreCase(String("circle"))) {
            c = new CirclePathComponent();
        } else if (typeStr.equalsIgnoreCase(String("rectangle"))) {
            c = new RectanglePathComponent();
        } else if (typeStr.equalsIgnoreCase(String("triangle"))) {
            c = new TrianglePathComponent();
        } else if (typeStr.equalsIgnoreCase(String("path"))) {
            c = new PathBaseComponent();
        } else if (typeStr.equalsIgnoreCase(String("text"))) {
            c = new TextGlphComponent();
        } else if (typeStr.equalsIgnoreCase(String("group"))) {
            c = new SymbolGroupComponent();
        } else {
            cout << "Unknown symbol type : " << typeStr << endl;
            c = NULL;
        }
        
        if (c != NULL)
        {
            // reads base component symbol values, and sets component bounds for display
            c->importFromSymbol( *s ) ;
            
            if ( attach_the_symbol )
            {                
                if( main_component_ptr != NULL )
                    c->setComponentID( c->getSymbolTypeStr() + "_" + (String)main_component_ptr->getPageComponent()->getNumSubcomponents() );

                c->setScoreSymbolPointer( s );
                
                // initializes object specific messages if not present
                c->addSymbolMessages( s, "" );
            }
        }
        
        return c;
    }
}

void SymbolistHandler::addComponentsFromScore ( )
{
    // recreate and add components from score symbols
    std::cout << "ADDING " << score.getSize() << " SYMBOLS" << std::endl;
    for (int i = 0; i < score.getSize(); i++)
    {
        Symbol *s = score.getSymbol(i);
        BaseComponent* c = makeComponentFromSymbol( s, false );
        main_component_ptr->getPageComponent()->addSubcomponent(c);
        c->setScoreSymbolPointer( s );
    }
}

/*=================================
 * MODIFY DATA FROM VIEW
 * (CALLBACKS FROM USER ACTIONS)
 ********************************/

void SymbolistHandler::addSymbolToScore ( BaseComponent* c )
{
    assert ( c->getScoreSymbolPointer() != NULL ) ;
    //cout << "ADDING SYMBOL FOR " << c << " " << c->getSymbolTypeStr() << " [ " << c->getScoreSymbolPointer() << " ]" << std::endl;
    score.addSymbol( c->getScoreSymbolPointer() );
    
    executeUpdateCallback( -1 );
    
    main_component_ptr->getPageComponent()->drawTimePoints();
}

void SymbolistHandler::removeSymbolFromScore ( BaseComponent* c )
{
    assert ( c->getScoreSymbolPointer() != NULL ) ;
    //cout << "REMOVING SYMBOL OF " << c << " " << c->getSymbolTypeStr() << " [ " << c->getScoreSymbolPointer() << " ]" << std::endl;
    
    Symbol *s = c->getScoreSymbolPointer();
    assert ( s != NULL ) ; // that's not normal
    
    cout << "removeSymbolFromScore" << endl;
    //s->printBundle();

    if( inspector ) inspector->clearInspector();
    
    score.removeSymbolTimePoints( s );
    score.removeSymbol( s );
    
    c->setScoreSymbolPointer( NULL );
    executeUpdateCallback( -1 );
    
    main_component_ptr->getPageComponent()->drawTimePoints();
}


void SymbolistHandler::modifySymbolInScore( BaseComponent* c )
{
    Symbol *s = c->getScoreSymbolPointer();
    assert ( s != NULL ) ; // that's not normal
    
    cout << c << " ---> modifySymbolInScore " << s << endl;
    //s->printBundle();
    printRect(c->getBounds(), "component");

    score.removeSymbolTimePoints( s );
    s->clearOSCBundle();
    
    c->addSymbolMessages( s , String("") );
    score.addSymbolTimePoints( s );
    executeUpdateCallback( score.getSymbolPosition( s ) );
    
    main_component_ptr->getPageComponent()->drawTimePoints();
    
    addToInspector( c );

}


void SymbolistHandler::addToInspector( BaseComponent *c )
{
    if( inspector ) inspector->setInspectorObject( c );
}

void SymbolistHandler::clearInspector()
{
    if( inspector ) inspector->clearInspector();
}

void SymbolistHandler::updateSymbolFromInspector( BaseComponent *c, Symbol& s )
{
    c->importFromSymbol(s);
    modifySymbolInScore( c );
}


void SymbolistHandler::copySelectedToClipBoard()
{
    clipboard.clear();
    
    for( auto c : main_component_ptr->getPageComponent()->getSelectedItems() )
    {
        clipboard.add(new Symbol( *((BaseComponent*)c)->getScoreSymbolPointer()) );
    }
}

void SymbolistHandler::newFromClipBoard()
{
    auto pc = main_component_ptr->getPageComponent();
    
    for( auto s : clipboard )
    {
        BaseComponent *c = makeComponentFromSymbol( new Symbol(*s), true );
        if ( c != NULL)
        {
            pc->addSubcomponent( c );
            c->toFront(true);
            pc->addToSelection( c );
        }

    }
}
