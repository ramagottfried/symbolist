#include "SymbolistHandler.h"

#include "SymbolistMainWindow.h"

#include "StaffComponent.hpp"
#include "SymbolGroupComponent.h"
#include "BasicShapePathComponents.h"
#include "PathBaseComponent.h"
#include "TextGlyphComponent.h"

using namespace std;

SymbolistHandler::SymbolistHandler()
{
    setModel(make_shared<SymbolistModel>());
    // setView(new SymbolistMainComponent(this));
    
    score = make_shared<Score>();
    
    // create two default items
    float symbol_size = 30.0;
    float symbol_pos = 0.0;
    
    shared_ptr<Palette> palette = getModel()->getPalette();
    
    shared_ptr<Symbol> s1 = make_shared<Symbol>();
    s1->setTypeXYWH("text", symbol_pos, symbol_pos, 20 , 20);
    palette->addDefaultItem(s1);
    
    shared_ptr<Symbol> s2 = make_shared<Symbol>();
    s2->setTypeXYWH("circle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s2);
    
    shared_ptr<Symbol> s3 = make_shared<Symbol>();
    s3->setTypeXYWH("rectangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s3);
    
    shared_ptr<Symbol> s4 = make_shared<Symbol>();
    s4->setTypeXYWH("triangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s4);
    
    cout << "symbolist handler " << this << endl;
}

SymbolistHandler::SymbolistHandler(shared_ptr<SymbolistModel> model, shared_ptr<SymbolistMainComponent> view)
{
    setModel(model);
    setView(view);
    
    // create two default items
    float symbol_size = 30.0;
    float symbol_pos = 0.0;
    
    shared_ptr<Palette> palette = getModel()->getPalette();
    
    shared_ptr<Symbol> s1 = make_shared<Symbol>();
    s1->setTypeXYWH("text", symbol_pos, symbol_pos, 20 , 20);
    palette->addDefaultItem(s1);
    
    shared_ptr<Symbol> s2 = make_shared<Symbol>();
    s2->setTypeXYWH("circle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s2);
    
    shared_ptr<Symbol> s3 = make_shared<Symbol>();
    s3->setTypeXYWH("rectangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s3);
    
    shared_ptr<Symbol> s4 = make_shared<Symbol>();
    s4->setTypeXYWH("triangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s4);
    
    
}


SymbolistHandler::~SymbolistHandler()
{
    cout << "deleting symbolist handler, main comp pointer:" << main_component_ptr <<  " window " << main_window << endl;
    if ( getView() != NULL )
        symbolistAPI_closeWindow();
    
}

/*********************************************
 * CONTROLLER METHODS CALLED FROM THE LIB API
 *********************************************/

// This is a static method called to create a window
// return the new SymbolistHandler
SymbolistHandler* SymbolistHandler::symbolistAPI_newSymbolist()
{
    cout << "this message manager instance  " << MessageManager::getInstance() << endl;

    return new SymbolistHandler ();
}

void SymbolistHandler::symbolistAPI_freeSymbolist()
{
    delete this;
}


void SymbolistHandler::symbolistAPI_openWindow()
{
    
    cout << "this thread " << Thread::getCurrentThread() << endl;
    cout << "this message manager instance  " << MessageManager::getInstance() << endl;
    
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
  
    if( main_window )
    {
//        cout << "nulling main window " << main_window << endl;
        main_component_ptr = nullptr;
        main_window = nullptr;
    }
}


void SymbolistHandler::symbolistAPI_windowToFront()
{
    const MessageManagerLock mml;

    if ( main_component_ptr != NULL)
    {
        main_component_ptr->getTopLevelComponent()->toFront(true);
    }

    if ( inspector_ptr != NULL)
    {
        inspector_ptr->getTopLevelComponent()->toFront(true);
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
    return static_cast<int>( score->getSize() );
}

shared_ptr<Symbol> SymbolistHandler::symbolistAPI_getSymbol(int n)
{
    return score->getSymbol(n);
}

OdotBundle_s SymbolistHandler::symbolistAPI_getSymbolBundle_s(int n)
{
    return score->getSymbol(n)->serialize();
}

void SymbolistHandler::symbolistAPI_setOneSymbol( const OdotBundle_s& bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    shared_ptr<Symbol> s = make_shared<Symbol>( bundle );
    score->addSymbol(s);
    
    if ( main_component_ptr != nullptr )
    {
        BaseComponent* c = makeComponentFromSymbol( s , false);
        main_component_ptr->getPageComponent()->addSubcomponent(c);
        c->setScoreSymbolPointer( s );
    }
    else
    {
        executeUpdateCallback( -1 ); // if the windows is open, this is called from the component creation routine
        cout << "main component is NULL" << endl;
    }
}

void SymbolistHandler::symbolistAPI_setSymbols( const OdotBundle_s& bundle_array)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    score->importScoreFromOSC( bundle_array );
    
    if ( main_component_ptr != NULL)
    {
        main_component_ptr->getPageComponent()->clearAllSubcomponents();
        addComponentsFromScore();
    }
}


int SymbolistHandler::symbolistAPI_getNumPaletteSymbols()
{
    return static_cast<int>( getModel()->getPalette()->getPaletteNumUserItems() );
}

shared_ptr<Symbol> SymbolistHandler::symbolistAPI_getPaletteSymbol(int n)
{
    return getModel()->getPalette()->getPaletteUserItem(n);
}

void SymbolistHandler::symbolistAPI_setOnePaletteSymbol( const OdotBundle_s& bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    shared_ptr<Symbol> s = make_shared<Symbol>( bundle );
    
    getModel()->getPalette()->addUserItem(s);
    
}

void SymbolistHandler::symbolistAPI_setPaletteSymbols(const OdotBundle_s& bundle_array)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    const OdotBundle bndl( bundle_array );
    
    for (auto msg : bndl.getMessageArray() )
    {
        if( msg[0].getType() == OdotAtom::O_ATOM_BUNDLE && msg.getAddress().find("/symbol") == 0 )
        {
            shared_ptr<Symbol> s = make_shared<Symbol>( msg.getBundle().get_o_ptr() );
            getModel()->getPalette()->addUserItem(s);
        }
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
        main_component_ptr->setTimePoint( time_ms );
    }
}

void SymbolistHandler::symbolistAPI_toggleTimeCusor()
{
    if ( main_component_ptr != NULL)
    {
        main_component_ptr->toggleTimeAndCursorDisplay();
    }
}

StaffComponent* SymbolistHandler::getStaveAtTime( float time )
{
    if( shared_ptr<Symbol> stave_sym = score->getStaveAtTime( time ) )
    {
        Component *c = main_component_ptr->getPageComponent()->findChildWithID( stave_sym->getID().c_str() );
        if( c )
        {
            StaffComponent *staff = dynamic_cast<StaffComponent*>(c);
            if( staff )
            {
                return staff;
            }
        }
    }
    
    return NULL;
}

OdotBundle_s SymbolistHandler::symbolistAPI_getSymbolsAtTime( float t )
{
    return score->getSymbolsAtTime(t);
}


OdotBundle_s SymbolistHandler::symbolistAPI_getdurationBundle()
{
    return score->getDurationBundle();
}


OdotBundle_s SymbolistHandler::symbolistAPI_getScoreBundle()
{
    return score->getScoreBundle_s();
}

/*
odot_bundle* SymbolistHandler::symbolistAPI_getTimePointBundle()
{
    return score->getScoreBundle();
}
*/

void SymbolistHandler::symbolistAPI_clearScore()
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope

    if ( main_component_ptr != NULL )
    {
        main_component_ptr->getPageComponent()->clearAllSubcomponents();
    }
    score->removeAllSymbols();
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
    //cout << "executeUpdateCallback" << endl;
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
    getModel()->getPalette()->setSelectedItem(n);
}

int SymbolistHandler::getCurrentSymbolIndex()
{
    return getModel()->getPalette()->getSelectedItem();
}

shared_ptr<Symbol> SymbolistHandler::getCurrentSymbol()
{
    shared_ptr<Palette> palette = getModel()->getPalette();
    int num_def_symbols = palette->getPaletteNumDefaultItems();
    int sel = palette->getSelectedItem();
    
    if ( sel < num_def_symbols )
        return palette->getPaletteDefaultItem(sel);
    else
        return palette->getPaletteUserItem(sel - num_def_symbols);
}

//=================================
// MODIFY VIEW FROM DATA
//=================================

// Component factory
BaseComponent* SymbolistHandler::makeComponentFromSymbol(shared_ptr<Symbol> s, bool attach_the_symbol)
{
    cout << "Creating component from Symbol: " ;
    s->print();
    
    string typeofSymbol = s->getMessage("/type").getString();
    if ( typeofSymbol.size() == 0 )
    {
        cout << "Could not find '/type' message in OSC Bundle.. " << endl;
        return NULL;
        
    } else {
        
        cout << typeofSymbol << std::endl;
        BaseComponent *newComponent;
        
        // allocates component based on type, all are derived from the BaseComponent
        if ( typeofSymbol == "circle" ) {
            newComponent = new CirclePathComponent();
        } else if ( typeofSymbol == "rectangle" ) {
            newComponent = new RectanglePathComponent();
        } else if ( typeofSymbol =="triangle" ) {
            newComponent = new TrianglePathComponent();
        } else if ( typeofSymbol == "path" ) {
            newComponent = new PathBaseComponent();
        } else if ( typeofSymbol == "text" ) {
            newComponent = new TextGlphComponent();
        } else if ( typeofSymbol == "group" ) {
            newComponent = new SymbolGroupComponent();
        } else if ( typeofSymbol == "staff" ) {
            newComponent = new StaffComponent();
        } else {
            cout << "Unknown symbol type : " << typeofSymbol << endl;
            newComponent = NULL;
        }
        
        if (newComponent != NULL)
        {
            // reads base component symbol values, and sets component bounds for display
            newComponent->importFromSymbol( *s ) ;
            
            // initializes object specific messages if not present
            newComponent->addSymbolMessages( s );
            
            if ( attach_the_symbol )
            {
                /*
                if( main_component_ptr != NULL )
                {
                    newComponent->setComponentID( newComponent->getSymbolTypeStr() + "_" + (String)main_component_ptr->getPageComponent()->getNumSubcomponents() );
                    s->setID( newComponent->getComponentID() );
                }
                */
                
                newComponent->setScoreSymbolPointer( s );
                score->addStaff( s ); // << /type checked internally and added if staff

            }
        }
        
        return newComponent;
    }
}

void SymbolistHandler::addComponentsFromScore ( )
{
    // recreate and add components from score symbols
    shared_ptr<Score> score = getModel()->getScore();
    cout << "ADDING " << score->getSize() << " SYMBOLS" << endl;
    
    for (int i = 0; i < score->getSize(); i++)
    {
        shared_ptr<Symbol> s = score->getSymbol(i);
        BaseComponent* c = makeComponentFromSymbol(s, false);
        
        main_component_ptr->getPageComponent()->addSubcomponent(c);
        c->setScoreSymbolPointer(s);
    }
}

/*=================================
 * MODIFY DATA FROM VIEW
 * (CALLBACKS FROM USER ACTIONS)
 ********************************/

void SymbolistHandler::addSymbolToScore ( BaseComponent* component )
{
    assert ( component->getScoreSymbolPointer() != NULL ) ;
    //cout << "ADDING SYMBOL FOR " << c << " " << c->getSymbolTypeStr() << " [ " << c->getScoreSymbolPointer() << " ]" << std::endl;
    //log_score_change();

    score->addSymbol( component->getScoreSymbolPointer() );
    executeUpdateCallback( -1 );
    
}

void SymbolistHandler::removeSymbolFromScore ( BaseComponent* component )
{
    assert ( component->getScoreSymbolPointer() != NULL ) ;
    //cout << "REMOVING SYMBOL OF " << c << " " << c->getSymbolTypeStr() << " [ " << c->getScoreSymbolPointer() << " ]" << std::endl;
    
    shared_ptr<Symbol> symbol = component->getScoreSymbolPointer();
    assert ( symbol != NULL ) ; // that's not normal
    
    // log_score_change();

    // cout << "removeSymbolFromScore" << endl;
    symbol->print();

    if( main_component_ptr )
        main_component_ptr->clearInspector();
    
    score->removeSymbolTimePoints( symbol );
    score->removeSymbol( symbol );
    
    component->setScoreSymbolPointer( NULL );
    executeUpdateCallback( -1 );
    
}


/*
 *  the component has changed, and so we need to update it's symbol bundle
 */
void SymbolistHandler::modifySymbolInScore( BaseComponent* c )
{
    
    //log_score_change();
    
    // get pointer to symbol attached to component
    shared_ptr<Symbol> s = c->getScoreSymbolPointer();
    assert ( s != NULL ) ;
    
    // cout << c << " ---> modifySymbolInScore " << s->getID() << endl;
    // printRect(c->getBounds(), "component");

    
    // remove current time point for symbol, or if stave remove all symbol timepoints on stave
    score->removeSymbolTimePoints( s );
    
    // clear the bundle attached to the component (since the component has been updated)
    // don't have to clear, because the symbol is updated not in add symbol
    // also we want to keep user data if any
    // s->clear();
    
    // update the symbol with the component's current state
    c->addSymbolMessages( s );
    
    if( s->getType() == "staff" )
    {
        cout << "type staff " << endl;
        // if the type is "staff" resort the stave order and update time point array
        score->updateStavesAndTimepoints();
    }
    else
    {
        // if the type is not a staff, add the time points for the symbol
        score->addSymbolTimePoints( s );
    }
    
    
    executeUpdateCallback( score->getSymbolPosition( s ) );
    
    c->repaint();
    
}


/***
 * called when something is changed, added, deleted (not but not undo)
 ***/
void SymbolistHandler::log_score_change()
{
    redo_stack.clear();
    push_undo_stack();
}


void SymbolistHandler::push_undo_stack()
{
    cout << "prev score :"<< endl;
    score->print();
    
    undo_stack.add( new Score( *score ) );
    
    if( undo_stack.size() > 10 )
    {
        undo_stack.remove( 0 );
    }
}

/***
 * when undo is called, move current state to redo_stack
 ***/
void SymbolistHandler::push_redo_stack()
{
    redo_stack.add( new Score( *score ) );
}


void SymbolistHandler::undo()
{
    if( undo_stack.size() > 0 )
    {
        const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
        
        if ( main_component_ptr != NULL )
        {
            push_redo_stack();

            main_component_ptr->getPageComponent()->unselectAllComponents();
            main_component_ptr->getPageComponent()->clearAllSubcomponents();
            score->removeAllSymbols();
            score = make_shared<Score>(undo_stack.removeAndReturn( undo_stack.size() - 1 ));
            
            addComponentsFromScore();
            main_component_ptr->repaint();
        }
        
    }
}

void SymbolistHandler::redo()
{
    if( redo_stack.size() > 0 )
    {
        const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
        
        if ( main_component_ptr != NULL )
        {
            push_undo_stack();
            
            main_component_ptr->getPageComponent()->unselectAllComponents();
            main_component_ptr->getPageComponent()->clearAllSubcomponents();
            score->removeAllSymbols();
            score = make_shared<Score>(redo_stack.removeAndReturn( redo_stack.size() - 1 ));
            addComponentsFromScore();
            main_component_ptr->repaint();
        }
        
    }
}

void SymbolistHandler::addToInspector( BaseComponent *c )
{
    // only selected and called if the main component is there...
    main_component_ptr->setInspectorObject(c);
}

void SymbolistHandler::clearInspector()
{
    main_component_ptr->clearInspector();
}

void SymbolistHandler::updateSymbolFromInspector( BaseComponent *c)
{
    c->importFromSymbol( *c->getScoreSymbolPointer() );
    modifySymbolInScore( c );
    //repaint is called in modify symbol
}

void SymbolistHandler::convertSelectedToStaff()
{
    main_component_ptr->getPageComponent()->createStaffFromSelected();
}



void SymbolistHandler::copySelectedToClipBoard()
{
    clipboard.clear();
    
    for( auto c : main_component_ptr->getPageComponent()->getSelectedItems() )
    {
        clipboard.add(new Symbol( *(dynamic_cast<BaseComponent*>(c))->getScoreSymbolPointer()) );
    }
}

void SymbolistHandler::newFromClipBoard()
{
    auto pc = main_component_ptr->getPageComponent();
    
    for( auto s : clipboard )
    {
        // cout << " SymbolistHandler::newFromClipBoard " << endl;
        // cout << s->getID() << endl;
        shared_ptr<Symbol> new_sym = make_shared<Symbol>(*s);
        BaseComponent *c = makeComponentFromSymbol( new_sym, true );
        
        if ( c != NULL)
        {
            pc->addSubcomponent( c );
            c->toFront(true);
            pc->addToSelection( c );
        }

    }
}
