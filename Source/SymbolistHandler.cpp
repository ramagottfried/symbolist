#include "SymbolistHandler.h"

#include "SymbolistMainWindow.h"

#include "StaffComponent.hpp"
#include "SymbolGroupComponent.h"
#include "BasicShapePathComponents.h"
#include "PathBaseComponent.h"
#include "TextGlyphComponent.h"

SymbolistHandler* SymbolistHandler::INSTANCE = NULL;

SymbolistHandler::SymbolistHandler()
{
    cout << "SymbolistHandler's default constructor " << this << endl;
}

SymbolistHandler::SymbolistHandler(SymbolistModel* model, SymbolistMainComponent* view)
{
    setModel(model);
    setView(view);
}

SymbolistHandler::~SymbolistHandler()
{
    cout << "Deleting symbolist handler, main component pointer: "
         << getView() <<  " window " << main_window.get() << endl;
    
    if (getView() != NULL)
        symbolistAPI_closeWindow();
    
    if (getModel() != NULL)
        delete getModel();
}

SymbolistHandler* SymbolistHandler::getInstance()
{
    if (INSTANCE == NULL)
    {
        INSTANCE = new SymbolistHandler();
    }
    
    return INSTANCE;
}

void SymbolistHandler::createPaletteController()
{
    this->paletteController = unique_ptr<PaletteController>(new PaletteController());
    this->paletteController->setModel(getModel());
}

/*********************************************
 * CONTROLLER METHODS CALLED FROM THE LIB API
 *********************************************/

// This is a static method that returns the new SymbolistHandler
SymbolistHandler* SymbolistHandler::symbolistAPI_newSymbolist()
{
    cout << __func__ << endl;
    
    // Instantiates the model.
    SymbolistModel* model = new SymbolistModel();
    
    // Adds four default items to the model.
    float symbol_size = 30.0;
    float symbol_pos = 0.0;
    
    Palette* palette = model->getPalette();
    
    Symbol s1 = Symbol();
    s1.setTypeXYWH("text", symbol_pos, symbol_pos, 20 , 20);
    palette->addDefaultItem(s1);
    
    Symbol s2 = Symbol();
    s2.setTypeXYWH("circle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s2);
    
    Symbol s3 = Symbol();
    s3.setTypeXYWH("rectangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s3);
    
    Symbol s4 = Symbol();
    s4.setTypeXYWH("triangle", symbol_pos, symbol_pos, symbol_size, symbol_size);
    palette->addDefaultItem(s4);
    
    SymbolistHandler::getInstance()->setModel(model);
    
    // Creates the paletteController.
    SymbolistHandler::getInstance()->createPaletteController();
    
    /* Adds the SymbolistHandler instance and
     * all its child controllers as observers of the model.
     */
    SymbolistHandler::getInstance()->getModel()->attach(SymbolistHandler::getInstance());
    SymbolistHandler::getInstance()->getModel()->attach(SymbolistHandler::getInstance()
                                                                ->getPaletteController());
    
    return SymbolistHandler::getInstance();
}

void SymbolistHandler::symbolistAPI_freeSymbolist()
{
    delete this;
}

void SymbolistHandler::symbolistAPI_openWindow()
{
    cout << __func__ << endl;
    cout << "This thread " << Thread::getCurrentThread() << endl;
    cout << "This message manager instance " << MessageManager::getInstance() << endl;
    
    const MessageManagerLock mml;
    
    /* Creates the SymbolistMainWindow instance which in its turn
     * creates the SymbolistMainComponent instance.
     */
    main_window = unique_ptr<SymbolistMainWindow>(new SymbolistMainWindow());
    
    /* Sets the view for SymbolistHandler's instance
     * and all its child controllers.
     */
    setView(main_window->getMainComponent());
    getPaletteController()->setView(getView()->getPaletteView());
    
    addComponentsFromScore();
    getView()->grabKeyboardFocus();

}

void SymbolistHandler::symbolistAPI_closeWindow()
{
    cout << __func__ << endl;
    MessageManagerLock mml;
  
    if (main_window)
        main_window = nullptr;
}

void SymbolistHandler::symbolistAPI_windowToFront()
{
    const MessageManagerLock mml;

    if ( getView() != NULL)
    {
        getView()->getTopLevelComponent()->toFront(true);
    }

    if ( getView()->getInspector()->getSymbolPanelTab() != NULL)
    {
        getView()
                ->getInspector()
                ->getSymbolPanelTab()
                ->getTopLevelComponent()
                ->toFront(true);
    }
}

void SymbolistHandler::symbolistAPI_windowSetName(String name)
{
    if ( getView() != NULL)
    {
        const MessageManagerLock mml;
        getView()->getTopLevelComponent()->setName(name);
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
    return static_cast<int>( getModel()->getScore()->getSize() );
}

Symbol* SymbolistHandler::symbolistAPI_getSymbol(int n)
{
    return getModel()->getScore()->getSymbol(n);
}

OdotBundle_s SymbolistHandler::symbolistAPI_getSymbolBundle_s(int n)
{
    return getModel()->getScore()->getSymbol(n)->serialize();
}

void SymbolistHandler::symbolistAPI_setOneSymbol( const OdotBundle_s& bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    Symbol s = Symbol(bundle);
    getModel()->getScore()->addSymbol(&s);
    
    if (getView() != nullptr)
    {
        BaseComponent* c = makeComponentFromSymbol(&s, false);
        getView()->getPageComponent()->addSubcomponent(c);
        c->setScoreSymbolPointer(&s);
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
    
    getModel()->getScore()->importScoreFromOSC( bundle_array );
    
    if ( getView() != NULL)
    {
        getView()->getPageComponent()->clearAllSubcomponents();
        addComponentsFromScore();
    }
}

int SymbolistHandler::symbolistAPI_getNumPaletteSymbols()
{
    return paletteController->getNumPaletteSymbols();
}

Symbol* SymbolistHandler::symbolistAPI_getPaletteSymbol(int n)
{
    return paletteController->getPaletteSymbol(n);
}

void SymbolistHandler::symbolistAPI_setOnePaletteSymbol(const OdotBundle_s& bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    paletteController->setOnePaletteSymbol(bundle);
}

void SymbolistHandler::symbolistAPI_setPaletteSymbols(const OdotBundle_s& bundle_array)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    paletteController->setPaletteSymbols(bundle_array);
}

void SymbolistHandler::symbolistAPI_setTime(float time_ms)
{
    const MessageManagerLock mmLock;
    current_time = time_ms;
    
    if (getView() != NULL)
        getView()->setTimePoint( time_ms );
}

void SymbolistHandler::symbolistAPI_toggleTimeCusor()
{
    if (getView() != NULL)
        getView()->toggleTimeAndCursorDisplay();
}

StaffComponent* SymbolistHandler::getStaveAtTime( float time )
{
    if( Symbol* stave_sym = getModel()->getScore()->getStaveAtTime( time ) )
    {
        Component *c = getView()->getPageComponent()->findChildWithID( stave_sym->getID().c_str() );
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
    return getModel()->getScore()->getSymbolsAtTime(t);
}

OdotBundle_s SymbolistHandler::symbolistAPI_getdurationBundle()
{
    return getModel()->getScore()->getDurationBundle();
}

OdotBundle_s SymbolistHandler::symbolistAPI_getScoreBundle()
{
    return getModel()->getScore()->getScoreBundle_s();
}

void SymbolistHandler::symbolistAPI_clearScore()
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope

    if ( getView() != NULL )
    {
        getView()->getPageComponent()->clearAllSubcomponents();
    }
    getModel()->getScore()->removeAllSymbols();
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
// SCORE
//=================================
Symbol* SymbolistHandler::createSymbolFromTemplate()
{
    Score* score = getModel()->getScore();
    Symbol* selectedSymbolInPalette = getSelectedSymbolInPalette();
    
    return score->addSymbol(selectedSymbolInPalette);
}

Symbol* SymbolistHandler::createSymbol()
{
    return getModel()->getScore()->createSymbol();
}

//=================================
// PALETTE
//=================================

Symbol* SymbolistHandler::getSelectedSymbolInPalette()
{
    return paletteController->getSelectedSymbolInPalette();
}

//=================================
// MODIFY VIEW FROM DATA
//=================================

// Component factory
BaseComponent* SymbolistHandler::makeComponentFromSymbol(Symbol* s, bool attach_the_symbol)
{
    cout << "SymbolistHandler::makeComponentFromSymbol : Creating component from Symbol: ";
    
    string typeofSymbol = s->getMessage("/type").getString();
    if (typeofSymbol.size() == 0)
    {
        cout << "Could not find '/type' message in OSC Bundle.. " << endl;
        return NULL;
        
    } else {
        
        std::cout << typeofSymbol << std::endl;
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
            newComponent->importFromSymbol(s) ;
            
            // initializes object specific messages if not present
            newComponent->addSymbolMessages(s);
            
            if (attach_the_symbol)
            {
                newComponent->setScoreSymbolPointer(s);
                getModel()->getScore()->addStaff(s); // << /type checked internally and added if staff
            }
        }
        
        return newComponent;
    }
}

void SymbolistHandler::addComponentsFromScore ( )
{
    // recreate and add components from score symbols
    Score* score = getModel()->getScore();
    cout << "ADDING " << score->getSize() << " SYMBOLS" << endl;
    
    for (int i = 0; i < score->getSize(); i++)
    {
        Symbol* s = score->getSymbol(i);
        BaseComponent* c = makeComponentFromSymbol(s, false);
        
        getView()->getPageComponent()->addSubcomponent(c);
        c->setScoreSymbolPointer(s);
    }
}

/*=================================
 * MODIFY DATA FROM VIEW
 * (CALLBACKS FROM USER ACTIONS)
 ********************************/
void SymbolistHandler::removeSymbolFromScore(BaseComponent* component)
{
    assert ( component->getScoreSymbolPointer() != NULL ) ;
    //cout << "REMOVING SYMBOL OF " << c << " " << c->getSymbolTypeStr() << " [ " << c->getScoreSymbolPointer() << " ]" << std::endl;
    
    Symbol* symbol = component->getScoreSymbolPointer();
    assert (symbol != NULL ); // that's not normal
    
    log_score_change();

    // cout << "removeSymbolFromScore" << endl;

    symbol->print();

    if (getView())
        getView()->clearInspector();
    
    getModel()->getScore()->removeSymbolTimePoints(symbol);
    getModel()->getScore()->removeSymbol(symbol);
    
    component->setScoreSymbolPointer(NULL);
    executeUpdateCallback(-1);
    
}

/*
 *  the component has changed, and so we need to update it's symbol bundle
 */
void SymbolistHandler::modifySymbolInScore( BaseComponent* c )
{
    
    log_score_change();
    
    // get pointer to symbol attached to component
    Symbol* s = c->getScoreSymbolPointer();
    assert (s != NULL) ;
    
    // cout << c << " ---> modifySymbolInScore " << s->getID() << endl;
    // printRect(c->getBounds(), "component");

    
    // remove current time point for symbol, or if stave remove all symbol timepoints on stave
    getModel()->getScore()->removeSymbolTimePoints(s);
    
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
        getModel()->getScore()->updateStavesAndTimepoints();
    }
    else
    {
        // if the type is not a staff, add the time points for the symbol
        getModel()->getScore()->addSymbolTimePoints( s );
    }
    
    executeUpdateCallback( getModel()->getScore()->getSymbolPosition( s ) );
    
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
    getModel()->getScore()->print();
    
    undo_stack.add( new Score( *getModel()->getScore() ) );
    
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
    redo_stack.add( new Score( *getModel()->getScore() ) );
}


void SymbolistHandler::undo()
{
    if( undo_stack.size() > 0 )
    {
        const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
        
        if ( getView() != NULL )
        {
            push_redo_stack();

            getView()->getPageComponent()->unselectAllComponents();
            getView()->getPageComponent()->clearAllSubcomponents();
            getModel()->getScore()->removeAllSymbols();
            getModel()->setScore(undo_stack.removeAndReturn( undo_stack.size() - 1 ));
            
            addComponentsFromScore();
            getView()->repaint();
        }
        
    }
}

void SymbolistHandler::redo()
{
    if( redo_stack.size() > 0 )
    {
        const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
        
        if ( getView() != NULL )
        {
            push_undo_stack();
            
            getView()->getPageComponent()->unselectAllComponents();
            getView()->getPageComponent()->clearAllSubcomponents();
            getModel()->getScore()->removeAllSymbols();
            getModel()->setScore(redo_stack.removeAndReturn( redo_stack.size() - 1 ));
            
            addComponentsFromScore();
            getView()->repaint();
        }
        
    }
}

void SymbolistHandler::addToInspector( BaseComponent *c )
{
    // only selected and called if the main component is there...
    getView()->setInspectorObject(c);
}

void SymbolistHandler::clearInspector()
{
    getView()->clearInspector();
}

void SymbolistHandler::updateSymbolFromInspector( BaseComponent *c)
{
    c->importFromSymbol( *c->getScoreSymbolPointer() );
    modifySymbolInScore( c );
    //repaint is called in modify symbol
}

void SymbolistHandler::convertSelectedToStaff()
{
    getView()->getPageComponent()->createStaffFromSelected();
}



void SymbolistHandler::copySelectedToClipBoard()
{
    clipboard.clear();
    
    for( auto c : getView()->getPageComponent()->getSelectedItems() )
    {
        clipboard.add(new Symbol( *(dynamic_cast<BaseComponent*>(c))->getScoreSymbolPointer()) );
    }
}

void SymbolistHandler::newFromClipBoard()
{
    auto pc = getView()->getPageComponent();
    
    for( auto s : clipboard )
    {
        // cout << " SymbolistHandler::newFromClipBoard " << endl;
        // cout << s->getID() << endl;
        Symbol* new_sym = new Symbol(*s);
        BaseComponent *c = makeComponentFromSymbol(new_sym, true);
        
        if ( c != NULL)
        {
            pc->addSubcomponent( c );
            c->toFront(true);
            pc->addToSelection( c );
        }

    }
}
