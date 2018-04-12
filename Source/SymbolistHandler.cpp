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
    /* Creates the paletteController and sets its model
     * and parent controller.
     */
    this->paletteController = unique_ptr<PaletteController>(new PaletteController());
    this->paletteController->setParentController(this);
    this->paletteController->setModel(getModel());
}

void SymbolistHandler::createPageController()
{
    /* Creates the pageController and sets its model
     * and parent controller.
     */
    this->pageController = unique_ptr<PageController>(new PageController());
    this->pageController->setParentController(this);
    this->pageController->setModel(getModel());
}

/*********************************************
 * CONTROLLER METHODS CALLED FROM THE LIB API
 *********************************************/

// Returns the new SymbolistHandler (this is a static method).
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
    
    // Creates the child controllers.
    SymbolistHandler::getInstance()->createPaletteController();
    SymbolistHandler::getInstance()->createPageController();
    
    /* Adds the SymbolistHandler instance and
     * all its child controllers as observers of the model.
     */
    SymbolistHandler::getInstance()->getModel()->attach(SymbolistHandler::getInstance());
    SymbolistHandler::getInstance()->getModel()->attach(SymbolistHandler::getInstance()
                                                        ->getPaletteController());
    SymbolistHandler::getInstance()->getModel()->attach(SymbolistHandler::getInstance()
                                                        ->getPageController());
    
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
    
    /* Sets the corresponding views for SymbolistHandler's instance
     * and all its child controllers.
     */
    setView(main_window->getMainComponent());
    paletteController->setView(getView()->getPaletteView());
    pageController->setView(getView()->getScoreView());
    
    pageController->addComponentsFromScore();
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
    return pageController->getCountOfSymbols();
}

Symbol* SymbolistHandler::symbolistAPI_getSymbol(int n)
{
    try
    {
        // May throw length_error exception.
        return pageController->getSymbolAtIndex(n);
    }
    catch (length_error &error)
    {
        cout << error.what() << endl;
        return NULL;
    }
}

OdotBundle_s SymbolistHandler::symbolistAPI_getSymbolBundle_s(int n)
{
    try
    {
        // May throw length_error exception.
        return pageController->getSymbolAtIndex(n)->serialize();
    }
    catch (length_error &error)
    {
        cout << error.what() << endl;
        return OdotBundle_s();
    }
    
}

void SymbolistHandler::symbolistAPI_setOneSymbol(const OdotBundle_s& bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    Symbol* symbol = pageController->setOneSymbol(bundle);
    
    if (getView() != nullptr)
        // Calls internally the executeUpdateCallback.
        pageController->makeComponentFromSymbol(symbol, true);
    else
    {
        executeUpdateCallback( -1 );
        cout << __func__ << " Main component is NULL." << endl;
    }
}

void SymbolistHandler::symbolistAPI_setSymbols(const OdotBundle_s& bundleArray)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    pageController->importScoreFromOSC(bundleArray);
    
    if ( getView() != NULL)
    {
        pageController->clearAllSubcomponents();
        pageController->addComponentsFromScore();
    }
}

int SymbolistHandler::symbolistAPI_importSVG( const char * filename )
{
    return getModel()->getScore()->importSVG( filename );

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

OdotBundle_s SymbolistHandler::symbolistAPI_getSymbolsAtTime( float t )
{
    return pageController->getSymbolsAtTime(t);
}

OdotBundle_s SymbolistHandler::symbolistAPI_getdurationBundle()
{
    return pageController->getDurationBundle();
}

OdotBundle_s SymbolistHandler::symbolistAPI_getScoreBundle()
{
    return pageController->getScoreBundle();
}

void SymbolistHandler::symbolistAPI_clearScore()
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope

    if ( getView() != NULL )
        pageController->clearAllSubcomponents();
    
    pageController->removeAllSymbols();
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
    return pageController->getModel()->addSymbolToScore(getSelectedSymbolInPalette());
}

Symbol* SymbolistHandler::createSymbol()
{
    return getModel()->getScore()->createSymbol();
}

StaffComponent* SymbolistHandler::getStaveAtTime(float time)
{
    return pageController->getStaveAtTime(time);
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

/*=================================
 * MODIFY DATA FROM VIEW
 * (CALLBACKS FROM USER ACTIONS)
 ********************************/
void SymbolistHandler::removeSymbolFromScore(BaseComponent* component)
{
    assert ( component->getScoreSymbolPointer() != NULL ) ;
    
    Symbol* symbol = component->getScoreSymbolPointer();
    assert (symbol != NULL ); // that's not normal
    
    log_score_change();

    symbol->print();

    if (getView())
        getView()->clearInspector();
    
    // Throws exceptions if symbol is NULL or score is empty
    try
    {
        getModel()->getScore()->removeSymbolTimePoints(symbol);
        getModel()->getScore()->removeSymbol(symbol);
    }
    catch(exception& e)
    {
        cout << e.what() << endl;
    }
    
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
            
            pageController->addComponentsFromScore();
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
            
            pageController->addComponentsFromScore();
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
