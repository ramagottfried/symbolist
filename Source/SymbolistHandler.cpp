#include "SymbolistHandler.h"

#include "SymbolistMainWindow.h"

#include "StaffComponent.hpp"
#include "SymbolGroupComponent.h"
#include "BasicShapePathComponents.h"
#include "PathBaseComponent.h"
#include "TextGlyphComponent.h"
#include "SmuflComponent.hpp"

SymbolistHandler::SymbolistHandler()
{
    MessageManager::getInstance(); // this wasn't necessary before, I think there might be some JUCE code starting too soon now?
	DEBUG_FULL("Instance address: " << this << endl)
	
    // Instantiates the model and the palette default items.
    SymbolistModel* model = new SymbolistModel();
    model->getPalette()->createDefaultItems();
		
    setModel(model);
	
    // Creates the child controllers.
	createPaletteController();
    createPageController();
    createMouseModeController();
    createInspectorController();
    createTimeDisplayController();
	
    /* Adds the SymbolistHandler instance and
     * all its child controllers as observers of the model.
     */
    getModel()->attach(this);
    getModel()->attach(palette_controller.get());
    getModel()->attach(page_controller.get());
    getModel()->attach(mouse_mode_controller.get());
	getModel()->attach(inspector_controller.get());
	getModel()->attach(time_display_controller.get());
	
}

SymbolistHandler::~SymbolistHandler()
{
    DEBUG_FULL("Deleting symbolist handler, main component pointer: "
    			<< getView() <<  " window " << main_window.get() << endl)
    
    if (getView() != NULL)
        symbolistAPI_closeWindow();
    
    if (getModel() != NULL)
        delete getModel();
}

void SymbolistHandler::createPaletteController()
{
    /* Creates the palette_controller and sets its model
     * and parent controller.
     */
    palette_controller = unique_ptr<PaletteController>(new PaletteController());
    palette_controller->setParentController(this);
    palette_controller->setModel(getModel());
}

void SymbolistHandler::createPageController()
{
    /* Creates the page_controller and sets its model
     * and parent controller.
     */
    page_controller = unique_ptr<PageController>(new PageController());
    page_controller->setParentController(this);
    page_controller->setModel(getModel());
}

void SymbolistHandler::createMouseModeController()
{
    /* Creates the mouse_mode_controller and sets its model
     * and parent controller.
     */
    mouse_mode_controller = unique_ptr<MouseModeController>(new MouseModeController());
    mouse_mode_controller->setParentController(this);
    mouse_mode_controller->setModel(getModel());
}

void SymbolistHandler::createInspectorController()
{
	/* Creates the mouse_mode_controller and sets its model
     * and parent controller.
     */
    inspector_controller = unique_ptr<InspectorController >(new InspectorController());
    inspector_controller->setParentController(this);
    inspector_controller->setModel(getModel());
}

void SymbolistHandler::createTimeDisplayController()
{
	/* Creates the time_display_controller and sets its model
     * and parent controller.
     */
    time_display_controller = unique_ptr<TimeDisplayController >(new TimeDisplayController());
    time_display_controller->setParentController(this);
    time_display_controller->setModel(getModel());
}

/*********************************************
 * CONTROLLER METHODS CALLED FROM THE LIB API
 *********************************************/

// Returns the new SymbolistHandler (this is a static method).
SymbolistHandler* SymbolistHandler::symbolistAPI_newSymbolist()
{
    DEBUG_TRACE()
    return new SymbolistHandler();
}

void SymbolistHandler::symbolistAPI_freeSymbolist()
{
    delete this;
}

void SymbolistHandler::symbolistAPI_openWindow()
{
    DEBUG_FULL("Current thread address: " << Thread::getCurrentThread() << endl)
    DEBUG_FULL("Current MessageManeger address: " << MessageManager::getInstance() << endl)
    
    const MessageManagerLock mml;
    
    /* Creates the SymbolistMainWindow instance which in its turn
     * creates the SymbolistMainComponent instance.
     */
    main_window = unique_ptr<SymbolistMainWindow>(new SymbolistMainWindow(this));
    
    /* Sets the corresponding views for SymbolistHandler's instance
     * and all its child controllers.
     */
    setView(main_window->getMainComponent());
    palette_controller->setView(getView()->getPaletteView());
    page_controller->setView(getView()->getScoreView());
	mouse_mode_controller->setView(getView()->getMouseModeView());
	inspector_controller->setView(getView()->getInspectorView());
	time_display_controller->setView(getView()->getTimeDisplayView());
	
    // Populates palette and gives focus to the main view.
    page_controller->addComponentsFromScore();
    getView()->grabKeyboardFocus();

}

void SymbolistHandler::symbolistAPI_closeWindow()
{
	DEBUG_TRACE()
    MessageManagerLock mml;
	
  	/* Calls the destructor of SymbolistMainWindow
	 * therefore invoking the destructor of SymbolistMainComponent.
  	 */
    if (main_window)
        main_window = nullptr;
	
	// Unsets the view references for each controller.
	setView(NULL);
	palette_controller->setView(NULL);
	page_controller->setView(NULL);
	mouse_mode_controller->setView(NULL);
	inspector_controller->setView(NULL);
	time_display_controller->setView(NULL);
	
}

void SymbolistHandler::symbolistAPI_windowToFront()
{
    const MessageManagerLock mml;

    if ( getView() != NULL)
        getView()->getTopLevelComponent()->toFront(true);

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
    my_update_callback = c;
}

void SymbolistHandler::symbolistAPI_registerCloseCallback(symbolistCloseCallback c)
{
    my_close_callback = c;
}

void SymbolistHandler::symbolistAPI_registerTransportCallback(symbolistTransportCallback c)
{
    my_transport_callback = c;
}

int SymbolistHandler::symbolistAPI_getNumSymbols()
{
    return page_controller->getCountOfSymbols();
}

Symbol* SymbolistHandler::symbolistAPI_getSymbol(int n)
{
    try
    {
        // May throw length_error exception.
        return page_controller->getSymbolAtIndex(n);
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
        return page_controller->getSymbolAtIndex(n)->serialize();
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
    
    Symbol* symbol = page_controller->setOneSymbol(bundle);
    
    if (getView() != nullptr)
    {
		// Calls internally the executeUpdateCallback.
        BaseComponent* newComponent = page_controller->makeComponentFromSymbol(symbol, true);
        page_controller->getView()->addSubcomponent(newComponent);
	}
    else
    {
        executeUpdateCallback( -1 );
        DEBUG_FULL(" Main component is NULL.")
    }
}

void SymbolistHandler::symbolistAPI_setSymbols( const OdotBundle_s& bundle )
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    
    page_controller->importSymbols( bundle );
    
    if ( getView() != NULL)
    {
        page_controller->clearAllSubcomponents();
        page_controller->addComponentsFromScore();
    }
}

int SymbolistHandler::symbolistAPI_importSVG( const char * filename )
{
    return getModel()->getScore()->importSVG( filename );
}

int SymbolistHandler::symbolistAPI_exportSVG( const char * filename )
{
    return getModel()->getScore()->exportSVG( filename );
}

int SymbolistHandler::symbolistAPI_getNumPaletteSymbols()
{
    return palette_controller->getNumPaletteSymbols();
}

Symbol* SymbolistHandler::symbolistAPI_getPaletteSymbol(int n)
{
    return palette_controller->getPaletteSymbol(n);
}

void SymbolistHandler::symbolistAPI_setOnePaletteSymbol(const OdotBundle_s& bundle)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    palette_controller->setOnePaletteSymbol(bundle);
}

void SymbolistHandler::symbolistAPI_setPaletteSymbols(const OdotBundle_s& bundle_array)
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope
    palette_controller->setPaletteSymbols(bundle_array);
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
    return page_controller->getSymbolsAtTime(t);
}

OdotBundle_s SymbolistHandler::symbolistAPI_getdurationBundle()
{
    return page_controller->getDurationBundle();
}

OdotBundle_s SymbolistHandler::symbolistAPI_getScoreBundle()
{
    return page_controller->getScoreBundle();
}

void SymbolistHandler::symbolistAPI_clearScore()
{
    const MessageManagerLock mmLock; // Will lock the MainLoop until out of scope

    if ( getView() != NULL )
		page_controller->clearAllSubcomponents();
    
    page_controller->removeAllSymbols();
}


/*******************************
 * these methods are called from symbolist to notify the host environment
 *******************************/
void SymbolistHandler::executeCloseCallback()
{
    if (my_close_callback) { my_close_callback( this ); }
}

void SymbolistHandler::executeUpdateCallback(int arg)
{
    // DEBUG_FULL("executeUpdateCallback" << endl)
    if (my_update_callback) { my_update_callback( this, arg ); }
}

void SymbolistHandler::executeTransportCallback(int arg)
{
    if (my_transport_callback) { my_transport_callback( this, arg ); }
}

//=================================
// SCORE
//=================================
Symbol* SymbolistHandler::createSymbolFromTemplate()
{
    return page_controller->getModel()
    					  ->addSymbolToScore( palette_controller->getSelectedSymbolInPalette() );
}

Symbol* SymbolistHandler::createSymbol()
{
    return getModel()->getScore()->createSymbol();
}

StaffComponent* SymbolistHandler::getStaveAtTime(float time)
{
    return page_controller->getStaveAtTime(time);
}

//=================================
// MODIFY VIEW FROM DATA
//=================================

// Component factory
BaseComponent* SymbolistHandler::makeComponentFromSymbol(Symbol* symbol, bool attachTheSymbol)
{
    DEBUG_FULL("Creating component from Symbol: ")
    
    string typeofSymbol = symbol->getMessage("/type").getString();
    if (typeofSymbol.size() == 0)
    {
		DEBUG_INLINE("Could not find '/type' message in OSC Bundle.. " << endl)
        return NULL;
        
    } else {
        
        DEBUG_INLINE(typeofSymbol << endl)
        BaseComponent* newComponent;
        
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
		} else if ( typeofSymbol == "smufl" ) {
			newComponent = new SmuflComponent();
        } else {
		  	DEBUG_FULL("Unknown symbol type : " << typeofSymbol << endl)
            newComponent = NULL;
        }
        
        if (newComponent != NULL)
        {
            // reads base component symbol values, and sets component bounds for display
            newComponent->importFromSymbol(symbol);
			
            if (attachTheSymbol)
            {
            	// initializes object specific messages if not present
				newComponent->addSymbolMessages(symbol);
                newComponent->setScoreSymbol(symbol);
                getModel()->getScore()->addStaff(symbol); // << /type checked internally and added if staff
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
    assert ( component->getScoreSymbol() != NULL ) ;
    
    Symbol* symbol = component->getScoreSymbol();
    assert (symbol != NULL ); // that's not normal
    
    log_score_change();

    // symbol->print();

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
    
    component->setScoreSymbol(NULL);
    executeUpdateCallback(-1);
    
}

/*
 *  the component has changed, and so we need to update it's symbol bundle
 */
void SymbolistHandler::modifySymbolInScore( BaseComponent* component )
{
    log_score_change();
    
    // get pointer to symbol attached to component
    Symbol* symbol = component->getScoreSymbol();
    assert (symbol != NULL) ;
    
    // DEBUG_FULL(c << " ---> modifySymbolInScore " << s->getID() << endl)
    // printRect(c->getBounds(), "component");

    
    // Remove current time point for symbol, or if stave remove all symbol timepoints on stave
    getModel()->getScore()->removeSymbolTimePoints(symbol);
	
    
    // update the symbol with the component's current state
    component->addSymbolMessages( symbol );
    
    if ( symbol->getType() == "staff" )
    {
        DEBUG_FULL("Type staff " << endl)
        // If the type is "staff" resort the stave order and update time point array
        getModel()->getScore()->updateStavesAndTimepoints();
    }
    else
        // If the type is not a staff, add the time points for the symbol
        getModel()->getScore()->addSymbolTimePoints( symbol );
    
    executeUpdateCallback( getModel()->getScore()->getSymbolPosition( symbol ) );
    
    component->repaint();
    
}

/***
 * called when something is changed, added, deleted (not but not undo)
 ***/
void SymbolistHandler::log_score_change()
{
    redo_stack.clear();
    push_undo_stack();
    
    //symbolistAPI_exportSVG( nullptr );
}

void SymbolistHandler::push_undo_stack()
{
    // DEBUG_FULL("Previous score :" << endl)
    // getModel()->getScore()->print();
    
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
            getModel()->setScore( undo_stack.removeAndReturn( undo_stack.size() - 1 ) );
            
            page_controller->addComponentsFromScore();
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
            
            page_controller->addComponentsFromScore();
            getView()->repaint();
        }
        
    }
}

void SymbolistHandler::addToInspector( BaseComponent *c )
{
    // only selected and called if the main component is there...
    inspector_controller->addToInspector(c);
}

void SymbolistHandler::clearInspector()
{
    inspector_controller->clearInspector();
}

void SymbolistHandler::updateSymbolFromComponent(BaseComponent* component)
{
    component->importFromSymbol( *component->getScoreSymbol() );
    modifySymbolInScore( component ); // repaint is called in modify symbol
	
}

string SymbolistHandler::createIdFromName(string& name)
{
	string id;
	int count = symbolNameCount( name );
	
	id = name + "/" + to_string(count);

	// Check to see if there are others with this id and then increment 1.
	while(!uniqueIDCheck(id))
		id = name + "/" + to_string(count++);
	
	return id;
}
