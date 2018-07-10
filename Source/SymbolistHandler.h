#pragma once

#include "JuceHeader.h"
#include "PaletteController.hpp"
#include "PageController.hpp"
#include "InspectorController.hpp"
#include "SymbolistModel.hpp"
#include <iostream>
#include <memory>

class SymbolistMainComponent;
class SymbolistMainWindow;
class BaseComponent;
class StaffComponent;
class SymbolPropertiesPanel;
class CodeBoxController;

/**
 * Describes the main controller class of the symbolist application.
 * The SymbolistHandler class is also the entry point to read/write
 * the symbolist score from the outside.
 *
 * All methods prefixed by symbolistAPI in the SymbolistHandler class
 * are called by methods of the symbolist C API used in the Max and OpenMusic version
 * of the application.
 */
class SymbolistHandler : public virtual Controller<SymbolistModel, SymbolistMainComponent> {
        
public:

    /*********************************************
     *                CONSTRUCTORS               *
     *********************************************/
	
    /**
     * SymbolistHandler's default constructor.
     * Creates a model and sets all the child controllers
     * for this SymbolistHandler instance.
     */
    SymbolistHandler();
	
    /**
     * SymbolistHandler's default destructor.
     */
    virtual ~SymbolistHandler() override;
    
    /*********************************************
     *             GETTERS & SETTERS             *
     *********************************************/
	
	/**
     * @return A reference to the application main window.
     */
	inline SymbolistMainWindow* getSymbolistMainWindow() { return main_window.get(); }
	
    /**
     * @return The PaletteController instance owned by this SymbolistHandler.
     */
    inline PaletteController* getPaletteController() { return palette_controller.get(); };
	
	/**
     * @return The PageController instance owned by this SymbolistHandler.
     */
    inline PageController* getPageController() { return page_controller.get(); }
	
	/**
     * @return The InspectorController instance owned by this SymbolistHandler.
     */
    inline InspectorController* getInspectorController() { return inspector_controller.get(); }
	
	/**
     * @return The CodeBoxController instance owned by this SymbolistHandler.
     */
    inline CodeBoxController* getCodeBoxController() { return code_box_controller.get(); }
	
	/**
	 * @return The current playtime in milliseconds.
	 */
    inline float getCurrentTime() { return current_time; }
	
	void inStandalone(){ in_standalone = true; };
    bool isStandalone(){ return in_standalone; };
	const StringArray getStaves() { return getModel()->getScore()->getStaves(); }
    StaffComponent* getStaveAtTime( float time );
	
    /**************************************************************
     *       FACTORY METHODS FOR CHILD CONTROLLERS' CREATION      *
     **************************************************************/
    
    /**
     * Creates and sets up the PaletteController
     * for this instance of SymbolistHandler.
     */
    void createPaletteController();
    
    /**
     * Creates and sets up the PageController
     * for this instance of SymbolistHandler.
     */
    void createPageController();
	
	/**
     * Creates and sets up the InspectorController
     * for this instance of SymbolistHandler.
     */
    void createInspectorController();
	
	/**
     * Creates and sets up the CodeBoxController
     * for this instance of SymbolistHandler.
     */
    void createCodeBoxController();
	
    /********************************************************
     ********************************************************
     *             METHODS CALLED FROM THE API              *
     ********************************************************
     ********************************************************/
    
    /**
     * Returns an instance of SymbolistHandler
     * with set up model and child controllers.
     *
     * Normally, this is the first method called when launching
     * the application or creating the Max or OpenMusic
     * object.
     *
     * @return a pointer to an instance of SymbolistHandler.
     */
    static SymbolistHandler* symbolistAPI_newSymbolist();
    
    /* Symbolist application lifecycle's methods. */
    void symbolistAPI_freeSymbolist();
    void symbolistAPI_openWindow();
    void symbolistAPI_closeWindow();
    void symbolistAPI_windowToFront();
    void symbolistAPI_windowSetName(String name);
    
    void symbolistAPI_registerUpdateCallback(symbolistUpdateCallback c);
    void symbolistAPI_registerCloseCallback(symbolistCloseCallback c);
    void symbolistAPI_registerTransportCallback(symbolistTransportCallback c);
    
    /* Score controller's methods. */
    int          symbolistAPI_getNumSymbols();
    OdotBundle_s symbolistAPI_getSymbolBundle_s(int n);
    Symbol*      symbolistAPI_getSymbol(int n);
    void         symbolistAPI_setOneSymbol( const OdotBundle_s& bundle );
    void         symbolistAPI_setSymbols( const OdotBundle_s& bundle );
    OdotBundle_s symbolistAPI_getSymbolsAtTime(float t);
    OdotBundle_s symbolistAPI_getScoreBundle();
    OdotBundle_s symbolistAPI_getdurationBundle();
	void 		 symbolistAPI_clearScore();
	
    /* File IO controller's methods. */
    int         symbolistAPI_importSVG( const char * filename );
    int         symbolistAPI_exportSVG( const char * filename );

    /* Palette controller's methods. */
    int     symbolistAPI_getNumPaletteSymbols();
    Symbol* symbolistAPI_getPaletteSymbol(int n);
    void    symbolistAPI_setOnePaletteSymbol(const OdotBundle_s& bundle);
    void    symbolistAPI_setPaletteSymbols(const OdotBundle_s& bundleArray);
    
    void symbolistAPI_setTime(float time_ms);
    void symbolistAPI_toggleTimeCusor();
    
    /*********************************************
     *   CALLBACK METHODS CALLED FROM THE GUI  *
     *********************************************/
    void executeUpdateCallback(int arg);
    void executeCloseCallback();
    void executeTransportCallback(int arg);
	
    /*********************************************
     *               INSPECTOR IO                *
     *********************************************/
    void addToInspector(BaseComponent *c);
    void clearInspector();

    /*******************************************************
     *       SYMBOL CREATION AND MODIFICATION METHODS      *
     *******************************************************/
    /**
     * Creates a new symbol in the score copied from
     * the selected template in the palette.
     *
     * @return a reference to the newly created symbol.
     */
    Symbol* createSymbolFromTemplate();
    
    /**
     * Creates a new symbol in the score.
     *
     * @return a pointer to the newly created symbol
     *         in the score.
     */
    Symbol* createSymbol();
    	
    /**
     *
     * Creates a new graphic component from the symbol in parameter.
     *
     * @param symbol          the symbol from which the new graphic component
     *				          will be created.
     *
     * @param attachTheSymbol a boolean indicating if the symbol in
     *						  parameter should be attached to the newly
     *						  created graphic component.
     *						  If <code>true</code> then the symbol is attached
     *                        to the graphic component by a call to the
     *						  BaseComponent::setScoreSymbolPointer() method.
     *
     * @return                a pointer to the newly created BaseComponent.
     *
     * @see					  BaseComponent#setScoreSymbolPointer(Symbol*)
     *						  BaseComponent::setScoreSymbolPointer
     */
    BaseComponent* makeComponentFromSymbol(Symbol* symbol, bool attachTheSymbol);
    
    void removeSymbolFromScore(BaseComponent* c);
    void modifySymbolInScore(BaseComponent* c);
	
    /**
	 * Passes on the changes applied to the BaseComponent
	 * in paremeter to its attached symbol in the score.
	 *
	 * @param component the modified graphic component.
	 */
    void updateSymbolFromComponent(BaseComponent *component);
	
	/*********************************************
     *           UNDO/REDO COMMAND METHODS       *
     *********************************************/
    void scoreChanged();
    void push_undo_stack();
    void push_redo_stack();
    
    void undo();
    void redo();

	/*********************************************
     *           ID GENERATION METHODS           *
     *********************************************/
    int symbolNameCount( string& name )
    {
		return getModel()->getScore()->getNameCount( name );
    }
    
    bool uniqueIDCheck( string& idToCheck )
    {
        return !getModel()->getScore()->idExists( idToCheck );
    }
	
	/**
	 * Creates a unique id based on the name in parameter.
	 * The created id is of the form "{name}/{number}",
	 * where name is the name in parameter and number is
	 * calculated depending on the existing ids in the score.
	 *
	 * @param name the name of a symbol or a graphic component
	 *             serving as a base to create the new id.
	 *
	 * @return     a string representing a unique id in the score,
	 *			   meaning that no symbol or graphic component
	 *             possess this id.
	 */
	string createIdFromName(string& name);
	
    /* Overrides the update method inherited from the Observer class. */
    inline void update() override {}

private:
	
    /**
     * A Controller to handle palette-related actions.
     */
    unique_ptr<PaletteController >     palette_controller;
	
    /**
     * A Controller to handle score-related actions.
     */
    unique_ptr<PageController >        page_controller;

	/**
     * A Controller to handle inspector-related actions.
     */
    unique_ptr<InspectorController >   inspector_controller;
	
    /**
     * A Controller to handle actions related to the odot expressions
     * code editor.
     */
     unique_ptr<CodeBoxController>     code_box_controller;
	
    /**
     * The main graphic window of the application.
     * Normally, SymbolistMainWindow should be defined as
     * the view of SymbolistHandler class, but since it is
     * only a wrapper window around the main graphic component
     * it is only referenced here as an instance variable.
     */
    std::unique_ptr<SymbolistMainWindow> main_window;
	
    /**
     * The current play-time in ms.
     * (change for float or long_int?)
     */
    float current_time = 0;
	
    /**
     * On update callback to the host environment.
     */
    symbolistUpdateCallback my_update_callback = NULL;
	
    /**
     * On close callback to the host environment.
     */
    symbolistCloseCallback my_close_callback = NULL;
	
    /**
     * On transport callback to the host environment.
     * This function is called when the user invokes
     * the time cursor view.
     */
    symbolistTransportCallback my_transport_callback = NULL;
	
    OwnedArray<Score> undo_stack;
    OwnedArray<Score> redo_stack;
	
    bool in_standalone = false;
	
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistHandler)
};
