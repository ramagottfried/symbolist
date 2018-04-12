#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "PaletteController.hpp"
#include "PageController.hpp"
#include "SymbolistModel.hpp"
#include <iostream>
#include <memory>

class SymbolistMainComponent;
class SymbolistMainWindow;
class BaseComponent;
class StaffComponent;
class SymbolPropertiesPanel;

class SymbolistHandler : public virtual Controller<SymbolistModel, SymbolistMainComponent>
{
    /**
     * The static singleton instance of SymbolistHandler.
     */
    static SymbolistHandler* INSTANCE;
    
    /**
     * A Controller to handle palette-connected actions.
     */
    std::unique_ptr<PaletteController> paletteController;
    
    /**
     * A Controller to handle score-connected actions.
     */
    std::unique_ptr<PageController> pageController;
    
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
    symbolistUpdateCallback myUpdateCallback = NULL;
    
    /**
     * On close callback to the host environment.
     */
    symbolistCloseCallback myCloseCallback = NULL;
    
    /**
     * On transport callback to the host environment.
     * This function is called when the user invokes
     * the time cursor view.
     */
    symbolistTransportCallback myTransportCallback = NULL;
    
    OwnedArray<Symbol> clipboard;
    OwnedArray<Score> undo_stack;
    OwnedArray<Score> redo_stack;
    bool in_standalone = false;
    
    /*********************************************
     *                CONSTRUCTORS               *
     *********************************************/
    /* All constructors are private to implement
     * the singleton design pattern.
     */
    /**
     * SymbolistHandler's default constructor.
     * Creates a model and a view from call to
     * model and view's default constructors.
     */
    SymbolistHandler();
    
    /**
     * SymbolistHandler's constructor with model
     * and view passed as parameters.
     */
    SymbolistHandler(SymbolistModel* model, SymbolistMainComponent* view);
        
public:
    /**
     * SymbolistHandler's default destructor.
     */
    virtual ~SymbolistHandler() override;
    
    /**
     * @return The singleton instance of SymbolistHandler.
     */
    static SymbolistHandler* getInstance();
    
    /*********************************************
     *             GETTERS & SETTERS             *
     *********************************************/
    
    /**
     * @return The PaletteController instance owned by the SymbolistHandler.
     */
    inline PaletteController* getPaletteController() { return paletteController.get(); };
    inline PageController*    getPageController() { return pageController.get(); }
    inline float getCurrentTime() { return current_time; }
    
    /**************************************************************
     *       FACTORY METHODS FOR CHILD CONTROLLERS' CREATION      *
     **************************************************************/
    
    /**
     * Creates and sets up the PaletteController
     * for the singleton instance of SymbolistHandler.
     */
    void createPaletteController();
    
    /**
     * Creates and sets up the PageController
     * for the singleton instance of SymbolistHandler.
     */
    void createPageController();
    
    /********************************************************
     ********************************************************
     *             METHODS CALLED FROM THE API              *
     ********************************************************
     ********************************************************/
    
    /**
     * Returns the SymbolistHandler singleton instance
     * with setted model and child controllers.
     *
     * Normally, this is the first method called when launching
     * the application or creating the Max or OpenMusic
     * object.
     *
     * @return a pointer to the singleton instance of
     *         SymbolistHandler.
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
    void         symbolistAPI_setOneSymbol(const OdotBundle_s& bundle);
    void         symbolistAPI_setSymbols(const OdotBundle_s& bundle_array);
    
    /* Palette controller's methods. */
    int     symbolistAPI_getNumPaletteSymbols();
    Symbol* symbolistAPI_getPaletteSymbol(int n);
    void    symbolistAPI_setOnePaletteSymbol(const OdotBundle_s& bundle);
    void    symbolistAPI_setPaletteSymbols(const OdotBundle_s& bundleArray);
    
    void symbolistAPI_setTime(float time_ms);
    void symbolistAPI_toggleTimeCusor();
    
    OdotBundle_s symbolistAPI_getSymbolsAtTime(float t);
    OdotBundle_s symbolistAPI_getScoreBundle();

    OdotBundle_s symbolistAPI_getdurationBundle();
    
    void symbolistAPI_clearScore();
    
    /*********************************************
     *   CONTROLLER METHODS CALLED FROM THE GUI  *
     *********************************************/
    void executeUpdateCallback(int arg);
    void executeCloseCallback();
    void executeTransportCallback(int arg);
    
    // functions modifying the parent Windows's score
    void removeSymbolFromScore(BaseComponent* c);
    void modifySymbolInScore(BaseComponent* c);
    
    /*********************************************
     *               INSPECTOR IO                *
     *********************************************/
    void addToInspector( BaseComponent *c);
    void clearInspector();
    
    void updateSymbolFromInspector( BaseComponent *c );
    const StringArray getStaves() { return getModel()->getScore()->getStaves(); }
    
    void convertSelectedToStaff();
    StaffComponent* getStaveAtTime( float time );

    /*********************************************
     *       PALETTE AND SYMBOL CONSTRUCTOR      *
     *********************************************/
    
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
    
    Symbol* getSelectedSymbolInPalette();
    
    BaseComponent* makeComponentFromSymbol(Symbol* s, bool attach_the_symbol);
    
    void inStandalone(){ in_standalone = true; };
    bool isStandalone(){ return in_standalone; };

    const TimePointArray* getTimePointArray() { return getModel()->getScore()->getTimePointArray(); }
    void removeTimePointsForSymbol(Symbol* s){ getModel()->getScore()->removeSymbolTimePoints( s ); }
    
    void copySelectedToClipBoard();
    void newFromClipBoard();

    void log_score_change();
    void push_undo_stack();
    void push_redo_stack();
    
    void undo();
    void redo();

    int symbolNameCount( string& name )
    {
        return getModel()->getScore()->getNameCount( name );
    }
    
    bool uniqueIDCheck( string& name )
    {
        return !getModel()->getScore()->idExists( name );
    }
    
    /* Overrides the update method inherited from the Observer class. */
    inline void update() override {}
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistHandler)
};
