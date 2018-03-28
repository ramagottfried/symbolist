#pragma once

#ifndef SymbolistHandler_h
#define SymbolistHandler_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "Controller.hpp"
#include "SymbolistModel.hpp"

class SymbolistMainComponent;
class SymbolistMainWindow;
class BaseComponent;
class StaffComponent;
class SymbolPropertiesPanel;

class SymbolistHandler : public virtual Controller<SymbolistModel, SymbolistMainComponent>
{
    OwnedArray<Score> undo_stack;
    OwnedArray<Score> redo_stack;
    
    // main window, allocated here in symbolist handler
    ScopedPointer<SymbolistMainWindow> main_window;
    
    // main component, allocated and owned by main window
    // the main view of the editor (could be embedded in a foreign app independently of the window)
    SymbolistMainComponent* main_component_ptr = nullptr;
    
    // the main view of the editor (could be embedded in a foreign app independently of the window)
    SymbolPropertiesPanel* inspector_ptr = nullptr;
    
    OwnedArray<Symbol> clipboard;
    
    // the current play-time in ms (change for float or long_int?)
    float current_time = 0;
    
    // callbacks to the host environment
    symbolistUpdateCallback myUpdateCallback = NULL;
    symbolistCloseCallback myCloseCallback = NULL;
    symbolistTransportCallback myTransportCallback = NULL;
    
    
    bool in_standalone = false;
    
public:
    /*********************************************
     *                CONSTRUCTORS               *
     *********************************************/
    
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
    SymbolistHandler(shared_ptr<SymbolistModel> model, shared_ptr<SymbolistMainComponent> view);
    
    /**
     * SymbolistHandler's default destructor.
     */
    ~SymbolistHandler();
    
    /*********************************************
     * CONTROLLER METHODS CALLED FROM THE API
     *********************************************/
    // main entry point / symbolist factory
    static SymbolistHandler* symbolistAPI_newSymbolist();
 
    // apply on an existing symbolist instance
    void symbolistAPI_freeSymbolist();
    void symbolistAPI_openWindow();
    void symbolistAPI_closeWindow();
    void symbolistAPI_windowToFront();
    void symbolistAPI_windowSetName(String name);
    
    void symbolistAPI_registerUpdateCallback(symbolistUpdateCallback c);
    void symbolistAPI_registerCloseCallback(symbolistCloseCallback c);
    void symbolistAPI_registerTransportCallback(symbolistTransportCallback c);
    
    int symbolistAPI_getNumSymbols();
    OdotBundle_s symbolistAPI_getSymbolBundle_s(int n);
    shared_ptr<Symbol> symbolistAPI_getSymbol(int n);
    
    void symbolistAPI_setOneSymbol( const OdotBundle_s& bundle);
    void symbolistAPI_setSymbols(const OdotBundle_s& bundle_array);
    
    int symbolistAPI_getNumPaletteSymbols();
    shared_ptr<Symbol> symbolistAPI_getPaletteSymbol(int n);
    void symbolistAPI_setOnePaletteSymbol( const OdotBundle_s& bundle);
    void symbolistAPI_setPaletteSymbols(const OdotBundle_s& bundle_array);
    
    void symbolistAPI_setTime(float time_ms);
    void symbolistAPI_toggleTimeCusor();
    
    OdotBundle_s symbolistAPI_getSymbolsAtTime(float t);
    OdotBundle_s symbolistAPI_getScoreBundle();

    OdotBundle_s symbolistAPI_getdurationBundle();
    
    void symbolistAPI_clearScore();
    
    /*********************************************
     * CONTROLLER METHODS CALLED FROM THE GUI
     *********************************************/
    
    void executeUpdateCallback(int arg);
    void executeCloseCallback();
    void executeTransportCallback(int arg);
    
    // functions modifying the parent Windows's score
    void addSymbolToScore ( BaseComponent* c ) ;
    void removeSymbolFromScore ( BaseComponent* c ) ;
    void modifySymbolInScore ( BaseComponent* c ) ;
    
    float getCurrentTime() { return current_time; }
    
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
    
    void setCurrentSymbol(int n);
    int getCurrentSymbolIndex();
    shared_ptr<Symbol> getCurrentSymbol();
    
    BaseComponent* makeComponentFromSymbol( shared_ptr<Symbol> s, bool attach_the_symbol );
    void addComponentsFromScore ();
    
    void inStandalone(){ in_standalone = true; };
    bool isStandalone(){ return in_standalone; };

    const TimePointArray* getTimePointArray() { return getModel()->getScore()->getTimePointArray(); }
    void removeTimePointsForSymbol(shared_ptr<Symbol> s){ getModel()->getScore()->removeSymbolTimePoints( s ); }
    
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
    
    inline void update() override {}
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistHandler)
};

#endif

