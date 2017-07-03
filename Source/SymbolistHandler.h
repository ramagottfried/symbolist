

#ifndef SymbolistHandler_h
#define SymbolistHandler_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "Score.h"

class SymbolistMainWindow;
class SymbolistMainComponent;
class BaseComponent;

class InspectorWindow;
class SymbolPropertiesPanel;


class SymbolistHandler
{
    
public:

    SymbolistHandler();
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
    void symbolistAPI_openInspectorWindow();
    void symbolistAPI_closeInspectorWindow();
    void symbolistAPI_toggleInspectorWindow();
    
    void symbolistAPI_registerUpdateCallback(symbolistUpdateCallback c);
    void symbolistAPI_registerCloseCallback(symbolistCloseCallback c);
    void symbolistAPI_registerTransportCallback(symbolistTransportCallback c);
    
    int symbolistAPI_getNumSymbols();
    odot_bundle* symbolistAPI_getSymbol(int n);
    void symbolistAPI_setOneSymbol( odot_bundle *bundle);
    void symbolistAPI_setSymbols(int n, odot_bundle **bundle_array);
    
    int symbolistAPI_getNumPaletteSymbols();
    odot_bundle* symbolistAPI_getPaletteSymbol(int n);
    void symbolistAPI_setOnePaletteSymbol( odot_bundle *bundle);
    void symbolistAPI_setPaletteSymbols(int n, odot_bundle **bundle_array);
    
    void symbolistAPI_setTime(float time_ms);

    odot_bundle* symbolistAPI_getSymbolsAtTime(float t);
    odot_bundle* symbolistAPI_getScoreBundle();
    
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
    
    void addToInspector( BaseComponent *c);
    void clearInspector();
    
    void updateSymbolFromInspector( BaseComponent *c, Symbol& s );
    
    /*********************************************
     * PALETTE AND SYMBOL CONSTRUCTOR
     *********************************************/
    
    void setCurrentSymbol(int n);
    int getCurrentSymbolIndex();
    Symbol* getCurrentSymbol();
    SymbolistPalette* getSymbolPalette() { return &palette; }
    
    BaseComponent* makeComponentFromSymbol( Symbol *s, bool attach_the_symbol );
    void addComponentsFromScore ();
    
    void inStandalone(){ in_standalone = true; };
    bool isStandalone(){ return in_standalone; };

    const TimePointArray* getTimePointArray() const { return score.getTimePointArray(); }
    
    void copySelectedToClipBoard();
    void newFromClipBoard();
private:
    
    Score score ;

    // the palette is an array of symbol 'templates'
    SymbolistPalette                    palette;
    
    // main window, allocated here in symbolist handler
    ScopedPointer<SymbolistMainWindow>  main_window;
    
    // main component, allocated and owned by main window
    // the main view of the editor (could be embedded in a foreign app independently of the window)
    SymbolistMainComponent*             main_component_ptr;
    
    // the main view of the editor (could be embedded in a foreign app independently of the window)
    ScopedPointer<InspectorWindow>      inspector_window;
    SymbolPropertiesPanel*              inspector_ptr;
    
    OwnedArray<Symbol>   clipboard;
    
    // the current play-time in ms (change for float or long_int?)
    float current_time = 0;
    
    // callbacks to the host environment
    symbolistUpdateCallback myUpdateCallback = NULL;
    symbolistCloseCallback myCloseCallback = NULL;
    symbolistTransportCallback myTransportCallback = NULL;
    
    
    bool in_standalone = false;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistHandler)
};

#endif

