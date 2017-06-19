

#ifndef SymbolistHandler_h
#define SymbolistHandler_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "Score.h"
//#include "SymbolistMainComponent.h"
//#include "BaseComponent.h"

class SymbolistMainComponent;
class BaseComponent;

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
    void symbolistAPI_registerUpdateCallback(symbolistUpdateCallback c);
    void symbolistAPI_registerCloseCallback(symbolistCloseCallback c);
    void symbolistAPI_registerTransportCallback(symbolistTransportCallback c);
    int symbolistAPI_getNumSymbols();
    odot_bundle* symbolistAPI_getSymbol(int n);
    void symbolistAPI_setSymbols(int n, odot_bundle **bundle_array);
    void symbolistAPI_setTime(int time_ms);
    
    
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
    
    int getCurrentTime() { return current_time; }
    
    /*********************************************
     * PALETTE AND SYMBOL CONSTRUCTOR
     *********************************************/
    
    void setCurrentSymbol(int n);
    int getCurrentSymbolIndex();
    Symbol* getCurrentSymbol();
    SymbolistPalette* getSymbolPalette() { return &palette; }
    
    static BaseComponent* makeComponentFromSymbol(const Symbol *s);
    void addComponentsFromScore ();
    
private:
    
    Score score ;

    // the palette is an array of symbol 'templates'
    SymbolistPalette palette;
 
    // the main view of the editor (could be embedded in a foreign app independently of the window)
    SymbolistMainComponent* main_component;
    
    // the current play-time in ms (change for float or long_int?)
    int current_time = 0;
    
    // callbacks to the host environment
    symbolistUpdateCallback myUpdateCallback = NULL;
    symbolistCloseCallback myCloseCallback = NULL;
    symbolistTransportCallback myTransportCallback = NULL;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistHandler)
};

#endif

