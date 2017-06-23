

#ifndef SymbolistHandler_h
#define SymbolistHandler_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "Score.h"

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
    void symbolistAPI_setTime(float time_ms);

    odot_bundle* symbolistAPI_getSymbolsAtTime(float t);
    odot_bundle* symbolistAPI_getScoreBundle();
    
    void symbolistAPI_setOneSymbol( odot_bundle *bundle);
    
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
     * PALETTE AND SYMBOL CONSTRUCTOR
     *********************************************/
    
    void setCurrentSymbol(int n);
    int getCurrentSymbolIndex();
    Symbol* getCurrentSymbol();
    SymbolistPalette* getSymbolPalette() { return &palette; }
    
    static BaseComponent* makeComponentFromSymbol( Symbol *s, bool attach_the_symbol );
    void addComponentsFromScore ();
    
    void inStandalone(){ in_standalone = true; };
    bool isStandalone(){ return in_standalone; };

    const TimePointArray* getTimePointArray() const { return score.getTimePointArray(); }
    
private:
    
    Score score ;

    // the palette is an array of symbol 'templates'
    SymbolistPalette palette;
    
    // the main view of the editor (could be embedded in a foreign app independently of the window)
    SymbolistMainComponent* main_component = NULL;
    
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

