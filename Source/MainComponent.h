
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "Score.h"
#include "Symbol.h"
#include "PageComponent.h"
#include "PaletteComponent.h"
#

/*
 * SymbolistMainComponent is the main controller of the application
 * managing the connection between data (score) and visualization/editing.
 * It is also the node and pointer for interaction with the library
 */

class SymbolistMainComponent : public SymbolistComponent, public KeyListener
{
public:
    
    SymbolistMainComponent();
    ~SymbolistMainComponent();

    /*********************************************
     * CONTROLLER METHODS CALLED FROM THE LIB API
     *********************************************/
    static SymbolistMainComponent* symbolistAPI_createWindow();
    void symbolistAPI_closeWindow();
    void symbolistAPI_windowToFront();
    void symbolistAPI_windowSetName(String name);
    void symbolistAPI_registerUpdateCallback(symbolistUpdateCallback c);
    void symbolistAPI_registerCloseCallback(symbolistCloseCallback c);
    int symbolistAPI_getNumSymbols();
    odot_bundle* symbolistAPI_getSymbol(int n);
    void symbolistAPI_setSymbols(int n, odot_bundle **bundle_array);
    
    
    
    /*********************************************
     * CONTROLLER METHODS CALLED FROM THE GUI
     *********************************************/
    
    void executeUpdateCallback(int arg);
    void executeCloseCallback();
    
    // functions modifying the parent Windows's score
    void notifyNewSymbol ( Symbol* s ) ;
    void notifySymbolRemoved ( Symbol* s ) ;
    void notifySymbolChange ( Symbol* s ) ;
    
    /*********************************************
     * STANDARD GUI FUNCTIONALITY AND TOOLS
     *********************************************/
    
    // Redefinition of methods from Juce::Component
    void resized() override;
    bool keyPressed (const KeyPress& key, Component* originatingComponent) override;
    void modifierKeysChanged (const ModifierKeys& modifiers) override;

    void setEditMode( UI_EditType m );
    UI_EditType getEditMode();
    
    void setCurrentSymbol(int n);
    int getCurrentSymbolIndex();
    Symbol* getCurrentSymbol();
    static BaseComponent* makeComponentFromSymbol(const Symbol *s);
    
    // Redefine these from SymbolistComponent
    inline SymbolistComponent* getPageComponent() override { return &scoreGUI; }
    inline SymbolistComponent* getMainComponent() override { return this; }
    
    // temporary 
    bool shift_down = false;
    
    
private:
    
    // the score data (model)
    Score score ;
    inline Score* getScore() { return &score; }
    // the score view
    PageComponent scoreGUI;
   
    // the palette is an array of symbol 'templates'
    //OwnedArray<BaseComponent> palette;
    SymbolistPalette palette;
    PaletteComponent paletteView ;
    
    UI_EditType     mouse_mode = edit;
    
    // callbacks to the host environment
    symbolistUpdateCallback myUpdateCallback = NULL;
    symbolistCloseCallback myCloseCallback = NULL;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};
