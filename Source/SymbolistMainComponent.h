
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistHandler.h"
#include "PageComponent.h"
#include "PaletteComponent.h"
#include "BaseComponent.h"


/*
 * SymbolistMainComponent is the main controller of the application
 * managing the connection between data (score) and visualization/editing.
 * It is also the node and pointer for interaction with the library
 */

class SymbolistMainComponent : public SymbolistComponent, public KeyListener
{

public:
    
    SymbolistMainComponent(SymbolistHandler *sh);
    ~SymbolistMainComponent();
    
    /*********************************************
     * GUI FUNCTIONALITY AND TOOLS
     *********************************************/
    
    // Redefinition of methods from Juce::Component
    void resized() override;
    bool keyPressed (const KeyPress& key, Component* originatingComponent) override;
    void modifierKeysChanged (const ModifierKeys& modifiers) override;

    void setEditMode( UI_EditType m );
    UI_EditType getEditMode();
    
    // Redefine these from SymbolistComponent
    inline SymbolistComponent* getPageComponent() override { return &scoreView; }
    inline SymbolistHandler* getSymbolistHandler() override { return symbolist_handler; }
    
    void clearScoreView () { scoreView.clearAllSubcomponents(); }

    void addSymbolComponent( BaseComponent* c) { scoreView.addSubcomponent( c ); }
    
    void close ()
    {
        //symbolist_handler->main_component = NULL;
        symbolist_handler->executeCloseCallback();
    }
    
private:
        
    UI_EditType      mouse_mode = select_mode;

    SymbolistHandler* symbolist_handler;
    
    PageComponent    scoreView;
    PaletteComponent paletteView ;
    
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};