
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistComponent.h"
#include "SymbolistHandler.h"
#include "PageComponent.h"
#include "PaletteComponent.h"
#include "BaseComponent.h"

#include "SymbolPropertiesPanel.h"

#include "SymbolistLookAndFeel.hpp"

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
    
    static ScopedPointer<SymbolistMainComponent> createMainComponent(SymbolistHandler *sh);
    
    /*********************************************
     * GUI FUNCTIONALITY AND TOOLS
     *********************************************/
    void updatePaletteView();
    
    // Redefinition of methods from Juce::Component
    void resized() override;
    void zoom( float delta );
    
    bool keyPressed (const KeyPress& key, Component* originatingComponent) override;
    void modifierKeysChanged (const ModifierKeys& modifiers) override;

    void setMouseMode( UI_EditType m );
    UI_EditType getMouseMode();
    void setDrawMode( UI_DrawType m );
    UI_DrawType getDrawMode();
    
    
    // Redefine these from SymbolistComponent
    inline PageComponent* getPageComponent() override { return (PageComponent*)score_viewport.getViewedComponent(); }
    inline SymbolistHandler* getSymbolistHandler() override { return symbolist_handler; }
    inline Viewport* getViewer() { return &score_viewport; }
    inline ModifierKeys* getCurrentMods(){ return &current_mods; }
    
    /*********************************************
     * Properties Panel (Inspector)
     *********************************************/
    void toggleInspector();
    inline void clearInspector(){ inspector->clearInspector(); }
    inline void setInspectorObject( BaseComponent *c ){ inspector->setInspectorObject( c ); }

private:
    
    UI_EditType      mouse_mode = selection;
    UI_DrawType      draw_mode = free_draw;

    SymbolistHandler*   symbolist_handler = nullptr; // (not allocated here)
    
    Viewport            score_viewport;
    PageComponent       scoreView;
    
    PaletteComponent    paletteView ;
    
    float               m_zoom = 1.0f;
    
    ModifierKeys        current_mods;

    ScopedPointer<SymbolPropertiesPanel>    inspector;

    SymbolistLookAndFeel    look_and_feel;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};
