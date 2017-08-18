
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistComponent.h"
#include "SymbolistHandler.h"
#include "PageComponent.h"
#include "PaletteComponent.h"
#include "BaseComponent.h"

#include "SymbolPropertiesPanel.h"
#include "MouseModeComponent.hpp"
#include "SymbolistMenu.hpp"
#include "SymbolistLookAndFeel.hpp"

/*
 * SymbolistMainComponent is the main controller of the application
 * managing the connection between data (score) and visualization/editing.
 * It is also the node and pointer for interaction with the library
 */

class SymbolistMainComponent : public SymbolistComponent, public ApplicationCommandTarget
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
    
    void modifierKeysChanged (const ModifierKeys& modifiers) override;

    void setMouseMode( UI_EditType m );
    UI_EditType getMouseMode();
    void setDrawMode( UI_DrawType m );
    UI_DrawType getDrawMode();
    
    
    // Redefine these from SymbolistComponent
    inline PageComponent* getPageComponent() override { return dynamic_cast<PageComponent*>( score_viewport.getViewedComponent() ); }
    inline SymbolistHandler* getSymbolistHandler() override { return symbolist_handler; }
    inline Viewport* getViewer() { return &score_viewport; }
    Rectangle<float> getViewRect();
    Rectangle<float> getZoomedRect();

    inline ModifierKeys* getCurrentMods(){ return &current_mods; }
    
    /*********************************************
     * Properties Panel (Inspector)
     *********************************************/
    void toggleInspector();
    inline void clearInspector(){ inspector->clearInspector(); }
    inline void setInspectorObject( BaseComponent *c ){ inspector->setInspectorObject( c ); }


    //==============================================================================
    // The following methods implement the ApplicationCommandTarget interface, allowing
    // this window to publish a set of actions it can perform, and which can be mapped
    // onto menus, keypresses, etc.
    
    
    enum CommandIDs
    {
        cmd_group                   = 0x2100,
        cmd_ungroup                 = 0x2101,
        cmd_deleteSelected          = 0x2000,
        cmd_toggleInspector         = 0x2001,
        cmd_addToPalette            = 0x2002,
        cmd_copy                    = 0x2003,
        cmd_paste                   = 0x2004,
        cmd_flipH                   = 0x2005,
        cmd_flipV                   = 0x2006,
        cmd_zoomIn                  = 0x2007,
        cmd_zoomOut                 = 0x2008,
        cmd_esc                     = 0x2009,
        cmd_playmsg                 = 0x2010 // to do ,
        //cmd_toggleCursor            = 0x2011
    };
    
    ApplicationCommandTarget* getNextCommandTarget() override;
    void getAllCommands (Array<CommandID>& commands) override;
    void getCommandInfo (CommandID commandID, ApplicationCommandInfo& result) override;
    bool perform (const InvocationInfo& info) override;
    
private:
    
    UI_EditType      mouse_mode = selection;
    UI_DrawType      draw_mode = free_draw;

    SymbolistHandler*   symbolist_handler = nullptr; // (not allocated here)
    
    Viewport            score_viewport;
    PageComponent       scoreView;
    
    PaletteComponent    paletteView ;
    SymbolistMenu       menu;

    float               m_zoom = 1.0f;
    
    ModifierKeys        current_mods;

    ScopedPointer<SymbolPropertiesPanel>    inspector;
    MouseModeComponent                      mouseModeView;

    
    SymbolistLookAndFeel    look_and_feel;
    

    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};
