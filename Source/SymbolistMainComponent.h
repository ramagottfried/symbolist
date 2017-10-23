
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistComponent.h"
#include "SymbolistHandler.h"
#include "PageComponent.h"
#include "PaletteComponent.h"
#include "BaseComponent.h"

#include "PropertyPanelTabs.h"
#include "MouseModeComponent.hpp"
#include "TimeDisplay.hpp"

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


    void setTimePoint( float t )
    {
        getPageComponent()->setTimePoint(t);
        timeDisplayView.setTime(t);
    }
    
    void toggleTimeAndCursorDisplay()
    {
        timeDisplayView.toggleView();
        getPageComponent()->toggleCursorDisplay();
    }
    
    /*********************************************
     * Application keyboard command wrapper (actual commands are set in SybolistMenu)
     *********************************************/
    ApplicationCommandTarget* getNextCommandTarget() override;
    void getAllCommands (Array<CommandID>& commands) override;
    void getCommandInfo (CommandID commandID, ApplicationCommandInfo& result) override;
    bool perform (const InvocationInfo& info) override;
    
private:
    SymbolistHandler*       symbolist_handler = nullptr; // (not allocated here)
    SymbolistLookAndFeel    look_and_feel;
    
    /************************************************************************
     * UI elements
     ************************************************************************/
    Viewport                                score_viewport;
    PageComponent                           scoreView;
    PaletteComponent                        paletteView ;
    SymbolistMenu                           menu; //<<  application commands are set here
    ScopedPointer<PropertyPanelTabs>        inspector;
    MouseModeComponent                      mouseModeView;
    
    TimeDisplayComponent                    timeDisplayView;

    int palette_w = 50;
    int menu_h; // set internally
    
    float            m_zoom = 1.0f;
    ModifierKeys     current_mods;

    UI_EditType      mouse_mode = selection;
    UI_DrawType      draw_mode = free_draw;
    

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};
