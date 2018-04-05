#pragma once

#ifndef SymbolistMainComponent_h
#define SymbolistMainComponent_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistHandler.h"
#include "PageComponent.h"
#include "PaletteComponent.h"
#include "BaseComponent.h"

#include "PropertyPanelTabs.h"
#include "MouseModeComponent.hpp"
#include "TimeDisplay.hpp"

#include "SymbolistMenu.hpp"
#include "SymbolistLookAndFeel.hpp"
#include "View.hpp"

/**
 * SymbolistMainComponent is the main graphic component of the application.
 *
 */
class SymbolistMainComponent : public virtual SymbolistComponent,
                               public virtual ApplicationCommandTarget,
                               public virtual View<SymbolistModel, SymbolistHandler>
{
    
    // SymbolistHandler*    symbolist_handler = nullptr; // (not allocated here)
    SymbolistLookAndFeel look_and_feel;
    
    /*******************************************************
     *                      UI ELEMENTS                    *
     *******************************************************/
    
    /**
     * A graphic component which surrounds the PageComponent
     * with scrollbars.
     */
    Viewport                         score_viewport;
    
    /**
     * A graphic component representing the main page of the score.
     */
    PageComponent                    scoreView;
    
    /**
     * A graphic component representing the palette.
     */
    PaletteComponent                 paletteView;
    SymbolistMenu                    menu; //<<  application commands are set here
    ScopedPointer<PropertyPanelTabs> inspector;
    MouseModeComponent               mouseModeView;
    TimeDisplayComponent             timeDisplayView;
    
    int palette_w = 50;
    int menu_h; // set internally
    
    float            m_zoom = 1.0f;
    ModifierKeys     current_mods;
    
    UI_EditType      mouse_mode = SELECTION;
    UI_DrawType      draw_mode = FREE_DRAW;
    
public:
    
    /**************************************************
     *        CONSTRUCTORS AND FACTORY FUNCTIONS      *
     **************************************************/
    SymbolistMainComponent();
    ~SymbolistMainComponent();

    /*************************************
     *        GETTERS AND SETTERS        *
     *************************************/
    inline ScopedPointer<PropertyPanelTabs> getInspector()
    {
        return inspector;
    }
    
    /*********************************************
     *        GUI FUNCTIONALITY AND TOOLS        *
     *********************************************/
    void updatePaletteView();
    
    // Redefinition of methods from Juce::Component
    void resized() override;
    void zoom( float delta );
    
    void modifierKeysChanged (const ModifierKeys& modifiers) override;

    void setMouseMode(UI_EditType m);
    UI_EditType getMouseMode();
    void setDrawMode(UI_DrawType m);
    UI_DrawType getDrawMode();
    
    
    // Redefine these from SymbolistComponent
    inline PageComponent* getPageComponent() override
    {
        return dynamic_cast<PageComponent*>(score_viewport.getViewedComponent());
    }
    inline SymbolistHandler* getSymbolistHandler() override {
        return getController();
    }
    inline Viewport* getViewer() { return &score_viewport; }
    Rectangle<float> getViewRect();
    Rectangle<float> getZoomedRect();

    inline ModifierKeys* getCurrentMods(){ return &current_mods; }
    
    /*********************************************
     *        PROPERTIES PANEL (INSPECTOR)       *
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
    
    /***********************************************************************************
     * APPLICATION KEYBOARD COMMAND WRAPPER (ACTUAL COMMANDS ARE SET IN SymbolistMenu) *
     ***********************************************************************************/
    ApplicationCommandTarget* getNextCommandTarget() override;
    void getAllCommands(Array<CommandID>& commands) override;
    void getCommandInfo(CommandID commandID, ApplicationCommandInfo& result) override;
    bool perform(const InvocationInfo& info) override;
    
    /* Overrides the update method inherited from the Observer class. */
    inline void update() override { repaint(); }
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};

#endif
