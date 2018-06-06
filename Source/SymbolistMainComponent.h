#pragma once

#ifndef SymbolistMainComponent_h
#define SymbolistMainComponent_h

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistHandler.h"
#include "PageComponent.h"
#include "PaletteComponent.h"
#include "BaseComponent.h"

#include "InspectorComponent.h"
#include "MouseModeComponent.hpp"
#include "TimeDisplayComponent.hpp"

#include "SymbolistMenu.hpp"
#include "SymbolistLookAndFeel.hpp"
#include "View.hpp"

/**
 * SymbolistMainComponent is the main graphic component of the application.
 *
 */
class SymbolistMainComponent : public virtual SymbolistComponent,
                               public virtual ApplicationCommandTarget,
                               public virtual View<SymbolistModel, SymbolistHandler> {
	
public:
    
    /**************************************************
     *        CONSTRUCTORS AND FACTORY FUNCTIONS      *
     **************************************************/
    SymbolistMainComponent(SymbolistHandler* mainController);
    ~SymbolistMainComponent();

    /*************************************
     *        GETTERS AND SETTERS        *
     *************************************/
    inline PaletteComponent*     getPaletteView() { return &palette_view; }
    inline PageComponent*        getScoreView() { return &score_view; }
	inline MouseModeComponent*   getMouseModeView() { return &mouse_mode_view; }
	inline InspectorComponent*   getInspectorView() { return inspector; }
	inline TimeDisplayComponent* getTimeDisplayView() { return &time_display_view; }
	
	void setMouseMode(UI_EditType m);
    UI_EditType getMouseMode();
    void setDrawMode(UI_DrawType m);
    UI_DrawType getDrawMode();
    
    // Redefines these from SymbolistComponent
    inline PageComponent*    getPageComponent() override { return &score_view; }
    inline SymbolistHandler* getSymbolistHandler() override { return getController(); }
    
    inline Viewport*     getViewer() { return &score_viewport; }
	inline ModifierKeys* getCurrentMods(){ return &current_mods; }

    /**************************************
     *        PALETTE VIEW METHODS        *
     **************************************/
    void updatePaletteView();
    void addSelectedSymbolsToPalette();
    
	/**************************************
     *        SCORE VIEW METHODS          *
     **************************************/
     
     /**
      * Calls groupSelectedSymbols on the currently edited component in
      * the score view.
      * The currently edited component could be the score view itself or
      * a lower level graphic components (for instance if one is grouping
      * sysmbols within a symbol group).
      */
    void groupSelectedSymbols();
    
     /**
      * Calls ungroupSelectedSymbols on the currently edited component in
      * the score view.
      * The currently edited component could be the score view itself or
      * a lower level graphic components (for instance if one is ungrouping
      * sysmbols within a symbol group).
      */
    void ungroupSelectedSymbols();
    
    /**
      * Calls deleteSelectedSymbols on the currently edited component in
      * the score view.
      * The currently edited component could be the score view itself or
      * a lower level graphic component (for instance if one is deleting
      * symbols within a symbol group).
      */
    void deleteSelectedComponents();
    
    /**
     * Wrapper method around the PageComponent::copySelectedToClipBoard
     * method.
     *
     * @see PageComponent#copySelectedToClipBoard()
     *		PageComponent::copySelectedToClipBoard
     */
    void copySelectedToClipBoard();
    
    /**
     * Wrapper method around the PageComponent::newFromClipBoard
     * method.
     *
     * @see PageComponent#newFromClipBoard()
     *		PageComponent::newFromClipBoard
     */
    void newFromClipBoard();
    
    /**
     * Wrapper method around the ScoreComponent::flipSelectedSymbols
     * method.
     * The 1 is passed as an argument, indicating a horizontal flip.
     *
     * @see ScoreComponent#flipSelectedSymbols(int)
     *		ScoreComponent::flipSelectedSymbols
     */
	void flipSelectedSymbolsHorizontally();
	
	/**
     * Wrapper method around the ScoreComponent::flipSelectedSymbols
     * method.
     * The 0 is passed as an argument, indicating a vertical flip.
     *
     * @see ScoreComponent#flipSelectedSymbols(int)
     *		ScoreComponent::flipSelectedSymbols
     */
	void flipSelectedSymbolsVertically();
	
	/**
     * Wrapper method around the ScoreComponent::nudgeSelected
     * method.
     * The 0 is passed as an argument, indicating a nudge left.
     *
     * @see ScoreComponent#nudgeSelected(int)
     *		ScoreComponent::nudgeSelected
     */
	void nudgeSelectedLeft();
	
	/**
     * Wrapper method around the ScoreComponent::nudgeSelected
     * method.
     * The 1 is passed as an argument, indicating a nudge right.
     *
     * @see ScoreComponent#nudgeSelected(int)
     *		ScoreComponent::nudgeSelected
     */
	void nudgeSelectedRight();
	
	/**
     * Wrapper method around the ScoreComponent::nudgeSelected
     * method.
     * The 2 is passed as an argument, indicating a nudge up.
     *
     * @see ScoreComponent#nudgeSelected(int)
     *		ScoreComponent::nudgeSelected
     */
	void nudgeSelectedUp();
	
	/**
     * Wrapper method around the ScoreComponent::nudgeSelected
     * method.
     * The 3 is passed as an argument, indicating a nudge down.
     *
     * @see ScoreComponent#nudgeSelected(int)
     *		ScoreComponent::nudgeSelected
     */
	void nudgeSelectedDown();
	
	/**
     * Wrapper method around the ScoreComponent::selectedToFront
     * method.
     *
     * @see ScoreComponent#selectedToFront()
     *		ScoreComponent::selectedToFront
     */
	inline void selectedToFront() { score_view.selectedToFront(); };
	
	/**
     * Wrapper method around the ScoreComponent::selectedToBack
     * method.
     *
     * @see ScoreComponent#selectedToBack()
     *		ScoreComponent::selectedToBack
     */
	inline void selectedToBack() { score_view.selectedToBack(); };
	
	/**
     * Wrapper method around the PageComponent::convertSelectedToStaff
     * method.
     *
     * @see PageComponent#convertSelectedToStaff()
     *		PageComponent::convertSelectedToStaff
     */
	inline void convertSelectedToStaff() { score_view.createStaffFromSelected(); }
	
	/**
     * Launches the staff selection mode in the score view.
     *
     * @see PageComponent#enterStaffSelMode()
     *		PageComponent::enterStaffSelMode
     */
	void attachSelectedToStaff();
	
	/**
	 * Exits the current selection mode in the score view and
	 * resets the time cursor and the inspector views.
	 */
	void escapeScoreViewMode();
	
	Rectangle<float> getViewRect();
    Rectangle<float> getZoomedRect();
    
	/***************************************
     *        INSPECTOR VIEW METHODS       *
     ***************************************/
    void toggleInspector();
    inline void clearInspector(){ inspector->clearInspector(); }
    inline void setInspectorObject( BaseComponent *c ){ inspector->setInspectorObject( c ); }
    
    // Redefinition of methods from Juce::Component
    void resized() override;
    void zoom( float delta );
    
    void modifierKeysChanged (const ModifierKeys& modifiers) override;
    
    void setTimePoint( float t )
    {
        score_view.setTimePoint(t);
        time_display_view.setTime(t);
    }
    
    void toggleTimeAndCursorDisplay()
    {
        time_display_view.toggleView();
        score_view.toggleCursorDisplay();
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

private:
	
    SymbolistLookAndFeel look_and_feel;
                                   
    /****************************************************
     *                   CHILD VIEWS                    *
     ****************************************************/
	
    /**
     * A graphic component which surrounds the PageComponent
     * with scrollbars.
     */
    Viewport                         score_viewport;
	
    /**
     * A view representing the score.
     */
    PageComponent                    score_view;
	
    /**
     * A view representing the palette.
     */
    PaletteComponent                 palette_view;
	
    /**
     * A view representing the application's menu.
     * All commands to interact with the symbolist application
     * are referenced here.
     */
    SymbolistMenu                    menu;
    
    /**
     * A graphic component describing the OSC bundle
     * associated with the currently selected component.
     *
     * The inspector can be toggle at the right side of
     * the application window by hitting the 'i' key.
     */
    ScopedPointer<InspectorComponent> inspector;
    
    /**
     * A view where all messages related to the current
     * mouse modes are displayed.
     * Messages in the mouse mode view are displayed
     * in the footer of the application window.
     */
    MouseModeComponent               mouse_mode_view;
    
    /**
     * A view displaying the position of the time cursor.
     * The message is displayed in the upper left corner
     * of the application window.
     */
    TimeDisplayComponent             time_display_view;
	
	/***************************************************
     *             MISC GRAPHIC PROPERTIES             *
     ***************************************************/
     
    int palette_w = 50;
    int menu_h; // set internally
	
    float            m_zoom = 1.0f;
    ModifierKeys     current_mods;
	
    UI_EditType      mouse_mode = SELECTION;
    UI_DrawType      draw_mode = FREE_DRAW;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};

#endif
