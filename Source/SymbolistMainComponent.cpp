#include "SymbolistMainComponent.h"
#include "SymbolistMainWindow.h"

SymbolistMainComponent::SymbolistMainComponent(SymbolistHandler* mainController)
{
    DEBUG_FULL("Instance address: " << this << endl)
    setComponentID("MainComponent");
	
    /* Sets the corresponding controllers for this
     * instance of SymbolistMainComponent and all
     * its child views.
     */
    setController(mainController);
    palette_view.setController(getController()->getPaletteController());
    score_view.setController(getController()->getPageController());
	mouse_mode_view.setController(getController()->getMouseModeController());
	time_display_view.setController(getController()->getTimeDisplayController());
	inspector_view.setController(getController()->getInspectorController());
	
    /*
     * Sets model for this SymbolistMainComponent instance
     * and all its child components.
     */
    setModel(getController()->getModel());
    palette_view.setModel(getModel());
    score_view.setModel(getModel());
    mouse_mode_view.setModel(getModel());
	time_display_view.setModel(getModel());
    inspector_view.setModel(getModel());
	
    /* Adds this SymbolistMainComponent instance and
     * its child components as observers of the model.
     */
    getModel()->attach(this);
    getModel()->attach(&palette_view);
    getModel()->attach(&score_view);
    getModel()->attach(&mouse_mode_view);
    getModel()->attach(&time_display_view);
    getModel()->attach(&inspector_view);
    
    // Sets UI look and creates the palette buttons.
    setLookAndFeel(&look_and_feel);
    updatePaletteView();
    
    // Sets scoreView properties and makes it visible.
    score_viewport.setViewedComponent(&score_view, false);
    score_viewport.setFocusContainer (true);
    score_viewport.setScrollBarsShown(true, true);
    
    score_view.setSize(6000, 2000);
    addAndMakeVisible(score_viewport);
    
    /* Highlights the default selected
     * button on the palette.
     */
    palette_view.selectPaletteButton(-1);
    
    // Makes all the other views visible.
    addAndMakeVisible(palette_view);
    addAndMakeVisible(mouse_mode_view);
    addChildComponent(time_display_view);
    addAndMakeVisible(menu);
    menu_h = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
	
    setSize(600, 400);
}

SymbolistMainComponent::~SymbolistMainComponent()
{
    assert(getController() != NULL);
	setLookAndFeel(nullptr);
	
    palette_view.deleteAllChildren();
    inspector_view.removeAllChildren();
	
    /* Removes SymbolistMainComponent and its child views
     * from the SymbolistModel's observers list.
     */
    getModel()->detach(&inspector_view);
    getModel()->detach(&time_display_view);
    getModel()->detach(&mouse_mode_view);
	getModel()->detach(&palette_view);
    getModel()->detach(&score_view);
    getModel()->detach(this);
	
}

/***********************************
 *       PALETTE VIEW METHODS      *
 ***********************************/
void SymbolistMainComponent::updatePaletteView()
{
    palette_view.buildFromPalette();
}

void SymbolistMainComponent::addSelectedSymbolsToPalette()
{
    palette_view.addSymbolsToPalette(score_view.getSelectedItems());
    updatePaletteView();
}

/***********************************
 *        SCORE VIEW METHODS       *
 ***********************************/
void SymbolistMainComponent::groupSelectedSymbols()
{
	score_view.getEditedComponent()->groupSelectedSymbols();
}

void SymbolistMainComponent::ungroupSelectedSymbols()
{
	score_view.getEditedComponent()->ungroupSelectedSymbols();
}

void SymbolistMainComponent::deleteSelectedComponents()
{
	score_view.getEditedComponent()->deleteSelectedComponents();
}

void SymbolistMainComponent::copySelectedToClipBoard()
{
	score_view.copySelectedToClipBoard();
}

void SymbolistMainComponent::newFromClipBoard()
{
	score_view.newFromClipBoard();
}

void SymbolistMainComponent::flipSelectedSymbolsHorizontally()
{
	score_view.flipSelectedSymbols(1);
}

void SymbolistMainComponent::flipSelectedSymbolsVertically()
{
	score_view.flipSelectedSymbols(0);
}

void SymbolistMainComponent::nudgeSelectedLeft()
{
	score_view.nudgeSelected(0);
}

void SymbolistMainComponent::nudgeSelectedRight()
{
	score_view.nudgeSelected(1);
}

void SymbolistMainComponent::nudgeSelectedUp()
{
	score_view.nudgeSelected(2);
}

void SymbolistMainComponent::nudgeSelectedDown()
{
	score_view.nudgeSelected(3);
}

void SymbolistMainComponent::attachSelectedToStaff()
{
	score_view.enterStaffSelMode();
	score_view.repaint();
}

void SymbolistMainComponent::escapeScoreViewMode()
{
	// Exits current selection mode.
	score_view.getEditedComponent()->unselectAllComponents();
	score_view.exitEditMode();
	score_view.exitStaffSelMode();

	// Stops transport view and resets time position to zero.
	getController()->executeTransportCallback(0);
	getController()->symbolistAPI_setTime(0);
	
	// Clears inspector view.
	getController()->clearInspector();

	score_view.repaint();
}

/***************************************
 *        INSPECTOR VIEW METHODS       *
 ***************************************/

void SymbolistMainComponent::toggleInspector()
{
    if( !inspector_view.isVisible() )
    {
        auto selectedItems = score_view.getSelectedItems();
        inspector_view.setInspectorObject( dynamic_cast<BaseComponent*>(selectedItems.getLast()) );
		
		auto menuH = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
		
		/* If the height of the inspector view is superior to the main window's height
		 * then increase the window's height.
		 */
		if (inspector_view.getSymbolPanelTab() != nullptr && inspector_view.getPreferedHeight() > getHeight())
				setSize(getWidth(), inspector_view.getPreferedHeight() + menu_h);
		
		auto area = getLocalBounds();
		area.removeFromTop(menuH);
		area.removeFromRight(score_viewport.getScrollBarThickness());
		inspector_view.setBounds(area.removeFromRight(getWidth() * 0.33));
        addAndMakeVisible( inspector_view );
		
//        inspector_view.setSize(getWidth() * 0.33, getHeight() - score_viewport.getScrollBarThickness() - menuH);
//        inspector_view.setTopRightPosition( getWidth() - score_viewport.getScrollBarThickness(), menuH );
        inspector_view.resized();
    }
    else
    {
        removeChildComponent(&inspector_view);
        inspector_view.setVisible(false);
    }
}

void SymbolistMainComponent::resized()
{
    auto w = getWidth();
    auto h = getHeight();
	
    menu_h = LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight();
	
	if ( inspector_view.isVisible() )
    {
        inspector_view.setSize(w * 0.33, h - score_viewport.getScrollBarThickness() - menu_h);
        inspector_view.setTopRightPosition( w - score_viewport.getScrollBarThickness(), menu_h );
    }
	
    palette_view.setBounds( 0, 0, palette_w, h );
    score_viewport.setBounds( palette_w, menu_h, w-palette_w, h );
    mouse_mode_view.setBounds( palette_w, h-25, w-palette_w, 25 );
    menu.setBounds(palette_w, 0, w-palette_w, menu_h );
    time_display_view.setBounds(palette_w, menu_h+2, 12, 13);
	
}

void SymbolistMainComponent::zoom( float delta )
{
    m_zoom += delta;
    score_view.setTransform( AffineTransform().scale( m_zoom ) );
    repaint();
}

Rectangle<float> SymbolistMainComponent::getZoomedRect()
{
    return score_viewport.getViewArea().toFloat() / m_zoom;
}

Rectangle<float> SymbolistMainComponent::getViewRect()
{
    return score_viewport.getViewArea().toFloat();
}

/***************************
 * edit/drax modes
 ***************************/

void SymbolistMainComponent::setMouseMode( UI_EditType m )
{
    mouse_mode = m;
    mouse_mode_view.setMouseMode( m );
}

UI_EditType SymbolistMainComponent::getMouseMode()
{
    return mouse_mode;
}

void SymbolistMainComponent::setDrawMode(UI_DrawType m)
{
    draw_mode = m;
    mouse_mode_view.setDrawMode(m);
}

UI_DrawType SymbolistMainComponent::getDrawMode()
{
    return draw_mode ;
}

void SymbolistMainComponent::modifierKeysChanged (const ModifierKeys& modifiers)
{
    if (modifiers.isCommandDown())
    {
        setMouseMode(UI_EditType::DRAW);
    }
    else
    {
        setMouseMode(UI_EditType::SELECTION);
    }
    
    current_mods = modifiers;
}

/***********************************************************************************
 * key actions (now handled by ApplicationCommand Target to synchronize with menu) *
 ***********************************************************************************/
ApplicationCommandTarget* SymbolistMainComponent::getNextCommandTarget()
{
    return findFirstTargetParentComponent();
}

void SymbolistMainComponent::getAllCommands(Array<CommandID>& commands)
{
    menu.getAllCommands(commands);
}

void SymbolistMainComponent::getCommandInfo(CommandID commandID, ApplicationCommandInfo& result)
{
    menu.getCommandInfo(commandID, result);
}

bool SymbolistMainComponent::perform (const InvocationInfo& info)
{
    return menu.perform(info);
}





