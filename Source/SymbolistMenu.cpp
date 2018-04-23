
#include "SymbolistMenu.hpp"
#include "SymbolistMainWindow.h"
#include "SymbolistMainComponent.h"

SymbolistMenu::SymbolistMenu()
{
    addAndMakeVisible(menu_bar = new MenuBarComponent(this));
    setApplicationCommandManagerToWatch(&SymbolistMainWindow::getApplicationCommandManager());
}

SymbolistMenu::~SymbolistMenu()
{
    PopupMenu::dismissAllActiveMenus();
}

void SymbolistMenu::resized()
{
    Rectangle<int> area (getLocalBounds());
    menu_bar->setBounds (area.removeFromTop (LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight()));
}

void SymbolistMenu::getAllCommands (Array<CommandID>& commands)
{
    // this returns the set of all commands that this target can perform..
    const CommandID ids[] = {
        cmd_group,
        cmd_ungroup,
        cmd_deleteSelected,
        cmd_toggleInspector,
        cmd_addToPalette,
        cmd_copy,
        cmd_paste,
        cmd_flipH,
        cmd_flipV,
        cmd_zoomIn,
        cmd_zoomOut,
        cmd_esc,
        cmd_playmsg,
        cmd_objToStaff,
        cmd_attachToStaff,
        cmd_selectedToFront,
        cmd_selectedToBack,
        cmd_toggleCursor,
        cmd_nudgeLeft,
        cmd_nudgeRight,
        cmd_nudgeUp,
        cmd_nudgeDown,
        cmd_undo,
        cmd_redo
    };
    
    commands.addArray(ids, numElementsInArray(ids));
}

void SymbolistMenu::getCommandInfo(CommandID commandID, ApplicationCommandInfo& result)
{
    switch (commandID)
    {
        case cmd_group:
            result.setInfo ("group selected", String(), String(), 0);
            result.addDefaultKeypress ('g', ModifierKeys::commandModifier);
            break;
            
        case cmd_ungroup:
            result.setInfo ("ungroup selected", String(), String(), 0);
            result.addDefaultKeypress ('u', ModifierKeys::commandModifier);
            break;
            
        case cmd_deleteSelected:
            result.setInfo ("delete selected", String(), String(), 0);
            result.addDefaultKeypress ( KeyPress::backspaceKey, ModifierKeys::noModifiers );
            break;
            
        case cmd_toggleInspector:
            result.setInfo ("open/close inspector", String(), String(), 0);
            result.addDefaultKeypress ('i', ModifierKeys::noModifiers );
            break;
            
        case cmd_addToPalette:
            result.setInfo ("add selected To palette", String(), String(), 0);
            result.addDefaultKeypress ('w', ModifierKeys::noModifiers );
            break;
            
        case cmd_copy:
            result.setInfo ("copy", String(),String(), 0);
            result.addDefaultKeypress ( 'c', ModifierKeys::commandModifier );
            break;
            
        case cmd_paste:
            result.setInfo ("paste", String(),String(), 0);
            result.addDefaultKeypress ('v', ModifierKeys::commandModifier );
            break;
            
        case cmd_flipH:
            result.setInfo ("flip horizontally",String(),String(), 0);
            result.addDefaultKeypress ('h', ModifierKeys::altModifier );
            break;
            
        case cmd_flipV:
            result.setInfo ("flip vertically", String(),String(), 0);
            result.addDefaultKeypress ('v', ModifierKeys::altModifier );
            break;
            
        case cmd_nudgeLeft:
            result.setInfo ("move selected to the left", String(),String(), 0);
            result.addDefaultKeypress ( KeyPress::leftKey, ModifierKeys::altModifier );
            break;
            
        case cmd_nudgeRight:
            result.setInfo ("move selected to the right", String(),String(), 0);
            result.addDefaultKeypress ( KeyPress::rightKey, ModifierKeys::altModifier );
            break;
            
        case cmd_nudgeUp:
            result.setInfo ("move selected upwards", String(),String(), 0);
            result.addDefaultKeypress ( KeyPress::upKey, ModifierKeys::altModifier );
            break;
            
        case cmd_nudgeDown:
            result.setInfo ("move selected downwards", String(),String(), 0);
            result.addDefaultKeypress ( KeyPress::downKey, ModifierKeys::altModifier );
            break;
            
        case cmd_zoomIn:
            result.setInfo ("zoom in", String(),String(), 0);
            result.addDefaultKeypress ( '=', ModifierKeys::noModifiers );
            break;
            
        case cmd_zoomOut:
            result.setInfo ("zoom out", String(),String(), 0);
            result.addDefaultKeypress ('-',  ModifierKeys::noModifiers );
            break;
            
        case cmd_esc:
            result.setInfo ("deselect", String(), String(), 0);
            result.addDefaultKeypress (KeyPress::escapeKey,  ModifierKeys::noModifiers );
            break;
            
        case cmd_playmsg:
            result.setInfo ("send transport start message", String(), String(), 0);
            result.addDefaultKeypress (KeyPress::spaceKey,  ModifierKeys::noModifiers );
            break;
            
        case cmd_objToStaff:
            result.setInfo ("convert selected to staff", String(), String(), 0);
            result.addDefaultKeypress ('s',  ModifierKeys::noModifiers );
            break;
            
        case cmd_attachToStaff:
            result.setInfo ("attached selected to staff", String(), String(), 0);
            result.addDefaultKeypress ('s',  ModifierKeys::altModifier );
            break;
            
        case cmd_selectedToFront:
            result.setInfo ("selected object to front layer", String(), String(), 0);
            result.addDefaultKeypress (']',  ModifierKeys::altModifier );
            break;
            
        case cmd_selectedToBack:
            result.setInfo ("selected object to back layer", String(), String(), 0);
            result.addDefaultKeypress ('[',  ModifierKeys::altModifier );
            break;
            
        case cmd_toggleCursor:
            result.setInfo ("toggle time cursor view", String(), String(), 0);
            result.addDefaultKeypress ('t',  ModifierKeys::noModifiers );
            break;
            
        case cmd_undo:
            result.setInfo ("undo", String(), String(), 0);
            result.addDefaultKeypress ('z',  ModifierKeys::ctrlModifier );
            break;
            
        case cmd_redo:
            result.setInfo ("redo", String(), String(), 0);
            result.addDefaultKeypress ('z',  ModifierKeys::shiftModifier | ModifierKeys::ctrlModifier );
            break;
            
        default:
            result.setInfo ("undefined", "", "", 0);
            
            break;
    }
    
    
    /*
     // Palette selection not currently in use
     else if (   key == KeyPress('c')) { // would be better to type a number and that selects the nth palete item..
     symbolist_handler->setCurrentSymbol(0);
     paletteView.selectPaletteButton(0);
     }
     
     else if (   key == KeyPress('p') ) {
     symbolist_handler->setCurrentSymbol(1);
     paletteView.selectPaletteButton(1);
     }
     */
}

bool SymbolistMenu::perform(const juce::ApplicationCommandTarget::InvocationInfo& info)
{
    SymbolistMainComponent* main = dynamic_cast<SymbolistMainComponent*>(getParentComponent());
    
    if (main != NULL)
    {
        PageComponent *score = main->getPageComponent();

        if( !score )
            return false;
        
        switch (info.commandID)
        {
            case cmd_group:
                main->groupSelectedSymbols();
                break;
                
            case cmd_ungroup:
                main->ungroupSelectedSymbols();
                break;
                
            case cmd_deleteSelected:
                score->getEditedComponent()->deleteSelectedComponents();
                break;
                
            case cmd_toggleInspector:
                main->toggleInspector();
                break;
                
            case cmd_addToPalette:
                main->addSelectedSymbolsToPalette();
                break;
                
            case cmd_copy:
                main->getSymbolistHandler()->copySelectedToClipBoard();
                break;
                
            case cmd_paste:
                main->getSymbolistHandler()->newFromClipBoard();
                break;
                
            case cmd_flipH:
                score->flipSelectedSymbols(1);
                break;
                
            case cmd_flipV:
                score->flipSelectedSymbols(0);
                break;
                
            case cmd_nudgeLeft:
                score->nudgeSelected(0);
                break;
            
            case cmd_nudgeRight:
                score->nudgeSelected(1);
                break;
            
            case cmd_nudgeUp:
                score->nudgeSelected(2);
                break;
            
            case cmd_nudgeDown:
                score->nudgeSelected(3);
                break;
                
            case cmd_zoomIn:
                main->zoom( 0.1 );
                break;
                
            case cmd_zoomOut:
                main->zoom( -0.1 );
                break;
                
            case cmd_esc:
                score->getEditedComponent()->unselectAllComponents();
                score->exitEditMode();
                score->exitStaffSelMode();
                
                main->getSymbolistHandler()->executeTransportCallback(0); // = stop
                main->getSymbolistHandler()->symbolistAPI_setTime(0);
                main->getSymbolistHandler()->clearInspector();
                
                score->repaint();
                break;
                
            case cmd_playmsg:
                main->getSymbolistHandler()->executeTransportCallback(1);
                break;
                
            case cmd_objToStaff:
                main->getSymbolistHandler()->convertSelectedToStaff();
                break;
                
            case cmd_attachToStaff:
                score->enterStaffSelMode();
                score->repaint();
                break;
                
            case cmd_selectedToFront:
                score->selectedToFront();
                break;
                
            case cmd_selectedToBack:
                score->selectedToBack();
                break;
                
            case cmd_toggleCursor:
                main->getSymbolistHandler()->symbolistAPI_toggleTimeCusor();
                break;
                
            case cmd_undo:
                main->getSymbolistHandler()->undo();
                break;
            
            case cmd_redo:
                main->getSymbolistHandler()->redo();
                break;
                
            default:
                return false;
        }
    }
    
    return true;
}


//==============================================================================
StringArray SymbolistMenu::getMenuBarNames()
{
    const char* const names[] = { "file", "edit", "view", "io", nullptr };
    
    return StringArray (names);
}

PopupMenu SymbolistMenu::getMenuForIndex (int menuIndex, const String& /*menuName*/)
{
    ApplicationCommandManager* commandManager = &SymbolistMainWindow::getApplicationCommandManager();
    
    PopupMenu menu;
    // cout << "SymbolistMenu::getMenuForIndex " << menuIndex << " " << commandManager << endl;

    if (menuIndex == 0)
    {
        //menu.addSeparator();
        menu.addCommandItem (commandManager, StandardApplicationCommandIDs::quit);
    }
    else if (menuIndex == 1)
    {
        menu.addCommandItem (commandManager, cmd_undo);
        menu.addCommandItem (commandManager, cmd_redo);

        menu.addCommandItem (commandManager, cmd_esc);
        
        menu.addSeparator();

        menu.addCommandItem (commandManager, cmd_copy);
        menu.addCommandItem (commandManager, cmd_paste);
        menu.addCommandItem (commandManager, cmd_deleteSelected);
       
        menu.addSeparator();
        
        menu.addCommandItem (commandManager, cmd_flipH);
        menu.addCommandItem (commandManager, cmd_flipV);
        menu.addCommandItem (commandManager, cmd_nudgeLeft);
        menu.addCommandItem (commandManager, cmd_nudgeRight);
        menu.addCommandItem (commandManager, cmd_nudgeUp);
        menu.addCommandItem (commandManager, cmd_nudgeDown);
        
        menu.addSeparator();

        menu.addCommandItem (commandManager, cmd_group);
        menu.addCommandItem (commandManager, cmd_ungroup);

        menu.addSeparator();
        
        menu.addCommandItem (commandManager, cmd_objToStaff);
        menu.addCommandItem (commandManager, cmd_attachToStaff);
        
        menu.addSeparator();
        
        menu.addCommandItem (commandManager, cmd_selectedToFront);
        menu.addCommandItem (commandManager, cmd_selectedToBack);
        
        menu.addSeparator();
        
        menu.addCommandItem (commandManager, cmd_addToPalette);
        
    }
    else if (menuIndex == 2)
    {
        menu.addCommandItem (commandManager, cmd_zoomIn);
        menu.addCommandItem (commandManager, cmd_zoomOut);
        menu.addSeparator();
        menu.addCommandItem (commandManager, cmd_toggleInspector);
        menu.addCommandItem (commandManager, cmd_toggleCursor);

    }
    else if (menuIndex == 3)
    {
        menu.addCommandItem (commandManager, cmd_playmsg);
    }

    return menu;
}

void SymbolistMenu::menuItemSelected(int menuItemID, int /*topLevelMenuIndex*/)
{
    cout << "SymbolistMenu::menuItemSelected " << menuItemID << endl;
    
    // most of our menu items are invoked automatically as commands, but we can handle the
    // other special cases here..
/*
    if (menuItemID == 6000)
    {
#if JUCE_MAC
        if (MenuBarModel::getMacMainMenu() != nullptr)
        {
            MenuBarModel::setMacMainMenu (nullptr);
            menuBar->setModel (this);
        }
        else
        {
            menuBar->setModel (nullptr);
            MenuBarModel::setMacMainMenu (this);
        }
#endif
    }
    else if (menuItemID >= 3000 && menuItemID <= 3003)
    {
        if (TabbedComponent* tabs = findParentComponentOfClass<TabbedComponent>())
        {
            TabbedButtonBar::Orientation o = TabbedButtonBar::TabsAtTop;
            
            if (menuItemID == 3001) o = TabbedButtonBar::TabsAtBottom;
            if (menuItemID == 3002) o = TabbedButtonBar::TabsAtLeft;
            if (menuItemID == 3003) o = TabbedButtonBar::TabsAtRight;
            
            tabs->setOrientation (o);
        }
    }
    else if (menuItemID >= 12298 && menuItemID <= 12305)
    {
        sendChangeMessage();
    }
 */
}
