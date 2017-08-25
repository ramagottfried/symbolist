
#include "SymbolistMenu.hpp"
#include "SymbolistMainWindow.h"

SymbolistMenu::SymbolistMenu()
{
    addAndMakeVisible (menuBar = new MenuBarComponent (this));
    setApplicationCommandManagerToWatch (&SymbolistMainWindow::getApplicationCommandManager());
}

SymbolistMenu::~SymbolistMenu()
{
    PopupMenu::dismissAllActiveMenus();
}

void SymbolistMenu::resized()
{
    Rectangle<int> area (getLocalBounds());
    menuBar->setBounds (area.removeFromTop (LookAndFeel::getDefaultLookAndFeel().getDefaultMenuBarHeight()));
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
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_esc);
        
        menu.addSeparator();

        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_copy);
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_paste);
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_deleteSelected);
        
        menu.addSeparator();

        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_group);
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_ungroup);

        menu.addSeparator();
        
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_objToStaff);
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_attachToStaff);
        
        menu.addSeparator();
        
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_selectedToFront);
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_selectedToBack);
        
        menu.addSeparator();
        
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_addToPalette);
        
    }
    else if (menuIndex == 2)
    {
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_zoomIn);
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_zoomOut);
        menu.addSeparator();
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_toggleInspector);

    }
    else if (menuIndex == 3)
    {
        menu.addCommandItem (commandManager, SymbolistMainComponent::cmd_playmsg);
    }

    return menu;
}

void SymbolistMenu::menuItemSelected (int menuItemID, int /*topLevelMenuIndex*/)
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
