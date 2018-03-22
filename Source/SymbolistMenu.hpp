#pragma once

#include "SymbolistComponent.h"

using namespace std;

class SymbolistMenu : public Component, public MenuBarModel, public ChangeBroadcaster
{
public:
    SymbolistMenu();    
    ~SymbolistMenu();
    
    void resized() override;
    
    StringArray getMenuBarNames() override;
    
    PopupMenu getMenuForIndex (int menuIndex, const String& /*menuName*/) override;
    
    void menuItemSelected (int menuItemID, int /*topLevelMenuIndex*/) override;
    
    ApplicationCommandTarget* getNextCommandTarget();
    void getAllCommands (Array<CommandID>& commands);
    void getCommandInfo (CommandID commandID, ApplicationCommandInfo& result);
    bool perform (const juce::ApplicationCommandTarget::InvocationInfo& info);
    
    
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
        cmd_playmsg                 = 0x2010,
        cmd_objToStaff              = 0x2011,
        cmd_attachToStaff           = 0x2012,
        cmd_selectedToFront         = 0x2013,
        cmd_selectedToBack          = 0x2014,
        cmd_toggleCursor            = 0x2015,
        cmd_nudgeLeft               = 0x2016,
        cmd_nudgeRight              = 0x2017,
        cmd_nudgeUp                 = 0x2018,
        cmd_nudgeDown               = 0x2019,
        cmd_undo                    = 0x2020,
        cmd_redo                    = 0x2021


    };
    
private:
    ScopedPointer<MenuBarComponent> menuBar;
    
    
    //==============================================================================
    class CustomMenuComponent   : public PopupMenu::CustomComponent, private Timer
    {
    public:
        CustomMenuComponent()
        {
            // set off a timer to move a blob around on this component every
            // 300 milliseconds - see the timerCallback() method.
            startTimer (300);
        }
        
        void getIdealSize (int& idealWidth, int& idealHeight) override
        {
            // tells the menu how big we'd like to be..
            idealWidth = 200;
            idealHeight = 60;
        }
        
        void paint (Graphics& g) override
        {
            g.fillAll (Colours::yellow.withAlpha (0.3f));
            
            g.setColour (Colours::pink);
            g.fillEllipse (blobPosition);
            
            g.setFont (Font (14.0f, Font::italic));
            g.setColour (Colours::black);
            
            g.drawFittedText ("This is a customised menu item (also demonstrating the Timer class)...",
                              getLocalBounds().reduced (4, 0),
                              Justification::centred, 3);
        }
        
    private:
        void timerCallback() override
        {
            Random random;
            blobPosition.setBounds ((float) random.nextInt (getWidth()),
                                    (float) random.nextInt (getHeight()),
                                    40.0f, 30.0f);
            repaint();
        }
        
        Rectangle<float> blobPosition;
    };
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMenu)
};
