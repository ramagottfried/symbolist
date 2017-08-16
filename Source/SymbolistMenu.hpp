
#pragma once

#include "SymbolistComponent.h"

class SymbolistMenu : public Component, public MenuBarModel, public ChangeBroadcaster
{
public:
    SymbolistMenu();    
    ~SymbolistMenu();
    
    void resized() override;
    
    StringArray getMenuBarNames() override;
    
    PopupMenu getMenuForIndex (int menuIndex, const String& /*menuName*/) override;
    
    void menuItemSelected (int menuItemID, int /*topLevelMenuIndex*/) override;
    
    
private:
    ScopedPointer<MenuBarComponent> menuBar;
    
    
    //==============================================================================
    class CustomMenuComponent   : public PopupMenu::CustomComponent,
    private Timer
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
