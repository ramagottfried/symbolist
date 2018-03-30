#pragma once

#ifndef SymbolistMainWindow_h
#define SymbolistMainWindow_h

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistMainComponent.h"

/**
 * Describes the main graphic window of the symbolist application.
 * All the graphic components of the symbolist app are contained
 * in this SymbolistMainWindow class.
 *
 * SHARED BY THE LIBRARY AND THE STANDALONE APP.
 */
class SymbolistMainWindow : public DocumentWindow, private AsyncUpdater
{
protected:
    
    SymbolistMainComponent* main_component;
    
public:

    SymbolistMainWindow() ;
    ~SymbolistMainWindow();
    
    SymbolistMainComponent* getMainComponent();
    static ApplicationCommandManager& getApplicationCommandManager();

    void handleAsyncUpdate() override;

    void closeButtonPressed() override;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainWindow)
};

#endif
