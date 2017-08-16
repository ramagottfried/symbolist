#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "SymbolistMainComponent.h"


/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/


class SymbolistMainWindow : public DocumentWindow, private AsyncUpdater
{
    
public:

    SymbolistMainWindow (SymbolistHandler *sh) ;
    ~SymbolistMainWindow ();
    
    SymbolistMainComponent* getMainComponent();
    static ApplicationCommandManager& getApplicationCommandManager();

    void handleAsyncUpdate() override;

    void closeButtonPressed() override;

    
protected:

    ScopedPointer<SymbolistMainComponent> main_component;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainWindow)
};

