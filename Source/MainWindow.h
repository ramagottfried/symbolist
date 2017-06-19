#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "types.h"

#include "SymbolistMainComponent.h"


/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/


class SymbolistMainWindow : public DocumentWindow {
    
public:

    SymbolistMainWindow (SymbolistHandler *sh) ;
    ~SymbolistMainWindow () {}
    
    inline SymbolistMainComponent* getMainComponent() { return &main_component; }
    
    void closeButtonPressed() override;
    
protected:

    SymbolistMainComponent main_component;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainWindow)
};



/***********************************
 * SPECIFIC FOR THE LIBRARY
 ***********************************/
class SymbolistEditorWindow : public SymbolistMainWindow {
    
public:
    
    SymbolistEditorWindow (SymbolistHandler *sh) : SymbolistMainWindow (sh) {}
    ~SymbolistEditorWindow () {}
    
    void closeButtonPressed() override;
   
private:

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistEditorWindow)
};



//==============================================================================
/*
 This class implements the desktop window that contains an instance of
 our MainContentComponent class.
 */
class AppMainWindow    : public SymbolistMainWindow
{
public:
    
    AppMainWindow (SymbolistHandler *sh)  : SymbolistMainWindow (sh) {}
    
    void closeButtonPressed() override;
    
private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (AppMainWindow)
};

