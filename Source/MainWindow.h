#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "types.h"

#include "MainComponent.h"


/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/

class SymbolistMainWindow : public DocumentWindow {
    
public:

    SymbolistMainWindow ();
    ~SymbolistMainWindow () {}
    
    inline SymbolistMainComponent* getSymbolistMainComponent() { return &main_component; }
    
protected:

    SymbolistMainComponent main_component;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainWindow)
};



/***********************************
 * SPECIFIC FOR THE LIBRARY
 ***********************************/
class SymbolistEditorWindow : public SymbolistMainWindow {
    
public:
    
    SymbolistEditorWindow () : SymbolistMainWindow () {}
    ~SymbolistEditorWindow () {}
    
    void closeButtonPressed() override;
   
private:

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistEditorWindow)
};

