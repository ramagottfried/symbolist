#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "InspectorTable.h"

/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/

using namespace std;

class InspectorWindow : public DocumentWindow, private ChangeListener
{
public:
    InspectorWindow ( SymbolistHandler *sh );
    ~InspectorWindow();
    void closeButtonPressed();
    OSCInspectorTable* getMainComponent();
    
    void resized() override
    {
        repaint();
    }
    
private:
    
    void changeListenerCallback (ChangeBroadcaster* source);
    
    OSCInspectorTable  inspector;
    
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (InspectorWindow)
};
