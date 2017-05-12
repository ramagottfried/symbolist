#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "MainComponent.h"
#include "ScoreData.h"

class MainWindow : public DocumentWindow {
    
    public:
    
    MainWindow ( String name, Score& s );
    ~MainWindow ();
    
    void closeButtonPressed() override;
    
    private:
        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainWindow)

};

