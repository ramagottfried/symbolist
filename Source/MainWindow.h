#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "MainComponent.h"
#include "ScoreData.h"

class MainWindow : public DocumentWindow {
    
public:

    MainWindow ( Score *s );
    MainWindow ();

    ~MainWindow ();

    void closeButtonPressed() override;

private:
    Score *score;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainWindow)

};

