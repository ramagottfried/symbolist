#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "types.h"

#include "MainComponent.h"
#include "ScoreData.h"




class MainWindow : public DocumentWindow {
    
public:

    MainWindow ( Score *s );
    MainWindow ();

    ~MainWindow ();

    void closeButtonPressed() override;
    void registerCallback(symbolistCallback c);


private:
    Score *score;
    symbolistCallback myCallback;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainWindow)

};

