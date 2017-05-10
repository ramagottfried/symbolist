/*
  ==============================================================================

    This file was auto-generated!

    It contains the basic startup code for a Juce application.

  ==============================================================================
*/

#include "../JuceLibraryCode/JuceHeader.h"

//==============================================================================
/*
 This class implements the desktop window that contains an instance of
 our MainContentComponent class.    */

/* Note: Be careful if you override any DocumentWindow methods - the base
 class uses a lot of them, so by overriding you might break its functionality.
 It's best to do all your work in your content component instead, but if
 you really have to override any DocumentWindow methods, make sure your
 subclass also calls the superclass's method.
 */

class MainWindow : public DocumentWindow {
    
    public:
    
    MainWindow (String name);
    ~MainWindow ();
    
    void closeButtonPressed() override;
    
    private:
        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainWindow)

};

