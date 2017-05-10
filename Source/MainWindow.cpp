


#include "MainWindow.h"
#include "MainComponent.h"


MainWindow::MainWindow(String name) : DocumentWindow (name,
                                            Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                                            DocumentWindow::allButtons)
    {
        setUsingNativeTitleBar (true);
        setContentOwned (new MainContentComponent(), true);
        
        centreWithSize (getWidth(), getHeight());
        setVisible (true);
    }

MainWindow::~MainWindow(){}


void MainWindow::closeButtonPressed()
{
        // This is called when the user tries to close this window. Here, we'll just
        // ask the app to quit when this happens, but you can change this to do
        // whatever you need.
        JUCEApplication::getInstance()->systemRequestedQuit();
}

