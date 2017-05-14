
#include "MainWindow.h"


MainWindow::MainWindow ( Score *s ) : DocumentWindow ( "symbolist",
                                            Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                                            DocumentWindow::allButtons)
{
    setUsingNativeTitleBar (true);
    setContentOwned ( new MainComponent( s ), true );
    
    centreWithSize (getWidth(), getHeight());
    setVisible (true);
    setResizable(true, true);

    score = s;
}

MainWindow::MainWindow () : MainWindow( new Score () ) {}


MainWindow::~MainWindow()
{}



void MainWindow::closeButtonPressed()
{
        // This is called when the user tries to close this window. Here, we'll just
        // ask the app to quit when this happens, but you can change this to do
        // whatever you need.
        //JUCEApplication::getInstance()->systemRequestedQuit();
    symbolistCallbackToHost(-1);
    delete this;
}


