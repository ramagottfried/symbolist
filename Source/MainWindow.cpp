
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


MainWindow::~MainWindow() {}


void MainWindow::registerCallback(symbolistCallback c) {

    myCallback = c;

}


void MainWindow::closeButtonPressed() {

    if (myCallback) { myCallback( this, NULL ); }
    delete this;

}


