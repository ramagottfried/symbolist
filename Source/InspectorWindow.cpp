
#include "InspectorWindow.h"


InspectorWindow::InspectorWindow ( SymbolistHandler *sh ) : DocumentWindow ( "inspector",
                  Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                  DocumentWindow::allButtons), inspector(sh)
{
    
    setUsingNativeTitleBar (true);
    setContentOwned (&inspector , true );
//    centreWithSize (100, 100 );
    setBounds(800, 100, 100, 100);
    setVisible (true);
    setResizable(true, true);
}

InspectorWindow::~InspectorWindow() {}

void InspectorWindow::closeButtonPressed()
{
    inspector.getSymbolistHandler()->symbolistAPI_closeInspectorWindow();
}

OSCInspectorTable* InspectorWindow::getMainComponent()
{
    return &inspector;
}

void InspectorWindow::changeListenerCallback (ChangeBroadcaster* source)
{
    cout << "callback thanks" << endl;
}
