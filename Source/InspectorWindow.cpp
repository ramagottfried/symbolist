
#include "InspectorWindow.h"


InspectorWindow::InspectorWindow ( SymbolistHandler *sh ) : DocumentWindow ( "inspector",
                  Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                  DocumentWindow::allButtons)
{
    
    setUsingNativeTitleBar (true);
    inspector = new SymbolPropertiesPanel(sh);
    setContentOwned ( inspector , true );
    setBounds(800, 100, inspector->getWidth(), inspector->getHeight() );
    setVisible (true);
    setResizable(true, true);
}

InspectorWindow::~InspectorWindow()
{
    inspector = nullptr;
}

void InspectorWindow::closeButtonPressed()
{
    inspector->getSymbolistHandler()->symbolistAPI_closeInspectorWindow();
}

SymbolPropertiesPanel* InspectorWindow::getMainComponent()
{
    return inspector;
}

void InspectorWindow::changeListenerCallback (ChangeBroadcaster* source)
{
    cout << "callback thanks" << endl;
}
