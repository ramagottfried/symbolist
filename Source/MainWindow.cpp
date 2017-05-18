
#include "types.h"
#include "MainWindow.h"

/************************************************
 * SHARED BY THE LIBRARY AND THE STANDALONE APP
 ************************************************/

SymbolistMainWindow::SymbolistMainWindow ( Score *s ): DocumentWindow ( "symbolist",
                                                                Desktop::getInstance().getDefaultLookAndFeel().findColour(ResizableWindow::backgroundColourId),
                                                                DocumentWindow::allButtons)
{
    score = s;
    comp = new MainComponent( score );
    setUsingNativeTitleBar (true);
    setContentOwned (comp , true );
    centreWithSize (getWidth(), getHeight());
    setVisible (true);
    setResizable(true, true);
}

SymbolistMainWindow::SymbolistMainWindow () : SymbolistMainWindow( new Score () ) {}
SymbolistMainWindow::~SymbolistMainWindow() {}

void SymbolistMainWindow::registerUpdateCallback(symbolistUpdateCallback c) { myUpdateCallback = c; }

void SymbolistMainWindow::updateSymbols( Score *s )
{
    delete score ;
    score = s ;
    comp->clearScore();
    comp->setContentFromScore(score);
}


void SymbolistMainWindow::notifyUpdate ( )
{
    if (myUpdateCallback)
    {
        int scoreSize =  static_cast<int>( score->getSize() );
        cout << "update" << scoreSize << " symbols" << endl;
        odot_bundle **bundle_array = score->exportScoreToOSC();
        cout << "exported" << endl;
        myUpdateCallback( this, scoreSize, bundle_array );
        Score::deleteOdotBundleArray(bundle_array,scoreSize);
        cout << "deleted !" << endl;
    }
}


/***********************************
 * SPECIFIC FOR THE LIBRARY
 ***********************************/

void SymbolistEditorWindow::registerCloseCallback(symbolistCloseCallback c) { myCloseCallback = c; }

void SymbolistEditorWindow::closeButtonPressed()
{
    if (myCloseCallback) { myCloseCallback( this ); }
    delete this;
}

