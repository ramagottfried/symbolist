
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

#include "ScoreData.h"
#include "ScoreComponent.h"
#include "PaletteComponent.h"


/*
 * SymbolistMainComponent is the main controller of the application
 * managing the connevtion between data (score) and visualization/editing.
 * It is also the node and pointyer for interaction with the library
 */

class SymbolistMainComponent : public Component, public KeyListener
{
public:
    
    SymbolistMainComponent();
    ~SymbolistMainComponent();

    inline Score* getScore() { return score; }

    // CONTROLLER METHODS
    static SymbolistMainComponent* createWindow();
    void closeWindow();
    void windowToFront();
    void windowSetName(String name);
    
    
    // set the contents of scoreGUI from s
    void setContentFromScore() ;
    void clearScoreView();
    
    void registerUpdateCallback(symbolistUpdateCallback c);
    void registerCloseCallback(symbolistCloseCallback c);
    void executeUpdateCallback(int arg);
    void executeCloseCallback();
    
    
    // create a Symbol from c and add it to parent Windows's score
    void handleComponentAdded ( BaseComponent* c ) ;
    // removes the Symbol corresponding to c from parent Windows's score
    void handleComponentRemoved ( BaseComponent* c ) ;
    // modified the Symbol corresponding to c from parent Windows's score
    void handleComponentModified ( BaseComponent* c ) ;
    
    
    
    // NORMAL COMPOENENT METHODS
    void paint (Graphics&) override;
    void resized() override;

    
    bool keyPressed (const KeyPress& key, Component* originatingComponent) override;

    
private:
    
    Score *score;
    ScoreComponent scoreGUI;
    
    Component* getWindow(); // will require static cast
    
    static BaseComponent* makeComponentFromSymbol(Symbol* s);
    static void setComponentSymbol(BaseComponent* c);
    
    symbolistUpdateCallback myUpdateCallback = NULL;
    symbolistCloseCallback myCloseCallback = NULL;

    
    //DrawableButton *dbutton = NULL;
    //PaletteComponent palette{this};
    
    PaletteComponent palette; // << this should be dynamically expandable

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolistMainComponent)
};
