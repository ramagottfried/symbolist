
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "ScoreComponent.h"
#include "ScoreData.h"
#include "PaletteComponent.h"


class MainComponent   : public Component
{
public:
    //==============================================================================
    MainComponent();
    MainComponent( Score *s );
    
    ~MainComponent();

    void paint (Graphics&) override;
    void resized() override;

    void buttonCallback(MouseEvent *event, int type)
    {
        std::cout << event->eventComponent->getName() << type << std::endl;
    }
    
    
    // set the contents of scoreGUI from s
    void setContentFromScore ( Score* s ) ;
    void clearScore();
    
    // create a Symbol from c and add it to parent Windows's score
    void handleNewComponent ( BaseComponent* c ) ;
    // toDo : removes the Symbol corresponding to c from parent Windows's score
    void handleRemoveComponent ( BaseComponent* c ) {} ;
    
    
private:
    
    ScoreComponent scoreGUI;
    
    static BaseComponent* makeComponentFromSymbol(Symbol* s);
    static Symbol* makeSymbolFromComponent(BaseComponent* s);
    
    //DrawableButton *dbutton = NULL;
    //PaletteComponent palette{this};
    
    OwnedArray<Component> palette; // << this should be dynamically expandable 

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MainComponent)
};
