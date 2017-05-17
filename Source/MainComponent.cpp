
#include "MainComponent.h"


MainComponent::MainComponent()
{
    // here there is no preexisiting score
    setSize (600, 400);
    addAndMakeVisible(scoreGUI);
   
//    addAndMakeVisible(palette);
    
    /*
    dbutton = new DrawableButton("Button 2", DrawableButton::ImageFitted);
    
    DrawablePath normal;
    
    {
        Path p;
        p.addStar (Point<float>(), 5, 20.0f, 50.0f, 0.2f);
        normal.setPath (p);
        normal.setFill ( Colours::black );
    }
    
    dbutton->setImages (&normal, &normal, &normal);
    dbutton->setClickingTogglesState (true);
    dbutton->setBounds (0, 0, 20, 20);
    dbutton->setTooltip ("This is an image-only DrawableButton");
    dbutton->addListener ( this );

    addAndMakeVisible(dbutton);
*/
}


MainComponent::MainComponent( Score *s )
{
    // setup score components here

    setSize (600, 400);
    
    // will populate scoreGUI
    setContentFromScore(s);
    
    addAndMakeVisible(scoreGUI);
    
}

MainComponent::~MainComponent()
{
}

void MainComponent::paint (Graphics& g)
{
}

void MainComponent::resized()
{
    scoreGUI.setBounds( 20, 0, getWidth(), getHeight() );
}


BaseComponent* MainComponent::makeComponentFromSymbol(Symbol* s) {

    BaseComponent *c;
    OSCBundle b = s->getOSCBundle();
    
    c = new CircleComponent( 10, 10);
    
    c->setSymbol(s);
    return c;
}

void MainComponent::setContentFromScore ( Score* s ){
    
    for (int i = 0; i < s->getSize(); i++) {
        scoreGUI.addScoreChildComponent( makeComponentFromSymbol(s->getSymbol(i)) );
    }
}



