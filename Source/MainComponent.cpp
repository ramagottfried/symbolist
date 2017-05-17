
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
