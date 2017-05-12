
#include "MainComponent.h"


MainComponent::MainComponent()
{
    // here there is no preexisiting score

    setSize (600, 400);
    addAndMakeVisible(scoreGUI);
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
    scoreGUI.setBounds( getLocalBounds() );
}