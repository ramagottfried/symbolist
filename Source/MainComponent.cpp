
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
    g.fillAll ( Colours::white );
}

void MainComponent::resized()
{
    scoreGUI.setBounds( 50, 50, getWidth()-100, getHeight()-100 );
}



BaseComponent* MainComponent::makeComponentFromSymbol(Symbol* s) {

    //BaseComponent *c;
    float x = 0.0;
    float y = 0.0;
    float size = 10.0;

    int xMessagePos = s->getOSCMessagePos("/x");
    int yMessagePos = s->getOSCMessagePos("/y");
    int sizeMessagePos = s->getOSCMessagePos("/size");
    int typeMessagePos = s->getOSCMessagePos("/type");

    if (xMessagePos != -1) x = s->getOSCMessageValue(xMessagePos).getFloat32();
    if (yMessagePos != -1) y = s->getOSCMessageValue(yMessagePos).getFloat32();
    if (sizeMessagePos != -1) size = s->getOSCMessageValue(sizeMessagePos).getFloat32();

    
    if ( typeMessagePos == -1 ) {
    
        return NULL;
    
    } else {
        
        String typeStr = s->getOSCMessageValue(typeMessagePos).getString();
        
        if (typeStr.equalsIgnoreCase(String("circle"))) {
        
            CircleComponent *c = new CircleComponent( x, y, size);
            c->setSymbol(s);
            return c;
    
        } else {
            
            return NULL;
            
        }
    }
}


void MainComponent::setContentFromScore ( Score* s ){
    
    for (int i = 0; i < s->getSize(); i++)
    {
        BaseComponent *c = makeComponentFromSymbol(s->getSymbol(i));
        if ( c != NULL ) scoreGUI.addScoreChildComponent(c);
    }
}



