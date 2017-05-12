#include "PaletteComponent.h"



PaletteComponent::PaletteComponent()
{
    
    DrawablePath normal;
    
    {
        Path p;
        p.addStar (Point<float>(), 5, 20.0f, 50.0f, 0.2f);
        normal.setPath (p);
        normal.setFill ( Colours::black );
    }
    
    {
        // create an image-only button from these drawables..
        DrawableButton* db = palette_add ( new DrawableButton ("Button 2", DrawableButton::ImageFitted) );
        
        db->setImages (&normal, &normal, &normal);
        db->setClickingTogglesState (true);
        db->setBounds (0, 0, 20, 10);
        db->setTooltip ("This is an image-only DrawableButton");
//        db->addListener ( getParentComponent() );
    }
    
}

PaletteComponent::~PaletteComponent(){}

template <typename ComponentType>
ComponentType *PaletteComponent::palette_add(ComponentType *newComp)
{
    palette.add (newComp);
    addAndMakeVisible (newComp);
    return newComp;
}