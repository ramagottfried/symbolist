#pragma once

#include "BaseComponent.h"

class CircleComponent : public BaseComponent
{
public:

    CircleComponent(float x, float y,
                    float w = 10, float h = 10,
                    float stroke = 2,
                    Colour color = Colours::black ) :
        BaseComponent("circle" , x - (w * .5), y - (h * .5), w , h, stroke, color )
    {}

    
    ~CircleComponent()
    {
        printf("freeing circle %p\n", this);
    }
    
    float symbol_getX() override { return getX() + ( getWidth() * .5) ; }
    float symbol_getY() override { return getY() + ( getHeight() * .5) ; }

    void symbol_paint ( Graphics& g ) override;

    
private:
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
