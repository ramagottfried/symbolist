#pragma once

#include "BaseComponent.h"

class CircleComponent : public BaseComponent
{
public:

    CircleComponent(float x, float y,
                    float w = 10, float h = 10,
                    float stroke = 2,
                    Colour color = Colours::black );
    
    
    ~CircleComponent()
    {
        printf("freeing circle %p\n", this);
    }
    
    float symbol_getX() override { return getX() + ( getWidth() * .5) ; }
    float symbol_getY() override { return getY() + ( getHeight() * .5) ; }

    void symbol_paint ( Graphics& g ) override;
    void resized() override;
    
private:
    Path            m_path;
    PathStrokeType  strokeType;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
