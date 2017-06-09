
#pragma once

#include "PathBaseComponent.h"

class LinePathComponent : public PathBaseComponent
{
public:
    LinePathComponent(float x, float y) : PathBaseComponent(x,y)
    {}
    
    void mouseDrag(const MouseEvent& event) override;
    
};