
#pragma once

#include "PathBaseComponent.h"

class LinePathComponent : public PathBaseComponent
{
public:
    LinePathComponent(const Symbol &s) : PathBaseComponent( s ) { std::cout << "LINE" << std::endl; }
    
    void mouseDrag(const MouseEvent& event) override;
    
};
