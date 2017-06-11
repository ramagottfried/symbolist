
#pragma once

#include "PathBaseComponent.h"

class LinePathComponent : public PathBaseComponent
{
public:
    LinePathComponent(const Symbol &s) : PathBaseComponent( s )
    {
        std::cout << "LINE " << this << std::endl;
    }
    
    void mouseDrag(const MouseEvent& event) override;
    void mouseMove(const MouseEvent& event) override;
    void mouseDown(const MouseEvent& event) override;
};
