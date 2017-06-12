
#pragma once

#include "PathBaseComponent.h"

class LinePathComponent : public PathBaseComponent
{
public:
    LinePathComponent(const Symbol &s);
    
    void mouseDrag(const MouseEvent& event) override;
    void mouseMove(const MouseEvent& event) override;
    void mouseDown(const MouseEvent& event) override;
    
    void deselectComponent () override;
    void selectComponent () override;
};
