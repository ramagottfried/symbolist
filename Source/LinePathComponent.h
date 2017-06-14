
#pragma once

#include "PathBaseComponent.h"

class LinePathComponent : public PathBaseComponent
{
public:
    LinePathComponent(const Symbol &s);
    
    String getSymbolTypeStr() const override { return "line"; }

    void mouseDrag(const MouseEvent& event) override;
    void mouseMove(const MouseEvent& event) override;
    void mouseDown(const MouseEvent& event) override;
    void mouseUp(const MouseEvent& event) override;
    
    void deselectComponent () override;
    void selectComponent () override;

    void newPathDrawing ();
  
    void componentCretated() override;

};
