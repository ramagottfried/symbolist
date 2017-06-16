
#pragma once

#include "PathBaseComponent.h"

class LinePathComponent : public PathBaseComponent
{
public:
    LinePathComponent(const Symbol &s);
    
    void mouseDrag(const MouseEvent& event) override;
    void mouseMove(const MouseEvent& event) override;
    void mouseDown(const MouseEvent& event) override;
    void mouseUp(const MouseEvent& event) override;
    
    void deselectComponent () override;
    void selectComponent () override;

    void newPathDrawing ();
  
    void componentCretated() override;

    String getSymbolTypeStr() const override { return "path"; }

private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (LinePathComponent)
    
};
