
#pragma once

#include "PathBaseComponent.h"

class LinePathComponent : public PathBaseComponent
{
public:
    LinePathComponent(const Symbol &s);
    
    String getSymbolTypeStr() const { return "line"; }

    void mouseDrag(const MouseEvent& event) override;
    void mouseMove(const MouseEvent& event) override;
    void mouseDown(const MouseEvent& event) override;
    
    void deselectComponent () override;
    void selectComponent () override;

    void notifyEditModeChanged( UI_EditType current_mode ) override;
    
    void newPathDrawing ();
    void endPathDrawing ();

    Rectangle<float> applyTranformAndGetNewBounds( Path& p );
    
    void updatePathFromPreivew();
private:
    Point<float>  ref_point;
    
};
