
#pragma once

#include "PathBaseComponent.h"

class CirclePathComponent : public PathBaseComponent
{
public:
    CirclePathComponent(const Symbol &s) :  PathBaseComponent(s)
    {
        std::cout << "CIRCLE" << std::endl;

        // x - (w * .5), y - (h * .5)
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        printRect(area, "area");
        
        auto hw = area.getWidth() * 0.5f;
        auto hw55 = hw * 0.55f;
        auto hh = area.getHeight() * 0.5f;
        auto hh55 = hh * 0.55f;
        auto cx = area.getX() + hw;
        auto cy = area.getY() + hh;
        
        m_path.startNewSubPath (cx, cy - hh);
        m_path.cubicTo (cx + hw55, cy - hh, cx + hw, cy - hh55, cx + hw, cy);
        m_path.cubicTo (cx + hw, cy + hh55, cx + hw55, cy + hh, cx, cy + hh);
        m_path.cubicTo (cx - hw55, cy + hh, cx - hw, cy + hh55, cx - hw, cy);
        m_path.cubicTo (cx - hw, cy - hh55, cx - hw55, cy - hh, cx, cy - hh);
        m_path.closeSubPath();
                
    }
    
    String getSymbolTypeStr() const override { return "circle"; }

        
};
