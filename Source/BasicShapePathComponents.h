
#pragma once

#include "PathBaseComponent.h"

class CirclePathComponent : public PathBaseComponent
{
public:
    CirclePathComponent(const Symbol &s) : PathBaseComponent(s)
    {
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path.addEllipse(area);
    }
    
    String getSymbolTypeStr() const override { return "circle"; }

private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CirclePathComponent)
    
};


class RectanglePathComponent : public PathBaseComponent
{
public:
    RectanglePathComponent(const Symbol &s) : PathBaseComponent(s)
    {
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path.addRectangle( area );
    }
    
    String getSymbolTypeStr() const override { return "rectangle"; }
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (RectanglePathComponent)
};


class TrianglePathComponent : public PathBaseComponent
{
public:
    TrianglePathComponent(const Symbol &s) : PathBaseComponent(s)
    {
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path.addTriangle( area.getBottomLeft(), Point<float>(area.getCentreX(), area.getY()), area.getBottomRight());
    }
    
    String getSymbolTypeStr() const override { return "triangle"; }
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TrianglePathComponent)
};
