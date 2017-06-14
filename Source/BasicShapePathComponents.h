
#pragma once

#include "PathBaseComponent.h"


class BasicShapePathComponent : public PathBaseComponent
{
    public :
    
    BasicShapePathComponent(const Symbol &s) : PathBaseComponent(s) {}
    
    void setBoundsFromSymbol( float x, float y , float w , float h) override final
    {
        setBounds( x - (w * 0.5) , y - (h * 0.5), w , h);
    }
        
};

class CirclePathComponent : public BasicShapePathComponent
{
public:
    
    CirclePathComponent(const Symbol &s) : BasicShapePathComponent ( s ) {};
    
    String getSymbolTypeStr() const override { return "circle"; }
    
    void importFromSymbol(const Symbol &s) override
    {
        BaseComponent::importFromSymbol(s);
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path.addEllipse(area);
    }

private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CirclePathComponent)
    
};


class RectanglePathComponent : public BasicShapePathComponent
{
public:
    
    RectanglePathComponent(const Symbol &s) : BasicShapePathComponent(s) {}
    
    void importFromSymbol(const Symbol &s) override
    {
        BaseComponent::importFromSymbol(s);
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path.addRectangle(area);
    }

    String getSymbolTypeStr() const override { return "rectangle"; }
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (RectanglePathComponent)
};


class TrianglePathComponent : public BasicShapePathComponent
{
public:
    
    TrianglePathComponent(const Symbol &s) : BasicShapePathComponent(s) {}
    
    void importFromSymbol(const Symbol &s) override
    {
        BaseComponent::importFromSymbol(s);
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path.addTriangle( area.getBottomLeft(), Point<float>(area.getCentreX(), area.getY()), area.getBottomRight());
    }

    String getSymbolTypeStr() const override { return "triangle"; }
    
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TrianglePathComponent)
};
