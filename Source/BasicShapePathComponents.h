
#pragma once

#include "PathBaseComponent.h"


class BasicShapePathComponent : public PathBaseComponent
{
    public :
    
    BasicShapePathComponent() = default;
    ~BasicShapePathComponent() = default;
    
    Rectangle<float> symbol_export_bounds() override
    {
        auto b = getBounds().toFloat();
        return Rectangle<float>( b.getX(), b.getY() + b.getHeight()/2, b.getWidth(), b.getHeight() );
    }
    
    void setBoundsFromSymbol( float x, float y , float w , float h) override final
    {
        setBounds( x, y - (h * 0.5), w , h);
    }

    int addSymbolMessages( Symbol* s, const String &base_address ) override
    {
        int messages_added = 0;
     
        auto b = symbol_export_bounds();
        s->addOSCMessage ((String(base_address) += "/type") ,   getSymbolTypeStr());
        s->addOSCMessage ((String(base_address) += "/x") ,      b.getX() );
        s->addOSCMessage ((String(base_address) += "/y") ,      b.getY() );
        s->addOSCMessage ((String(base_address) += "/w") ,      b.getWidth() );
        s->addOSCMessage ((String(base_address) += "/h") ,      b.getHeight() );
        s->addOSCMessage ((String(base_address) += "/time/start") , b.getX() * 0.01f );
        s->addOSCMessage ((String(base_address) += "/duration"),    b.getWidth() * 0.01f );
        
        messages_added += 7;
        
        return messages_added;
    }
    
};

class CirclePathComponent : public BasicShapePathComponent
{
public:
    
    CirclePathComponent() = default;
    ~CirclePathComponent() = default;
    
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
    
    RectanglePathComponent() = default;
    ~RectanglePathComponent() = default;
    
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
    
    TrianglePathComponent() = default;
    ~TrianglePathComponent() = default;
    
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
