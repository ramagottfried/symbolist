
#pragma once

#include "PathBaseComponent.h"


class BasicShapePathComponent : public PathBaseComponent
{
    public :
    
    BasicShapePathComponent(const Symbol &s) : PathBaseComponent(s) {}
    
    float symbol_export_X() override { return getX();/* + getWidth()/2;*/ }
    float symbol_export_Y() override { return getY() + getHeight()/2; }
    
    void setBoundsFromSymbol( float x, float y , float w , float h) override final
    {
//        setBounds( x - (w * 0.5) , y - (h * 0.5), w , h);
        setBounds( x, y - (h * 0.5), w , h);

    }

    int addSymbolMessages( Symbol* s, const String &base_address ) override
    {
        int messages_added = 0;
        
        s->addOSCMessage ((String(base_address) += "/type") ,   getSymbolTypeStr());
        s->addOSCMessage ((String(base_address) += "/x") ,      symbol_export_X());
        s->addOSCMessage ((String(base_address) += "/y") ,      symbol_export_Y());
        s->addOSCMessage ((String(base_address) += "/w") ,      (float) getWidth());
        s->addOSCMessage ((String(base_address) += "/h") ,      (float) getHeight());
        s->addOSCMessage ((String(base_address) += "/time/start") , symbol_export_X() * 0.01f );
        s->addOSCMessage ((String(base_address) += "/duration"),    (float) getWidth() * 0.01f );
        
        messages_added += 7;
        
        return messages_added;
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
