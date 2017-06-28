
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
        int messages_added = BaseComponent::addSymbolMessages( s, base_address );
     
        if( s->getOSCMessagePos("/fill") == -1 )
        {
            s->addOSCMessage ((String(base_address) += "/fill"), m_fill );
            messages_added++;
        }
        
        return messages_added;
    }
    
    void importFromSymbol(const Symbol &s) override
    {
        BaseComponent::importFromSymbol(s);
        int fill_pos = s.getOSCMessagePos("/fill");
        if( fill_pos != -1  )
            m_fill = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(fill_pos) );
            
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
        BasicShapePathComponent::importFromSymbol(s);
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path_array.add(new Path());
        m_path_array.getLast()->addEllipse(area);
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
        BasicShapePathComponent::importFromSymbol(s);
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path_array.add(new Path());
        m_path_array.getLast()->addRectangle(area);
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
        BasicShapePathComponent::importFromSymbol(s);
        auto area = getLocalBounds().toFloat().reduced( strokeWeight );
        m_path_array.add(new Path());
        m_path_array.getLast()->addTriangle( area.getBottomLeft(), Point<float>(area.getCentreX(), area.getY()), area.getBottomRight());
    }

    String getSymbolTypeStr() const override { return "triangle"; }
    
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TrianglePathComponent)
};
