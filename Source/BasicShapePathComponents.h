
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
        
        if ( modif_flag )
        {   // becomes a normal path
            messages_added += PathBaseComponent::addSymbolMessages( s, base_address );
        }
        
        else
        {
            messages_added += BaseComponent::addSymbolMessages( s, base_address );

            String addr = base_address + "/fill";
            if( s->getOSCMessagePos(addr) == -1 )
            {
                s->addOSCMessage( addr,         m_fill );
                messages_added++;
            }
            
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
    
    void updatePathPoints() override
    {
        PathBaseComponent::updatePathPoints();
        modif_flag = true;
    }
    
    protected :
    
        bool modif_flag = false;
};

class CirclePathComponent : public BasicShapePathComponent
{
public:
    
    CirclePathComponent() = default;
    ~CirclePathComponent() = default;
    
    String getSymbolTypeStr() const override { return ( modif_flag ? "path" : "circle" ); }
    
    void importFromSymbol(const Symbol &s) override
    {
        if( !modif_flag )
        {
            BasicShapePathComponent::importFromSymbol(s);
            auto area = getLocalBounds().toFloat().reduced( strokeWeight );
            cleanupPathArray();
            m_path_array.add(new Path());
            m_path_array.getLast()->addEllipse(area);
            
            updatePathBounds();
        }
        else
            PathBaseComponent::importFromSymbol(s);
            
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
        if( !modif_flag )
        {
            BasicShapePathComponent::importFromSymbol(s);
            auto area = getLocalBounds().toFloat().reduced( strokeWeight );
            cleanupPathArray();
            m_path_array.add(new Path());
            m_path_array.getLast()->addRectangle(area);
            updatePathBounds();

        }
        else
            PathBaseComponent::importFromSymbol(s);
        
    }

    String getSymbolTypeStr() const override { return ( modif_flag ? "path" : "rectangle" ) ; }
    
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
        if( !modif_flag )
        {
            BasicShapePathComponent::importFromSymbol(s);
            auto area = getLocalBounds().toFloat().reduced( strokeWeight );
            cleanupPathArray();
            m_path_array.add(new Path());
            m_path_array.getLast()->addTriangle( area.getBottomLeft(), Point<float>(area.getCentreX(), area.getY()), area.getBottomRight());
            updatePathBounds();

        }
        else
            PathBaseComponent::importFromSymbol(s);
        
    }

    String getSymbolTypeStr() const override { return ( modif_flag ? "path" : "triangle" ); }
    
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TrianglePathComponent)
};
