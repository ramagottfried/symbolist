
#pragma once

#include "PathBaseComponent.h"


class BasicShapePathComponent : public PathBaseComponent
{
    
public:
    
    BasicShapePathComponent() = default;
    ~BasicShapePathComponent() = default;
    
    /************
     * Basic Shape Components use the center point as their reference
     * additionally, depending on their type, and rotation the w and h could be different...
     ************/
    
    Rectangle<float> symbol_export_bounds() override
    {
        auto b = getBounds().toFloat();
        Path p = mergePathArray();

        Sym_PathBounds pbounds( p );
        
        // rotate backwards and to get size values
        p.applyTransform( AffineTransform().rotation( -m_rotation, pbounds.getCentreX(), pbounds.getCentreY()  ) );
        auto pb = pbounds.getRealPathBounds( p ).expanded( strokeType.getStrokeThickness() );
        
        return Rectangle<float>( b.getX(), b.getCentreY(), pb.getWidth(), pb.getHeight() );
    }
    
    virtual Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) = 0;
    
    // called from BaseComponent::importSymbol to set bounds
    // in the case of BasicShapes, w & h are the *pre-rotated values*
    void setBoundsFromSymbol( float x, float y , float w , float h) override final
    {
        m_w = w;
        m_h = h;
        
        auto bounds = drawAndRotateShape(x, y, w, h).expanded( strokeType.getStrokeThickness() );
        setBounds( x , y - (h * 0.5), bounds.getWidth() , bounds.getHeight() );
    }
    
    
    void accumTheta ( float theta ) override
    {
        m_rotation += theta;
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
            
            addr = base_address + "/stroke/thickness";
            if( s->getOSCMessagePos(addr) == -1 )
            {
                s->addOSCMessage( addr,         strokeType.getStrokeThickness() );
                messages_added++;
            }

            addr = base_address + "/rotation";
            if( s->getOSCMessagePos(addr) == -1 )
            {
                s->addOSCMessage( addr,         m_rotation );
                messages_added++;
            }
        }
       
        return messages_added;
    }
    
    void importFromSymbol(const Symbol &s) override
    {
        if( !modif_flag )
        {
            
            int pos = s.getOSCMessagePos("/fill");
            if( pos != -1  )
                m_fill = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(pos) );
                
            pos = s.getOSCMessagePos("/stroke/thickness");
            if( pos != -1  )
                strokeType.setStrokeThickness( Symbol::getOSCValueAsFloat( s.getOSCMessageValue(pos) ) );

            // rotation needs to be set before BaseComponent::import calls setBoundsFromSymbol
            pos = s.getOSCMessagePos("/rotation");
            if( pos != -1  )
                m_rotation = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(pos) );
            
            
            BaseComponent::importFromSymbol(s);
            
        }
        else
            PathBaseComponent::importFromSymbol(s);


        
    }
    
    void updatePathPoints() override
    {
        PathBaseComponent::updatePathPoints();
        modif_flag = true;
    }
    
protected:
    
    bool modif_flag = false;
    float m_rotation = 0;
    float m_w, m_h;
};

class CirclePathComponent : public BasicShapePathComponent
{
public:
    
    CirclePathComponent() = default;
    ~CirclePathComponent() = default;
    
    String getSymbolTypeStr() const override { return ( modif_flag ? "path" : "circle" ); }
    
    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) override
    {
        auto area = Rectangle<float>(0,0,w,h).reduced( strokeWeight );
        cleanupPathArray();
        m_path_array.add(new Path());
        m_path_array.getLast()->addEllipse(area);
        updatePathBounds();
        rotatePath(m_rotation, false);
        return m_path_bounds; // return bounds post rotation
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

    String getSymbolTypeStr() const override { return ( modif_flag ? "path" : "rectangle" ) ; }

    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) override
    {
        auto area = Rectangle<float>(0,0,w,h).reduced( strokeWeight );
        cleanupPathArray();
        m_path_array.add(new Path());
        m_path_array.getLast()->addRectangle(area);
        updatePathBounds();
        //rotateScoreComponent(m_rotation, cx, cy);
        rotatePath(m_rotation, false);
        return m_path_bounds; // return bounds post rotation
    }
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (RectanglePathComponent)
};


class TrianglePathComponent : public BasicShapePathComponent
{
public:
    
    TrianglePathComponent() = default;
    ~TrianglePathComponent() = default;

    String getSymbolTypeStr() const override { return ( modif_flag ? "path" : "triangle" ); }

    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) override
    {
        auto area = Rectangle<float>(0,0,w,h).reduced( strokeWeight );
        cleanupPathArray();
        m_path_array.add(new Path());
        m_path_array.getLast()->addTriangle( area.getBottomLeft(), Point<float>(area.getCentreX(), area.getY()), area.getBottomRight());
        updatePathBounds();
        rotatePath(m_rotation, false);
        return m_path_bounds; // return bounds post rotation
    }
    
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TrianglePathComponent)
};
