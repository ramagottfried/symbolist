
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


    void addSymbolMessages( Symbol* s ) override
    {
        if ( modif_flag )
        {   // becomes a normal path
            PathBaseComponent::addSymbolMessages( s );
        }
        
        else
        {
            BaseComponent::addSymbolMessages( s ) ;

            
            s->addMessage ( "/fill" ,               m_fill   );
            s->addMessage ( "/stroke/thickness" ,   strokeType.getStrokeThickness()   );
            s->addMessage ( "/rotation" ,           m_rotation   );

        }
       

    }
    
    void importFromSymbol(const Symbol &s) override
    {
        if( !modif_flag )
        {
           BaseComponent::importFromSymbol(s);
            
            m_fill = s.getMessage("/fill").getInt();
            
            strokeWeight = s.getMessage("/stroke/thickness").getInt();
            strokeWeight = (strokeWeight == 0) ? 2 : strokeWeight;

            strokeType.setStrokeThickness( strokeWeight );
            
            m_rotation = s.getMessage("/rotation").getFloat();
            
            
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
    
    string getSymbolTypeStr() const override { return ( modif_flag ? "path" : "circle" ); }
    
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

    string getSymbolTypeStr() const override { return ( modif_flag ? "path" : "rectangle" ) ; }

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

    string getSymbolTypeStr() const override { return ( modif_flag ? "path" : "triangle" ); }

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
