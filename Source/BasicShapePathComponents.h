
#pragma once

#include "PathBaseComponent.h"

/**
 * Describes a graphic component using a center point as its reference.
 * Additionally, depending on their type, and rotation the w and h could be different...
 */
class BasicShapePathComponent : public PathBaseComponent {
    
public:
    
    BasicShapePathComponent() = default;
    ~BasicShapePathComponent() = default;

    Rectangle<float> symbol_export_bounds() override
    {
        auto b = getBounds().toFloat();
        Path p = mergePathArray();

        Sym_PathBounds pbounds( p );
        
        // rotate backwards and to get size values
        p.applyTransform( AffineTransform().rotation( -m_rotation, pbounds.getCentreX(), pbounds.getCentreY()  ) );
        auto pb = pbounds.getRealPathBounds( p ).expanded( stroke_type.getStrokeThickness() );
        
        return Rectangle<float>( b.getX(), b.getCentreY(), pb.getWidth(), pb.getHeight() );
    }
    
    virtual Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) = 0;
    
    // called from BaseComponent::importSymbol to set bounds
    // in the case of BasicShapes, w & h are the *pre-rotated values*
    void setBoundsFromSymbol( float x, float y , float w , float h) override final
    {
        m_w = w;
        m_h = h;
        
        auto bounds = drawAndRotateShape(x, y, w, h).expanded( stroke_type.getStrokeThickness() );
        setBounds( x , y - (h * 0.5), bounds.getWidth() , bounds.getHeight() );
    }
    
    Point<float> computeSymbolPosition(float x, float y, float w, float h) override
    {
        return Point<float>( x , y + (h * 0.5) );
    }
    
    void accumTheta ( float theta ) override
    {
        m_rotation += theta;
    }


    inline void addSymbolMessages(Symbol* s) override
    {
        if ( modif_flag )
        {   // becomes a normal path
            PathBaseComponent::addSymbolMessages(s);
        }
        
        else
        {
            BaseComponent::addSymbolMessages(s) ;

            
            s->addMessage ( "/fill" ,               m_fill   );
            s->addMessage ( "/stroke/thickness" ,   stroke_type.getStrokeThickness()   );
            s->addMessage ( "/rotation" ,           m_rotation   );

        }
       

    }
    
    void importFromSymbol(const Symbol &s) override
    {
        if( !modif_flag )
        {
           BaseComponent::importFromSymbol(s);
            
            m_fill = s.getMessage("/fill").getInt();
            
            stroke_weight = s.getMessage("/stroke/thickness").getInt();
            stroke_weight = (stroke_weight == 0) ? 2 : stroke_weight;

            stroke_type.setStrokeThickness( stroke_weight );
            
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

/**
 * Describes graphic components representing a circle.
 */
class CirclePathComponent : public BasicShapePathComponent
{
public:
    
    CirclePathComponent() = default;
    ~CirclePathComponent() = default;
    
    string getSymbolTypeStr() const override { return ( modif_flag ? "path" : "circle" ); }
    
    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) override
    {
        auto area = Rectangle<float>(0,0,w,h).reduced( stroke_weight );
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

/**
 * Describes a graphic component representing a rectangle.
 */
class RectanglePathComponent : public BasicShapePathComponent
{
public:
    
    RectanglePathComponent()
    {
        addAndMakeVisible(svg.get());
    }
    ~RectanglePathComponent() = default;

    string getSymbolTypeStr() const override { return ( modif_flag ? "path" : "rectangle" ) ; }

    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) override
    {
        auto area = Rectangle<float>(0,0,w,h).reduced( stroke_weight );
        cleanupPathArray();
        m_path_array.add(new Path());
        m_path_array.getLast()->addRectangle(area);
        updatePathBounds();
        rotateScoreComponent(m_rotation, cx, cy);
        //rotatePath(m_rotation, false);
        return m_path_bounds; // return bounds post rotation
    }
    
private:
    String svgFile = R"(<svg>
    <rect x='119.2' y='139.2' transform='matrix(0.8965 -0.443 0.443 0.8965 -57.7869 83.0835)' fill='#ED1C24' stroke='#000000' stroke-miterlimit='10' width='59.5' height='52.1'/>
    <path fill='#ED1C24' stroke='#000000' stroke-miterlimit='10' d='M234.9,110.3c9.1-31.4,71.1,38,71.1,38s31.4,130.6-18.2,84.3
    s-47.9-83.5-5.8-49.6s97.5,38,101.7,65.3s-54.5,90.1-74.4,76c-19.8-14,80.2-68.6,97.5-55.4'/>
    <path fill='#ED1C24' stroke='#000000' stroke-miterlimit='10' d='M475.4,416.1'/>
    </svg>)";
    
    std::unique_ptr<DrawableComposite> svg = std::unique_ptr<DrawableComposite>( dynamic_cast<DrawableComposite*> ( DrawableComposite::createFromSVG( *std::unique_ptr<XmlElement>( XmlDocument::parse( svgFile ) ).get() ) )
                                                                                );
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (RectanglePathComponent)
};

/**
 * Describes a graphic component representing a triangle.
 */
class TrianglePathComponent : public BasicShapePathComponent
{
public:
    
    TrianglePathComponent() = default;
    ~TrianglePathComponent() = default;

    string getSymbolTypeStr() const override { return ( modif_flag ? "path" : "triangle" ); }

    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h) override
    {
        auto area = Rectangle<float>(0,0,w,h).reduced( stroke_weight );
        cleanupPathArray();
        m_path_array.add(new Path());
        m_path_array.getLast()->addTriangle( area.getBottomLeft(), Point<float>(area.getCentreX(), area.getY()), area.getBottomRight());
        updatePathBounds();
        rotateScoreComponent(m_rotation, cx, cy);
        //rotatePath(m_rotation, false);
        return m_path_bounds; // return bounds post rotation
    }
    
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TrianglePathComponent)
};
