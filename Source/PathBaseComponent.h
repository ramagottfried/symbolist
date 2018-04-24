
#pragma once

#include "BaseComponent.h"
#include "PathHandleComponent.h"
#include "PathInfo.h"

/**
 * Describes a graphic component composed of one or multiple paths.
 */
class PathBaseComponent : public BaseComponent 
{
public:
    
    PathBaseComponent() = default;
    ~PathBaseComponent() ;
    
    static void printPath( Path p, const char* name = "path" );
    
    virtual void addSymbolMessages(Symbol* s) override;
    virtual void importFromSymbol(const Symbol &s) override;

    virtual void setBoundsFromSymbol( float x, float y , float w , float h) override;
    virtual Point<float> computeSymbolPosition( float x, float y, float w, float h ) override;

    Rectangle<float> symbol_export_bounds() override
    {
        
        // initially for simple path:
        //return getBounds().toFloat();
        
        auto b = getBounds().toFloat();
        
        Sym_PathBounds pbounds( m_path );
        
        // rotate backwards and to get size values
        m_path.applyTransform( AffineTransform().rotation( -m_rotation, pbounds.getCentreX(), pbounds.getCentreY()  ) );
        auto pb = pbounds.getRealPathBounds( m_path ).expanded( m_stroke_type.getStrokeThickness() );
        
        return Rectangle<float>( b.getX(), b.getCentreY(), pb.getWidth(), pb.getHeight() );
    }
    
    void addHandle( PathHandle::handleType type, float x, float y );
    void updateRotationHandle();
    
    void subtractHandle( int i );

    
    void addHandlesTo( Point<float> p, PathHandle* last );
    void insertHandleBefore( PathHandle* target );
    void makeHandlesFromPath();
    void removeHandles();
    void updateHandlePositions();
    virtual void updatePathPoints();
    void drawHandlesLines( Graphics& g);

    void removeSubcomponent(SymbolistComponent* c) override;
    void unselectAllComponents() override;
    
    void paint ( Graphics& g ) override;
    void resized () override;

    bool intersectRect( Rectangle<int> rect) override;
    
    void mouseUp(const MouseEvent& event) override;
    void mouseMove(const MouseEvent& event) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseDoubleClick( const MouseEvent& event ) override;
    void mouseAddClick ( const MouseEvent& event ) override;
    bool hitTest (int x, int y) override;
    static Point<float> shiftConstrainMouseAngle( const PathHandle* last, const MouseEvent& event );
    
    void setMinimalBounds () override;
    void setMaximalBounds () override;
    void updatePathBounds ();
    
    void setEditMode(bool val) override;
    void updatePathFromPreview ();
    void abortDrawPath();
    
    void h_flip(float ax, float ay) override;
    void v_flip(float ax, float ay) override;

    virtual void rotateScoreComponent(float theta, float ax, float ay) override;
    virtual void scaleScoreComponent(float scale_w, float scale_h) override;
    
    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h);

    // void resizeToFit(int x, int y, int w, int h) override;
    
    void rotatePath ( float theta, bool accum = true );
    void rotatePath ( float theta, float ax, float ay );
    
    inline void accumTheta ( float theta ) { m_rotation += theta; }
    inline Rectangle<float> getPathBounds() { return m_path_bounds; }

    string exportSVG();

    
protected:
    
    /**
     * An array containing the Path objects composing this PathBaseComponent.
     */
    Path                    m_path;
    PathStrokeType          m_stroke_type = PathStrokeType(2.0) ;
    bool                    m_fill = false;
    Colour                  m_fill_color;
    Colour                  m_stroke_color;
    
    // editing utils
    bool                    drawing = false;
    Path                    m_preview_path;
    Colour                  preview_stroke_color = Colours::cornflowerblue ;
    
    Point<float>            m_path_origin;
    
    // note: path_handles are also stored as subcomponents in the main score, to use the selection system... maybe not both necessary...
    /**
     * An array containing PathHandle objects used to manipulate the paths
     * composing this PathBaseComponent.
     */
    Array<PathHandle*>      path_handles;
    
    /**
     * A pointer to the PathHandle object controlling
     * the rotation of this PathBaseComponent.
     */
    PathHandle*             rotation_handle = NULL;
    float m_rotation = 0; // get rifd of this ?
    
    /**
     * The centroid point of this PathBaseComponent.
     */
    Point<float>            m_path_centroid;
    Sym_PathBounds          m_path_bounds;
        
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathBaseComponent)
};



/*
 String svgFile = R"(<svg>
 <rect x='119.2' y='139.2' transform='matrix(0.8965 -0.443 0.443 0.8965 -57.7869 83.0835)' fill='#ED1C24' stroke='#000000' stroke-miterlimit='10' width='59.5' height='52.1'/>
 <path fill='#ED1C24' stroke='#000000' stroke-miterlimit='10' d='M234.9,110.3c9.1-31.4,71.1,38,71.1,38s31.4,130.6-18.2,84.3
 s-47.9-83.5-5.8-49.6s97.5,38,101.7,65.3s-54.5,90.1-74.4,76c-19.8-14,80.2-68.6,97.5-55.4'/>
 <path fill='#ED1C24' stroke='#000000' stroke-miterlimit='10' d='M475.4,416.1'/>
 </svg>)";
 
 std::unique_ptr<DrawableComposite> svg = std::unique_ptr<DrawableComposite>( dynamic_cast<DrawableComposite*> ( DrawableComposite::createFromSVG( *std::unique_ptr<XmlElement>( XmlDocument::parse( svgFile ) ).get() ) )
 );
*/


