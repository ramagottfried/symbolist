
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
    
    PathBaseComponent() ;
    PathBaseComponent( t_sym_type sType ) : BaseComponent( sType ) {} ;
    ~PathBaseComponent() ;
    
    // utility
    static void printPath( Path p, const char* name = "path" );
    
    void paint ( Graphics& g ) override;
    
    bool intersectRect( Rectangle<int> rect) override;

    virtual void addSymbolMessages(Symbol* s) override;
    
    
    virtual void importFromSymbol(const Symbol &s) override;

    // Set component attributes from Symbol
    // x, y , w  h are given but could be retrieved from Symbol s
    virtual void setComponentFromSymbol(const Symbol &s, float x, float y , float w , float h) override;

    // Get the Symbol's stored "symbolic" position given components coordinates
    virtual Point<float> computeSymbolPosition( float x, float y, float w, float h ) override;
    
    // Get the Component's graphic position according to the Symbols pos and size
    virtual Point<float> computePositionFromSymbolValues(float x, float y, float w, float h) override;

    Rectangle<float> drawAndRotateShape(float cx, float cy, float w, float h);

    
    Rectangle<float> symbol_export_bounds() override;
    
    /****
     * FOR EDIT MODE ONLY
     **/
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
    
    void rotatePath ( float theta, bool accum = true );
    void rotatePath ( float theta, float ax, float ay );
    
    inline void accumTheta ( float theta ) { m_rotation += theta; }
    inline Rectangle<float> getPathBounds() { return m_path_bounds; }

    string exportSVG();

    
protected:
    
    /**
     * The actual Juce 'Path' object
     */
    Path                    m_path;
    
    /**
     * Draw attributes
     */
    PathStrokeType          m_stroke_type = PathStrokeType(2.0) ;
    Colour                  m_stroke_color;
    bool                    m_fill = false;
    Colour                  m_fill_color;
    float                   m_rotation = 0;
    
    /**
     * The 'symbolic' center of the component, as specified in the Symbol
     * Also used as the center of rotation
     */
    Point<float>            m_component_center = Point<float>( 0.0 , 0.0 );
    
    
    /**
     * EDITING UTILS (only in Edit mode)
     */
    bool                    drawing = false;
    Path                    m_preview_path;
    Colour                  preview_stroke_color = Colours::cornflowerblue ;
    
    /**
     * An array containing PathHandle objects used to manipulate the path
     */
    Array<unique_ptr<PathHandle>>      path_handles;
    // note: path_handles are also stored as subcomponents in the main score
    // to use the selection system... maybe not both necessary...

    /**
     * A PathHandle object controlling
     * the rotation of this PathBaseComponent.
     */
    unique_ptr<PathHandle> rotation_handle = unique_ptr<PathHandle>( new PathHandle( PathHandle::rotate, 0.0, 0.0) ) ;

    
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


