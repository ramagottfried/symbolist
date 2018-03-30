
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
    void cleanupPathArray();
    
    virtual void addSymbolMessages(Symbol* s) override;
    virtual void importFromSymbol(const Symbol &s) override;

    string getSymbolTypeStr() const override { return "path"; }
    
    Rectangle<float> symbol_export_bounds() override
    {
        return getBounds().toFloat();
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
    
    Path mergePathArray();
    void makePathArrayFromPath(const Path &p);

    void setEditMode(bool val) override;
    void updatePathFromPreview ();
    void abortDrawPath();
    
    void h_flip(float ax, float ay) override;
    void v_flip(float ax, float ay) override;

    virtual void rotateScoreComponent(float theta, float ax, float ay) override;
    virtual void scaleScoreComponent(float scale_w, float scale_h) override;
    
    // void resizeToFit(int x, int y, int w, int h) override;
    
    void rotatePath ( float theta, bool accum = true );
    void rotatePath ( float theta, float ax, float ay );
    virtual void accumTheta ( float theta ) {}
    
    inline Rectangle<float> getPathBounds() { return m_path_bounds; }

    
    
protected:
    
    /**
     * An array containing the Path objects composing this PathBaseComponent.
     */
    Array<Path*>            m_path_array;
    
    PathStrokeType          strokeType = PathStrokeType(2.0) ;
    bool                    m_fill = false;
    Colour                  m_fill_color;
    Colour                  m_stroke_color;
    
    // editing objects
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
    
    /**
     * The centroid point of this PathBaseComponent.
     */
    Point<float>            m_path_centroid;
    Sym_PathBounds          m_path_bounds;
        
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathBaseComponent)
};

