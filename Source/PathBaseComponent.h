
#pragma once

#include "BaseComponent.h"
#include "PathHandleComponent.h"
#include "PathInfo.h"

class PathBaseComponent : public BaseComponent 
{
public:
    
    PathBaseComponent(  const Symbol& s );
    ~PathBaseComponent() ;
    
    static void printPath( Path p, const char* name = "path" );
    
    int addSymbolMessages(Symbol* s, const String &base_address) override;
    void importFromSymbol(const Symbol &s) override;
    
    void addHandle( PathHandle::handleType type, float x, float y );
    void removeHandle(PathHandle* h);
    void addHandlesTo( Point<float> p, PathHandle* last );
    void insertHandleBefore( PathHandle* target );
    void makeHandlesFromPath();
    void removeHandles();
    void updateHandlePositions();
    void updatePathPoints();
    void drawHandlesLines( Graphics& g);
    
    void paint ( Graphics& g ) override;
    void resized () override;
    
    void mouseUp(const MouseEvent& event) override;
    void mouseMove(const MouseEvent& event) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseDoubleClick( const MouseEvent& event ) override;
    void mouseAddClick ( const MouseEvent& event ) override;
    bool hitTest (int x, int y) override;
    Point<float> shiftConstrainMouseAngle( const PathHandle* last, const MouseEvent& event );
    
    void setMinimalBounds () override;
    void setMaximalBounds () override;
    void updatePathBounds ();
    void resizeToFit(int x, int y, int w, int h) override;
    Rectangle<float> tranformAndGetBoundsInParent( Path& p );
    
    
    void setEditMode(bool val) override;
    void updatePathFromPreview ();
    void abortDrawPath();
    
    void h_flip() override;
    void v_flip() override;
    void rotatePath ( float theta );
    void rotatePath ( float theta, float ax, float ay );
    
    
    inline Rectangle<float> getPathBounds() { return m_path_bounds; }


protected:
    
    Path                    m_path;
    
    PathStrokeType          strokeType = PathStrokeType(2.0) ;
    bool                    fill = false;
    Colour                  fill_color;
    Colour                  stroke_color;
    
    // editing objects
    bool                    drawing = false;
    Path                        m_preview_path;
    Colour                      preview_stroke_color = Colours::cornflowerblue ;
    
    Point<float>                m_path_origin;
    Array<PathHandle*>          path_handles;
    
    //Point<float>                m_prev_drag;
    
    Point<float>                m_path_centroid;
    Sym_PathBounds              m_path_bounds;
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathBaseComponent)
};

