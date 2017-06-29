
#pragma once

#include "BaseComponent.h"
#include "PathHandleComponent.h"
#include "PathInfo.h"

class PathBaseComponent : public BaseComponent 
{
public:
    
    PathBaseComponent() = default;
    ~PathBaseComponent() ;
    
    static void printPath( Path p, const char* name = "path" );
    void cleanupPathArray();
    
    int addSymbolMessages(Symbol* s, const String &base_address) override;
    virtual void importFromSymbol(const Symbol &s) override;

    String getSymbolTypeStr() const override { return "path"; }
    
    Rectangle<float> symbol_export_bounds() override
    {
        return getBounds().toFloat();
    }
    
    void addHandle( PathHandle::handleType type, float x, float y );
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
    
    void resizeToFit(int x, int y, int w, int h) override;
    bool intersectRect( Rectangle<int> rect) override;

    void setEditMode(bool val) override;
    void updatePathFromPreview ();
    void abortDrawPath();
    
    void h_flip() override;
    void v_flip() override;
    void rotatePath ( float theta );
    void rotatePath ( float theta, float ax, float ay );
    
    
    inline Rectangle<float> getPathBounds() { return m_path_bounds; }


protected:
    
    Array<Path*>             m_path_array;
    
    PathStrokeType          strokeType = PathStrokeType(2.0) ;
    bool                    m_fill = false;
    Colour                  m_fill_color;
    Colour                  m_stroke_color;
    
    // editing objects
    bool                        drawing = false;
    Path                        m_preview_path;
    Colour                      preview_stroke_color = Colours::cornflowerblue ;
    
    Point<float>                m_path_origin;
    Array<PathHandle*>          path_handles;
    PathHandle*                 rotation_handle = NULL;
    //Point<float>                m_prev_drag;
    
    Point<float>                m_path_centroid;
    Sym_PathBounds              m_path_bounds;
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathBaseComponent)
};

