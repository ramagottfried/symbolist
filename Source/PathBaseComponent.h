
#pragma once

#include "BaseComponent.h"
#include "PathHandleComponent.h"
#include "PathInfo.h"

class PathBaseComponent : public BaseComponent 
{
public:
    
    PathBaseComponent(  const Symbol& s );
    ~PathBaseComponent() ;
    
    void printPath( Path p, const char* name = "path" );
    
    int addSymbolMessages(Symbol* s, const String &base_address) override;
    
    void importFromSymbol(const Symbol &s) override;
    
    void addHandle( int type, float x, float y );
    void makeHandles();
    void removeHandles();
    void updateHandlePositions();

    void updatePathPoints();

    void drawHandlesLines( Graphics& g);
    
    void paint ( Graphics& g ) override;
    void resized () override;
    
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseAddClick ( Point<float> p ) override;
    
    void h_flip() override;
    void v_flip() override;
    void rotatePath ( float theta );
    void rotatePath ( float theta, float ax, float ay );
    
    void setMinimalBounds () override;
    void setMaximalBounds () override;
    void updatePathBounds ();
    void resizeToFit(int x, int y, int w, int h) override;
    
    Rectangle<float> tranformAndGetBoundsInParent( Path& p );
    
    inline Point<float> getCentroid(){ return m_path_centroid; }
    inline Rectangle<float> getPathBounds() { return m_path_bounds; }
    
    virtual void componentCretated() override {}
    
    void setEditMode(bool val) override;
    void updatePathFromPreview ();
    
    bool hitTest (int x, int y) override
    {
        if( in_edit_mode || is_selected )
            return true;
        
        return m_path.intersectsLine( Line<float>( x - 5, y - 5, x + 5, y + 5) ) || m_path.intersectsLine( Line<float>( x + 5, y - 5, x - 5, y + 5) );
    }


protected:
    
    Path                    m_path;
    
    bool                    fill = false;
    Colour                  fill_color;
    
    PathStrokeType          strokeType = PathStrokeType(2.0) ;
    Colour                  stroke_color;
    
    // editing objects
    Path                        m_preview_path;
    Colour                      preview_stroke_color = Colours::cornflowerblue ;

    
    Point<float>                m_path_origin;
    std::vector<PathHandle*>    path_handles;
    
    Point<float>                m_prev_drag;
    Point<float>                m_path_centroid;
    
    Sym_PathBounds              m_path_bounds;
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathBaseComponent)
};

