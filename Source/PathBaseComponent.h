
#pragma once

#include "BaseComponent.h"
#include "PathHandleComponent.h"

class PathBaseComponent : public BaseComponent
{
public:
    
    PathBaseComponent(  const Symbol& s );
    ~PathBaseComponent() ;
    
    String getSymbolTypeStr() const override { return "path"; }

    void printPath( Path p, const char* name = "path" );
    
    int addSymbolMessages(Symbol* s, const String &base_address) override;
    
    void importFromSymbol(const Symbol &s) override;
    
    void addHandle( int type, float x, float y );
    void makeHandles();
    void removeHandles();
    void updatePathPoints();
    void drawHandles( Graphics& g);
    
    void deselectComponent () override;
    void selectComponent () override;

    void paint ( Graphics& g ) override;
    void resized () override;
    void h_flip() override;
    void v_flip() override;

    void mouseDown( const MouseEvent& event ) override;
    void mouseMove( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;
    
    Rectangle<float> tranformAndGetBoundsInParent( Path& p );
    
    void notifyEditModeChanged( UI_EditType current_mode ) override;
    virtual void componentCretated() override {}
    
    void enterPathEdit ();
    void exitPathEdit ();
    void updatePathFromPreivew ();
    
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
    bool                        in_edit_mode = false;
    
    Point<float>                m_prev_drag;
    
private:
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathBaseComponent)
};

