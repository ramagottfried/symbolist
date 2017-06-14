
#pragma once

#include "BaseComponent.h"

class PathHandle;

class PathBaseComponent : public BaseComponent
{
public:
    
    PathBaseComponent(  const Symbol& s );
    ~PathBaseComponent() ;
    
    String getSymbolTypeStr() const override { return "path"; }

    void printPath( Path p, const char* name = "path" );
    
    int addSymbolMessages(Symbol* s, const String &base_address) override;
    
    void importPathFromSymbol(const Symbol &s) ;
    
    void addHandle( float x, float y );
    void makeHandles();
    void removeHandles();
    void updatePathPoints();
    void drawHandles( Graphics& g);
    
    void deselectComponent () override;
    void selectComponent () override;

    void paint ( Graphics& g ) override;
    
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
        if( getMainEditMode() == draw_mode)
            return true;
        
        return m_path.intersectsLine( Line<float>( x - 5, y - 5, x + 5, y + 5) ) || m_path.intersectsLine( Line<float>( x + 5, y - 5, x - 5, y + 5) );
    }

protected:
        
    PathStrokeType          strokeType = PathStrokeType(2.0) ;
    Path                    m_path;
    Path                    m_preview_path;
    Point<float>            m_path_origin;

    std::vector<PathHandle*> path_handles;
    
    bool                    in_edit_mode = false;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathBaseComponent)
};


class PathHandle : public Component
{
public:
    PathHandle( float x, float y, PathBaseComponent *pc)
    {
        m_path = pc;
        setComponentID("path_handle");
        float halfsize = m_size * 0.5;
        setBounds( x-halfsize, y-halfsize, m_size, m_size);
    }
    
    ~PathHandle()
    {
//        std::cout << "freeing " << getComponentID() << " " << this << "\n";
    }
    
    void paint ( Graphics& g ) override
    {
        g.setColour ( Colours::cornflowerblue );
        const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( m_strokeweight );
        g.drawRect ( bounds, (float) m_strokeweight );
    }
    
    void mouseDown( const MouseEvent& event ) override
    {
        m_down = event.position;
    }
    
    void mouseDrag( const MouseEvent& event ) override
    {
        
        // not sure why I need to make this relative, it was jumping back and forth between being relative to the score and then to the component. is it possibe that it has an extra mouselistener somewhere?
        Point<int> draggy = event.getEventRelativeTo( m_path->getPageComponent() ).getPosition();
        setTopLeftPosition ( draggy - (m_down).toInt() );
        m_path->updatePathPoints();
    }
    void mouseUp( const MouseEvent& event ) override
    {}
    
private:
    
    Point<float>        m_down;
    
    PathBaseComponent   *m_path;
    float               m_size = 10;
    float               m_strokeweight = 1;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathHandle)
    
};
