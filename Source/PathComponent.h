#pragma once

#include "BaseComponent.h"

class PathHandle;


class PathComponent : public BaseComponent
{
public:
    
    PathComponent(  float x, float y,
                    float w = 10, float h = 10,
                    float stroke = 2,
                    Colour color = Colours::black ) :
        BaseComponent("path" , x , y , w , h, stroke, color ),
        strokeType(1.0)
        {};
    
    ~PathComponent();
    
    void printPath( Path p );
    
    int addSymbolMessages(const String &base_address) override;
    
    void importFromSymbol() override;

    void addHandle( float x, float y, int index);
    void makeHandles();
    void removeHandles();
    void updatePathPoints();
    
    void deselectComponent () override;
    
    void symbol_paint ( Graphics& g ) override;
    
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;
    
private:
    
    Point<float>    m_drag;
    PathStrokeType  strokeType;
    Path            m_path;
    
    std::vector<PathHandle*> path_handles;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathComponent)
};


class PathHandle : public BaseComponent
{
public:
    PathHandle( float x, float y, PathComponent *pc ) : BaseComponent("UI_only", x, y )
    {
        m_path = pc;
        setComponentID("handle");
        float halfsize = m_size * 0.5;
        setBounds( x-halfsize, y-halfsize, m_size, m_size);
    }
    
    ~PathHandle()
    {
        printf("freeing Path %p\n", this);
    }
    
    void symbol_paint ( Graphics& g ) override
    {
        g.setColour ( current_color );
        const Rectangle<float> bounds = getLocalBounds().toFloat().reduced( strokeWeight );
        g.drawRect ( bounds, (float) strokeWeight );
    }
    
    void mouseDown( const MouseEvent& event ) override
    {
        BaseComponent::mouseDown(event);
    }
    
    void mouseDrag( const MouseEvent& event ) override
    {
        
        // not sure why I need to make this relative, it was jumping back and forth between being relative to the score and then to the component. is it possibe that it has an extra mouselistener somewhere?
        Point<int> draggy = event.getEventRelativeTo( getPageComponent() ).getPosition();
        setTopLeftPosition ( draggy - (m_down).toInt() );
        m_path->updatePathPoints();
    }
    void mouseUp( const MouseEvent& event ) override
    {}
    
private:
    
    PathComponent   *m_path;
    float           m_size = 10;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PathHandle)
    
};
