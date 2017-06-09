#pragma once

#include "BaseComponent.h"

class CircleComponent : public BaseComponent
{
public:

    CircleComponent(const Symbol &s);
    ~CircleComponent()
    {
        printf("freeing circle %p\n", this);
    }
    
    float symbol_getX() override { return getX() + ( getWidth() * .5) ; }
    float symbol_getY() override { return getY() + ( getHeight() * .5) ; }

    void paint ( Graphics& g ) override;
    void resized() override;
    
<<<<<<< Updated upstream
    void addHandle( float x, float y );
    void makeHandles();
    void removeHandles();
    void updatePathPoints();
    
    void deselectComponent () override;

    void mouseDown( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;
    
=======
    void importFromSymbol() override;

>>>>>>> Stashed changes
    
private:
    Path            m_path;
    PathStrokeType  strokeType = PathStrokeType (2.0);
    
    std::vector<PathHandle*> path_handles;

    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CircleComponent)
};
