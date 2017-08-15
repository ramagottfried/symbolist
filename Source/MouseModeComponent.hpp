#pragma once

#include "SymbolistComponent.h"

class MouseModeComponent : public SymbolistComponent
{
public:
    MouseModeComponent(){}
    ~MouseModeComponent(){}
    
    void paint( Graphics &g ) override;
    
    void setMouseMode( UI_EditType t );
    void setDrawMode( UI_DrawType t );
    
    void drawString();
    
    bool hitTest (int x, int y) override { return false; }
    
private:
    
    String m_str;
    
    UI_DrawType m_drawType;
    UI_EditType m_editType;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MouseModeComponent)

};
