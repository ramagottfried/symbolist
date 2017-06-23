
#pragma once

#include "BaseComponent.h"

class TextGlphComponent : public BaseComponent
{
public:
    TextGlphComponent() = default;
    ~TextGlphComponent() = default;
    
    void importTextFromSymbol( const Symbol& s ) {}
    
    void paint( Graphics& g ) override
    {
        g.drawText( m_text, getLocalBounds(), Justification::topLeft );
    }
    
    String getSymbolTypeStr() const override { return "text"; }

private:
    
    String      m_text = "text";
    Font        m_font;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TextGlphComponent)
    
};
