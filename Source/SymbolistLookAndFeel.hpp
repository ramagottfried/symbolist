#pragma once

#include "../JuceLibraryCode/JuceHeader.h"

class SymbolistLookAndFeel : public LookAndFeel_V4
{
public:
    
    SymbolistLookAndFeel()
    {
        LookAndFeel_V4::LookAndFeel_V4();
        
        // not sure why this has to be set twice, once here and again in the OSC property components ? weird
        
        setColour( PropertyComponent::backgroundColourId, Colours::transparentWhite );
        setColour( PropertyComponent::labelTextColourId, Colours::black );
        
        setColour(Label::textWhenEditingColourId, Colours::black  );
        setColour(Label::backgroundWhenEditingColourId, Colours::transparentWhite  );

        setColour(ComboBox::textColourId, Colours::black );
        setColour(ComboBox::backgroundColourId, Colours::transparentWhite );
        setColour(ComboBox::arrowColourId, Colours::black );
        
        setColour(PopupMenu::textColourId, Colours::black );
        setColour(PopupMenu::backgroundColourId, Colour::fromFloatRGBA(0.9, 0.9, 0.9, 1) );

        
        // these don't have an effect due to needing to be set in the TextProp comp
        
        //setColour(TextPropertyComponent::textColourId , Colours::black );
        //setColour(TextPropertyComponent::backgroundColourId, Colours::transparentWhite );

        //setColour( ScrollBar::backgroundColourId, Colours::yellow  );
        //setColour( ScrollBar::trackColourId, Colours::blue  );
        setColour( ScrollBar::thumbColourId, Colour::fromFloatRGBA(0., 0., 0., 0.4)  );
        
        
    }
    ~SymbolistLookAndFeel() = default;
    
    
    void drawResizableFrame (Graphics& g, int w, int h, const BorderSize<int>& border) override
    {
        if (! border.isEmpty() )
        {
            const Rectangle<int> b (0, 0, w, h);
            
            g.saveState();
            
            g.setColour ( Colours::cornflowerblue );
            
            const float dashLengths[2] = {1,2};
            int numDashLengths = 2;
            
            
            g.drawDashedLine( Line<float>(  b.getTopLeft().x, b.getTopLeft().y,
                                            b.getTopRight().x, b.getTopRight().y ),
                                            dashLengths, numDashLengths);

            g.drawDashedLine( Line<float>(  b.getTopRight().x, b.getTopRight().y,
                                            b.getBottomRight().x, b.getBottomRight().y ),
                                            dashLengths, numDashLengths);
            
            g.drawDashedLine( Line<float>(  b.getBottomRight().x, b.getBottomRight().y,
                                            b.getBottomLeft().x, b.getBottomLeft().y ),
                                            dashLengths, numDashLengths);
            
            g.drawDashedLine( Line<float>(  b.getBottomLeft().x, b.getBottomLeft().y,
                                            b.getTopLeft().x, b.getTopLeft().y ),
                                            dashLengths, numDashLengths);
            
            
            g.restoreState();
        }
    }
    
    
    void drawScrollbar (Graphics& g, ScrollBar& scrollbar, int x, int y, int width, int height,
                                        bool isScrollbarVertical, int thumbStartPosition, int thumbSize, bool isMouseOver, bool isMouseDown) override
    {
        ignoreUnused (isMouseDown);
        
        Rectangle<int> thumbBounds;
        
        if (isScrollbarVertical)
            thumbBounds = { x, thumbStartPosition, width, thumbSize };
        else
            thumbBounds = { thumbStartPosition, y, thumbSize, height };
        
        const auto c = scrollbar.findColour (ScrollBar::ColourIds::thumbColourId);
        g.setColour (isMouseOver ? c.brighter (0.25f) : c);
        g.fillRoundedRectangle (thumbBounds.reduced (1).toFloat(), 4.0f);
    }
    
    int getMinimumScrollbarThumbSize (ScrollBar& scrollbar) override
    {
        return jmin (scrollbar.getWidth(), scrollbar.getHeight()) * 2;
    }
    
private:
    
    
};
