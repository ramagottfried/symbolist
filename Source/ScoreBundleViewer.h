

#pragma once
#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"


class ScoreBundleViewer : public Component
{
public:
    ScoreBundleViewer(SymbolistHandler *sh)
    {
        symbolist_handler = sh;
    }
    
    ~ScoreBundleViewer(){}
    
    void paint (Graphics& g) override
    {
        g.setColour(Colours::black);
        
        int num = symbolist_handler->symbolistAPI_getNumSymbols();
        
        int x = 20;
        int y = 20;
        int y_inr = 15;
        
        for( int i = 0; i < num; i++ )
        {
            StringArray strarray = symbolist_handler->symbolistAPI_getSymbolString( i );
            for( int j = 0; j < strarray.size(); j++ )
            {
                g.drawText(strarray[j], x, y, 400, 20, Justification::topLeft );
                y += y_inr;
            }
        }
        
        setSize(400, y+40);
    }

private:
    SymbolistHandler*   symbolist_handler;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreBundleViewer)
};



class ScoreBundleViewPort : public Component
{
public:
    ScoreBundleViewPort(SymbolistHandler *sh) : bundle_comp(sh)
    {
        bundle_viewport.setViewedComponent( &bundle_comp, false );
        bundle_viewport.setFocusContainer ( true );
        bundle_viewport.setScrollBarsShown(true, true);
     
        bundle_comp.setSize(400, 5000);
        addAndMakeVisible(bundle_viewport);
     //   addAndMakeVisible(bundle_comp);

        setSize (400, 600);
    }

    void resized() override
    {
        
        bundle_viewport.setBounds( 0, 0, getParentWidth(), getParentHeight() );
    }
    
private:
    SymbolistHandler*                   symbolist_handler;
    ScoreBundleViewer                   bundle_comp;
    Viewport                            bundle_viewport;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ScoreBundleViewPort)
};



