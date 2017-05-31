#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "PrimitiveIncludes.h"
#include "SymbolistComponent.h"

/**************************************
 * Non-graphic object : can be handle by the SymbolistMainComponent
 * even if the palette is not displayed in the window
 **************************************/
class SymbolistPalette
{
public:
    
    SymbolistPalette(){};
    ~SymbolistPalette(){ for (int i = 0; i < items.size(); i++) delete items[i] ; }
    
    void addPaletteItem( BaseComponent *c ) { items.emplace_back(c); }
    BaseComponent* getPaletteItem( int i ) { return items[i] ; }
    int getPaletteNumItems() { return static_cast<int>( items.size() ) ; }
    int getSelectedItem() { return selected_item ; }
    void setSelectedItem(int n) { selected_item = n; }
    
    private :
    
    std::vector<BaseComponent*> items;
    int selected_item = 0;
};




/*************************************
 * A graphical component of the palette.
 **************************************/
class PaletteButton : public SymbolistComponent {

    friend class SymbolistMainComponent;
    
public:
    
    PaletteButton(int i) { button_id = i ; }
    ~PaletteButton() {}
    
    void setSelected(bool sel);
    void paint(Graphics &g) override;
    void mouseDown ( const MouseEvent& event ) override;
    
private:
    
    int button_id;
    bool selected;
    
};


/**************************************
 * Graphic view for the palette.
 **************************************/
class PaletteComponent : public SymbolistComponent
{
    
    public:
    
    PaletteComponent();
    ~PaletteComponent();
    
    void buildFromPalette(SymbolistPalette *palette);
    void selectPaletteButton(int i);
    BaseComponent* getPaletteItem(int i);
    
    void paint (Graphics& g) override;
    
    private:
    
    SymbolistPalette* palette_pointer;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (PaletteComponent)

};
