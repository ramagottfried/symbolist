#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>
#include "types.h"
#include "Symbol.h"


using namespace std;


//============================
// SCORE
//============================

class Score
{
    
public:

    Score();
    Score( int n, odot_bundle** bundle_array ) ;
    ~Score();

    size_t getSize();
    Symbol *getSymbol(int n);
    int getSymbolPosition(Symbol *s);

    void addSymbol(Symbol *s);
    void removeSymbol(Symbol *s);
    void removeAllSymbols();

    void importScoreFromOSC( int n, odot_bundle** bundle_array );

//    void sortScore();
    
private:

    OwnedArray<Symbol>  score_symbols;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Score)
};



/**************************************
 * Non-graphic object : can be handle by the SymbolistMainComponent
 * even if the palette is not displayed in the window
 **************************************/
class SymbolistPalette
{
public:
    
    SymbolistPalette(){};
    ~SymbolistPalette(){ for (int i = 0; i < items.size(); i++) delete items[i] ; }
    
    void addPaletteItem( Symbol *c ) { items.emplace_back(c); }
    Symbol* getPaletteItem( int i ) { return items[i] ; }
    int getPaletteNumItems() { return static_cast<int>( items.size() ) ; }
    int getSelectedItem() { return selected_item ; }
    void setSelectedItem(int n) { selected_item = n; }
    
    private :
    
    std::vector<Symbol*> items;
    int selected_item = 0;
};


