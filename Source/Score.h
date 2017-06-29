#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>
#include "types.h"
#include "Symbol.h"
#include "TimePointArray.h"

using namespace std;

//============================
// SCORE
//============================

struct ScoreSorter
{
    static int compareElements (const Symbol* a, const Symbol* b)
    {
        auto a_t = a->getTime();
        auto b_t = b->getTime();
        return (a_t < b_t ? -1 : (a_t == b_t ? 0 : 1));
    }
};

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
    
    void addSymbolTimePoints( Symbol *s );
    void removeSymbolTimePoints( Symbol *s );
    
    odot_bundle *getSymbolsAtTime( float t );

    odot_bundle *getScoreBundle();

    
    const TimePointArray* getTimePointArray() const { return &time_points; }
    
private:
    
    OwnedArray<Symbol>          score_symbols;
    TimePointArray              time_points;
    
    ScoreSorter                 score_sorter;

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
    ~SymbolistPalette()
    {
        for (int i = 0; i < default_items.size(); i++) delete default_items[i] ;
        for (int i = 0; i < user_items.size(); i++) delete user_items[i] ;
    }

    void addDefaultItem( Symbol *s ) { default_items.emplace_back(s); }
    void addUserItem( Symbol *s ) { user_items.emplace_back(s); }
    Symbol* getPaletteDefaultItem( int i ) { return default_items[i] ; }
    Symbol* getPaletteUserItem( int i ) { return user_items[i] ; }
    int getPaletteNumDefaultItems() { return static_cast<int>( default_items.size() ) ; }
    int getPaletteNumUserItems() { return static_cast<int>( user_items.size() ) ; }
    int getSelectedItem() { return selected_item ; }
    void setSelectedItem(int n) { selected_item = n; }
    
    private :
    
    std::vector<Symbol*> default_items;
    std::vector<Symbol*> user_items;
    
    int selected_item = 0;
};


