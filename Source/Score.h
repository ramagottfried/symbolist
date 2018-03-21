#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>
#include "types.h"
#include "Symbol.h"
#include "TimePointArray.h"
#include "SortedStaves.hpp"

using namespace std;

//============================
// SCORE
//============================

struct ScoreSorter
{
    static int compareElements ( Symbol* a, Symbol* b)
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
    Score( Score& src );
    Score( int n, t_osc_bndl_s** bundle_array ) ;
    ~Score();
    
    size_t getSize();
    
    Symbol *getSymbol(int n);    
    int getSymbolPosition(Symbol *s);
    
    void addSymbol(Symbol *s);
    void removeSymbol(Symbol *s);
    void removeAllSymbols();
    
    void importScoreFromOSC( int n, t_osc_bndl_s** bundle_array );
    
    //    void sortScore();
    
    void addSymbolTimePoints( Symbol *s );
    void removeSymbolTimePoints( Symbol *s );
    
    odot_bundle *getSymbolsAtTime( float t );

    //odot_bundle *getScoreBundle();
    
    OdotBundle_s getScoreBundle_s();

    const Array<Symbol*> getSymbolsByValue( const string& address, const string& value );

    const TimePointArray* getTimePointArray() const { return &time_points; }
    
    const StringArray getStaves();
    void addStaff( Symbol *s );
    
    void updateStaves(Symbol *moved_stave);
    void updateStavesAndTimepoints();
    Symbol *getStaveAtTime( float time );
    const Symbol* getStaveByID( const string& id );

    odot_bundle* getDurationBundle();

    
    int getNameCount( string& name )
    {
        int count = 0;
        for( Symbol *s : score_symbols )
        {
            if( s->getName() == name )
                count++;
        }
    
        return count;
    }
    
    bool idExists( string& idStr )
    {
        for( Symbol *s : score_symbols )
        {
            if( s->getID() == idStr )
                return true;
        }
        
        return false;
    }
    
private:
    
    OwnedArray<Symbol>          score_symbols;
    
    OdotBundle                  score_bundle;
    
    TimePointArray              time_points;
    SortedStaves                staves;
    
    ScoreSorter                 score_sorter;
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (Score)
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
//        cout << "~SymbolistPalette" << this <<  endl;
    }

    void addDefaultItem( Symbol *s ) { default_items.add(s); }
    void addUserItem( Symbol *s ) { user_items.add(s); }
    Symbol* getPaletteDefaultItem( int i ) { return default_items[i] ; }
    Symbol* getPaletteUserItem( int i ) { return user_items[i] ; }
    int getPaletteNumDefaultItems() { return static_cast<int>( default_items.size() ) ; }
    int getPaletteNumUserItems() { return static_cast<int>( user_items.size() ) ; }
    int getSelectedItem() { return selected_item ; }
    void setSelectedItem(int n) { selected_item = n; }
    
private :
    
    OwnedArray<Symbol> default_items;
    OwnedArray<Symbol> user_items;
    
    int selected_item = 0;
};


