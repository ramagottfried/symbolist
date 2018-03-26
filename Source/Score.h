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
    static int compareElements (const Symbol* a, const Symbol* b)
    {
        auto a_t = a->getTime();
        auto b_t = b->getTime();
        return (a_t < b_t ? -1 : (a_t == b_t ? 0 : 1));
    }
};

class Score
{
    OwnedArray<Symbol> score_symbols;
    TimePointArray     time_points;
    SortedStaves       staves;
    ScoreSorter        score_sorter;
    
public:
    
    Score();
    Score( Score& src );
    Score( int n, odot_bundle** bundle_array ) ;
    ~Score();
    
    size_t getSize();
    
    Symbol *getSymbol(int n);    
    int getSymbolPosition(Symbol *s);
    
    void addSymbol(Symbol *s);
    void removeSymbol(Symbol *s);
    void removeAllSymbols();
    
    Symbol * lookupSymbolID( const String & id );
    void updateExistingScoreSymbol( Symbol * dst, Symbol * src );
    
    void importScoreFromOSC( int n, odot_bundle** bundle_array );
    
    //    void sortScore();
    
    void addSymbolTimePoints( Symbol *s );
    void removeSymbolTimePoints( Symbol *s );
    
    odot_bundle *getSymbolsAtTime( float t );

    odot_bundle *getScoreBundle();

    const Array<Symbol*> getSymbolsByValue( const String& address, const String& value );

    const TimePointArray* getTimePointArray() const { return &time_points; }
    
    const StringArray getStaves();
    void addStaff( Symbol *s );
    
    void updateStaves(Symbol *moved_stave);
    void updateStavesAndTimepoints();
    Symbol *getStaveAtTime( float time );
    const Symbol* getStaveByID( const String& id );

    odot_bundle* getDurationBundle();

    
    int getNameCount( String& name )
    {
        int count = 0;
        for( Symbol *s : score_symbols )
        {
            if( s->getName() == name )
                count++;
        }
    
        return count;
    }
    
    bool idExists( String& idStr )
    {
        for( Symbol *s : score_symbols )
        {
            if( s->getID() == idStr )
                return true;
        }
        
        return false;
    }
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (Score)
};








