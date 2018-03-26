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
    static int compareElements ( const Symbol* a, const Symbol* b)
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
    Score( const OdotBundle_s& s_bundle ) ;
    ~Score();
    
    size_t getSize();
    
    Symbol *getSymbol(int n);    
    int getSymbolPosition(Symbol *s);
    
    void addSymbol(Symbol *s);
    void removeSymbol(Symbol *s);
    void removeAllSymbols();
    
    Symbol * lookupSymbolID( const String & id );
    void updateExistingScoreSymbol( Symbol * dst, Symbol * src );
    
    void importScoreFromOSC( const OdotBundle_s& s_bundle );
    
    //    void sortScore();
    
    void print() const;
    
    void addSymbolTimePoints( Symbol *s );
    void removeSymbolTimePoints( Symbol *s );
    
    OdotBundle_s getSymbolsAtTime( float t );

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

    OdotBundle_s getDurationBundle();

    
    int getNameCount( string& name )
    {
        int count = 0;
        for( Symbol *s : score_symbols )
        {
            s->print();
            if( s != NULL && s->getName() == name )
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
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (Score)
};








