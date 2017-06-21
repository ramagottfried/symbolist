#pragma once

#include "Symbol.h"

//============================
// SYMBOL Timepoint
//============================

using namespace std;

struct SymbolTimePoint
{
    SymbolTimePoint(Symbol * s, float t)
    {
        addSymbol( s );
        time = t;
        
    }
    
    void removeSymbol( Symbol * s)
    {
        symbols_at_time.erase( remove(symbols_at_time.begin(), symbols_at_time.end(), s ), symbols_at_time.end() );
        
        if( symbols_at_time.size() == 0 )
            delete this;
    }
    
    void addSymbol( Symbol * s )
    {
        symbols_at_time.emplace_back( s );
    }
    
    float time;
    vector<Symbol*> symbols_at_time;
    
private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolTimePoint)
    
};


class TimePointArray : public OwnedArray<SymbolTimePoint>
{
public:
    TimePointArray() = default;
    ~TimePointArray() = default;
    
    void printTimePoints();
    
    int getTimePointIndex( float t, bool& match );
    
    void addSymbolTimePoints( Symbol *s );
    void addSymbol_atTime( Symbol *s, float t);
    
    inline int compareTimes( float a_t, float b_t )
    {
        return (a_t < b_t ? 1 : (a_t == b_t ? 0 : -1));
    }

    odot_bundle *getSymbolsAtTime( float t );
    odot_bundle * symbolVectorToOSC( const vector<Symbol*> vec );

    
private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimePointArray)
};
