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
    
    ~SymbolTimePoint(){ cout << "deleting timepoint " << time << endl; }
    
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
    
    float           time;
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
    void printBundle(OSCBundle bndl);
    
    int getTimePointInsertIndex( float t, bool& match );
    
    void addSymbolTimePoints( Symbol *s );
    void removeSymbolTimePoints( Symbol *s);

    int addSymbol_atTime( Symbol *s, float t);
    
    bool f_almost_equal(float x, float y, int ulp)
    {
        // the machine epsilon has to be scaled to the magnitude of the values used
        // and multiplied by the desired precision in ULPs (units in the last place)
        return std::abs(x-y) < std::numeric_limits<float>::epsilon() * std::abs(x+y) * ulp
        // unless the result is subnormal
        || std::abs(x-y) < std::numeric_limits<float>::min();
    }
    
    // f_almost_equal(a_t, b_t, 2)
    inline int compareTimes( float a_t, float b_t )
    {
        return ( a_t < b_t ? 1 : ( a_t == b_t ? 0 : -1 ) );
    }

    odot_bundle *getSymbolsAtTime( float t );
    odot_bundle *timePointStreamToOSC(const SymbolTimePoint *tpoint);
    
    int lookupTimePoint( float t );
    Point<float> lookupPathPoint( const Symbol *s, const float t );
    Point<float> lookupPathPoint( const Symbol *s, const int pathIDX, const float t, const float start, const float dur );
    
private:
    
    int current_point = 0;
    float current_time = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimePointArray)
};
