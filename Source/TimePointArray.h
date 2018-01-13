#pragma once

#include "Symbol.h"

//============================
// SYMBOL Timepoint
//============================

using namespace std;

class Score;

struct SymbolTimePoint
{
    SymbolTimePoint(Symbol * s, double t, Symbol *staff)
    {
        addSymbol( s );
        time = t;
        staff_ref = staff;
    }
    /*
    SymbolTimePoint(SymbolTimePoint& src) // same as = default?
    {
        time = src.time;
        staff_ref = src.staff_ref;
        symbols_at_time = src.symbols_at_time;
    }
    */
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
    
    double           time;
    vector<Symbol*> symbols_at_time;
    Symbol *        staff_ref; // << add reference to staff for timepoint (a timepoint can only be on one staff)

    
private:
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolTimePoint)
    
};

class TimePointArray : public OwnedArray<SymbolTimePoint>
{
public:
    TimePointArray(Score *s)
    {
        score_ptr = s;
    }
    
    /*
    TimePointArray(TimePointArray& t)
    {
        score_ptr = t.score_ptr;
        for( auto tpoint : t )
        {
            add( new SymbolTimePoint( *tpoint ) );
        }
    }
    
    TimePointArray& operator=(TimePointArray& other)
    {
        if (this != &other) // protect against invalid self-assignment
        {
            score_ptr = other.score_ptr;
            for( auto tpoint : other )
            {
                add( new SymbolTimePoint( *tpoint ) );
            }
        }
        
        return *this;
    }
    */
    ~TimePointArray() = default;
    
    void printTimePoints();
    void printBundle(OSCBundle bndl);
    
    int getTimePointInsertIndex( float t, bool& match );
    
    void addSymbolTimePoints( Symbol *s );
    void removeSymbolTimePoints( Symbol *s);
    void removeStaffAndSymbolTimePoints( Symbol *s);

    int addSymbol_atTime( Symbol *s, float t, Symbol *staff);
    
    inline bool f_almost_equal(float x, float y, int ulp = 2)
    {
        // the machine epsilon has to be scaled to the magnitude of the values used
        // and multiplied by the desired precision in ULPs (units in the last place)
        return std::abs(x-y) < std::numeric_limits<float>::epsilon() * std::abs(x+y) * ulp
        // unless the result is subnormal
        || std::abs(x-y) < std::numeric_limits<float>::min();
    }
    
    inline int compareTimes( float a_t, float b_t )
    {
        return ( a_t < b_t ? 1 : ( f_almost_equal(a_t, b_t) ? 0 : -1 ) );
    }

    odot_bundle *getSymbolsAtTime( float t );
    odot_bundle *timePointStreamToOSC(const SymbolTimePoint *tpoint);
    
    int lookupTimePoint( float t );
    Point<float> lookupPathPoint( const Symbol *s, const float t );
    Point<float> lookupPathPoint( const Symbol *s, const int pathIDX, const float t, const float start, const float dur );
    Point<float> lookupPathPoint( const Symbol *s, String& path_base_addr , const float t );

    void groupLookup( const Symbol *s,
                                     const String& output_prefix,
                                     const String& groupsymbol_addr,
                                     double parent_x,
                                     double parent_y,
                                     float time_ratio,
                                     OSCBundle& bndl);
    
    vector<const Symbol *> getNoteOffs( const SymbolTimePoint *prev_tpoint , const SymbolTimePoint *tpoint   );
    bool isNewSym( const Symbol *s , const SymbolTimePoint *prev_tpoint   );
    
    pair<size_t, int> getVoiceNumberState( const Symbol *s, const SymbolTimePoint *tpoint );
    pair<size_t, int> setNoteOff( const Symbol *s);
    vector< tuple<size_t, const Symbol*, const Symbol*> > getNoteOffs( const SymbolTimePoint *p );

    void resetTimes();

    inline void reset()
    {
        clear();
        prev_timepoint = nullptr;
    }

    
private:
    
    int current_point = 0;
    float current_time = 0;
    
    Score   *score_ptr = nullptr;
    
    const SymbolTimePoint*      prev_timepoint = NULL;
    
    vector< pair<const Symbol*,const Symbol*>>  voice_staff_vector;


    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimePointArray)
};
