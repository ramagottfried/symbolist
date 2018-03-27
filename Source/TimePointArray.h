#pragma once

#ifndef TimePointArray_h
#define TimePointArray_h

#include "Symbol.h"

//============================
// SYMBOL Timepoint
//============================

using namespace std;

class Score;

struct SymbolTimePoint
{
    double                      time;
    vector<shared_ptr<Symbol> > symbols_at_time;
    shared_ptr<Symbol>          staff_ref; // << add reference to staff for timepoint (a timepoint can only be on one staff)
    
    SymbolTimePoint(shared_ptr<Symbol> s, double t, shared_ptr<Symbol> staff)
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
    
    void removeSymbol(shared_ptr<Symbol> s)
    {
        symbols_at_time.erase( remove(symbols_at_time.begin(), symbols_at_time.end(), s ), symbols_at_time.end() );
        
        if( symbols_at_time.size() == 0 )
            delete this;
    }
    
    void addSymbol(shared_ptr<Symbol> s)
    {
        symbols_at_time.emplace_back(s);
    }
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolTimePoint)
    
};

class TimePointArray : public vector<shared_ptr<SymbolTimePoint> >
{
    int                                current_point = 0;
    float                              current_time = 0;
    shared_ptr<Score>                  score_ptr = nullptr;
    shared_ptr<SymbolTimePoint>        prev_timepoint = nullptr;
    vector<pair<shared_ptr<Symbol>,
                shared_ptr<Symbol>> >  voice_staff_vector;
    
public:
    inline TimePointArray() {}
    TimePointArray(Score* s);
    
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
    
    int getTimePointInsertIndex(float t, bool& match);
    
    void addSymbolTimePoints(shared_ptr<Symbol> s);
    void removeSymbolTimePoints(shared_ptr<Symbol> s);
    void removeStaffAndSymbolTimePoints( shared_ptr<Symbol> s);

    int addSymbol_atTime(shared_ptr<Symbol> s, float t, shared_ptr<Symbol> staff);
    
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

    OdotBundle_s getSymbolsAtTime(float t);
    OdotBundle_s timePointStreamToOSC(const shared_ptr<SymbolTimePoint> tpoint);
    
    int lookupTimePoint(float t);
    Point<float> lookupPathPoint(const shared_ptr<Symbol> s, const float t);
    Point<float> lookupPathPoint(const shared_ptr<Symbol> s, const int pathIDX, const float t, const float start, const float dur);
    Point<float> lookupPathPoint(const shared_ptr<Symbol> s, string& path_base_addr , const float t);
    
    /**
     * @param s                main root symbol (not subbundle)
     *
     * @param output_prefix    prefix to be added to this level (previous level
     *                         prefix + group name culled from the calling function)
     *
     * @param time_ratio       toplevel group time point
     *
     * @param bndl             toplevel bundle to write into
     *
     * All paths within group are read in terms of the time span of the top level group.
     * Paths are scaled in terms of their bounding box, *or* if the bounding box of the group containing them
     * --- in the case of path within a group within a group, the scaling would be in terms of the first containing group.
     * For example subsymbol_addr could be "/subsymbol/1/subsymbol/2".
     */
    void groupLookup(const shared_ptr<Symbol> s,
                     const string& output_prefix,
                     double parent_x,
                     double parent_y,
                     float time_ratio,
                     OdotBundle& bndl);
    
    vector<const shared_ptr<Symbol> > getNoteOffs(const shared_ptr<SymbolTimePoint> prev_tpoint , const shared_ptr<SymbolTimePoint> tpoint);
    bool isNewSym(const shared_ptr<Symbol> s , const shared_ptr<SymbolTimePoint> prev_tpoint);
    
    pair<size_t, int> getVoiceNumberState(const shared_ptr<Symbol> s, const shared_ptr<SymbolTimePoint> tpoint);
    pair<size_t, int> setNoteOff(const shared_ptr<Symbol> s);
    vector<tuple<size_t, const shared_ptr<Symbol>, const shared_ptr<Symbol>> > getNoteOffs(const shared_ptr<SymbolTimePoint> p);

    void resetTimes();

    inline void reset()
    {
        clear();
        prev_timepoint = nullptr;
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimePointArray)
};

#endif
