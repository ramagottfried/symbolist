#pragma once
#include "OdotBundle.hpp"
#include <cmath>

//============================
// SYMBOL Timepoint
//============================

using namespace std;

class Score;

struct SymbolTimePoint
{
    inline SymbolTimePoint() {}
    inline SymbolTimePoint(OdotBundle& s, double t, OdotBundle& staff)
    {
        addSymbol(s);
        time = t;
        staff_ref = staff;
    }
	
    inline void addSymbol(OdotBundle& s)
    {
        symbols_at_time.emplace_back(s);
    }
    
    int size() const { return symbols_at_time.size(); }
    
    double              time;
    vector<OdotBundle>  symbols_at_time;
    OdotBundle          staff_ref; // << add reference to staff for timepoint (a timepoint can only be on one staff)

};

class TimePointArray
{
public:
    
    //TimePointArray(const Score& ref_score) : score(ref_score) {}
    //~TimePointArray(){}
    
	void reset()
    {
        symbol_time_points.clear();
        voice_staff_vector.clear();
        
        m_duration = 0;
        current_time = 0;
        
        current_point = 0;
        prev_point = 0;
    }
    
	
    /**
     * Adds symbol to time point array, with staff reference.
     *
     * @param symbol      Symbol to insert into TimePointArray
     *
     * @param staff       Staff reference for Symbol
     *
     * @return            the index of the time point for the start of the symbol
     */
    int addSymbol(OdotBundle& symbol, OdotBundle& staff);

    /**
     * Retrieves a bundle of active symbols at the lookup time. (pre-evaluation of expressions)
     *
     * @param t       time to lookup (float)
     *
     * @return        OdotBundle_s, serialized OSC bundle for output
     */
    OdotBundle getSymbolsAtTime(float t);
    
    /**
     * Retrieves a bundle of active symbols at the lookup time. (pre-evaluation of expressions)
     *
     * @param tpoint   SymbolTimePoint to output
     *
     * @return        OdotBundle_s, serialized OSC bundle for output
     */
    OdotBundle timePointStreamToOSC(const SymbolTimePoint& tpoint);
    
    
    int lookupTimePoint(float t);
    
    
    // should pathpoint lookups be in the score, clef, or stave? or here?
    vector<float> lookupPathPoint(const OdotBundle& symbol, const float t);
    vector<float> lookupPathPoint(const OdotBundle& symbol, const int pathIDX, const float t, const float start, const float dur);
    vector<float> lookupPathPoint(const OdotBundle& symbol, string& path_base_addr , const float t);
    
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
    void groupLookup(const OdotBundle& s,
                     const string& output_prefix,
                     double parent_x,
                     double parent_y,
                     float time_ratio,
                     OdotBundle& bndl);
    
    
    
    
    vector< const OdotBundle >
    getNoteOffs(const SymbolTimePoint& prev_tpoint , const SymbolTimePoint& tpoint);
    
    pair<size_t, int>
    getVoiceNumberState(const OdotBundle& symbol, const SymbolTimePoint& tpoint);
    
    pair<size_t, int>
    setNoteOff(const OdotBundle& symbol);
    
    typedef tuple<size_t, const OdotBundle, const OdotBundle> ID_SYM_STAFF;
    
    vector<ID_SYM_STAFF> getNoteOffs(const SymbolTimePoint& p);

    
    
    /**
     * Gets total time for score.
     *
     * @return float, duration of full score
     */
    inline double getTotalDuration()
    {
        // calculate time...
        return m_duration;
    }
    
    /**
     * Says if a symbol is referenced in a time point.
     *
     * @param symbol    the symbol to search in the time point.
     *
     * @param timePoint the time point in which to look for the symbol.
     *
     * @return          <code>true</code> if symbol is referenced in
     *                    timePoint, <code>false</code> otherwise.
     */
    bool isSymbolInTimePoint( OdotBundle& symbol, const SymbolTimePoint& timePoint);
    
    inline bool timeHitTest( OdotBundle& symbol, float t )
    {
        float start = symbol.getMessage("/time/start").getFloat();
        return t >= start && t <= symbol.getMessage("/time/end").getFloat();
    }
    
    void printTimePoints();

    
    inline vector<SymbolTimePoint>& getSymbolTimePoints()
    {
        return symbol_time_points;
    }
    
private:
    
    /**
     * Retrieves the time point index matching the time timeToMatch.
     *
     * Runs a dichotomic search to find a time point matching the time
     * timeToMatch in this TimePointArray instance.
     *
     * @param timeToMatch the time to match.
     *
     * @param match       indicates if a time point has matched the time t.
     *
     * @return            the index to insert a new time point, matching the time timeToMatch,
     *                       in this TimePointArray instance.
     */
    int getTimePointInsertIndex(float timeToMatch, bool& match);
    
    inline bool f_almost_equal(float x, float y, int ulp = 2)
    {
        return std::abs(x-y) < std::numeric_limits<float>::epsilon() * std::abs(x+y) * ulp || std::abs(x-y) < std::numeric_limits<float>::min();
    }
    
    inline int compareTimes( float a_t, float b_t )
    {
        return ( a_t < b_t ? 1 : ( f_almost_equal(a_t, b_t) ? 0 : -1 ) );
    }
    
    
    
    vector<SymbolTimePoint>     symbol_time_points;
    
    // const Score&                score;
    
    SymbolTimePoint             prev_timepoint;
    
    vector< pair< OdotBundle, OdotBundle> > voice_staff_vector;
    
    int current_point = 0;
    int prev_point = 0;
    float current_time = 0;
    float m_duration = 0;
    
};

