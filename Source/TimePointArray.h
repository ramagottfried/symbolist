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
    inline SymbolTimePoint() {}
    inline SymbolTimePoint(Symbol* s, double t, Symbol* staff)
    {
        addSymbol(s);
        time = t;
        staff_ref = staff;
    }
	
    inline ~SymbolTimePoint()
    {
		DEBUG_FULL("Time point value = " << time << endl)
        staff_ref = NULL;
        for (Symbol* symbol : symbols_at_time)
            symbol = NULL;
    }
    
    inline void removeSymbol(Symbol* s)
    {
        symbols_at_time.erase( remove(symbols_at_time.begin(), symbols_at_time.end(), s ), symbols_at_time.end() );
        
        if( symbols_at_time.size() == 0 )
            delete this;
    }
    
    inline void addSymbol(Symbol* s)
    {
        symbols_at_time.emplace_back(s);
    }
    
    double           time;
    vector<Symbol* > symbols_at_time;
    Symbol*          staff_ref; // << add reference to staff for timepoint (a timepoint can only be on one staff)

    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SymbolTimePoint)
    
};

class TimePointArray
{
public:
    
    inline TimePointArray(){}
    inline ~TimePointArray(){}
    
    /**
     * Sets the score_ptr of this TimePointArray instance.
     */
    inline void setScore(Score* pointerToScore)
    {
        this->score_ptr = pointerToScore;
    }
	
    inline vector< unique_ptr<SymbolTimePoint> >& getSymbolTimePoints()
    {
        return symbol_time_points;
    }
	
	/**
	 * Gets the last time point in this instance of TimePointArray.
	 *
	 * @return a pointer to the last element of the symbol_time_points vector,
	 *         or <code>NULL</code> if symbol_time_points is empty.
	 */
    inline SymbolTimePoint* getLastTimePoint()
    {
    	if (!symbol_time_points.empty())
        	return symbol_time_points.back().get();
		else return NULL;
    }
    
    void printTimePoints();
    void printBundle(OSCBundle bndl);
	
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
     * 		              in this TimePointArray instance.
     */
    int getTimePointInsertIndex(float timeToMatch, bool& match);
    
    void addSymbolTimePoints(Symbol* s);
    void removeSymbolTimePoints(Symbol* s);
    void removeStaffAndSymbolTimePoints( Symbol* s);

    int addSymbol_atTime(Symbol* s, float t, Symbol* staff);
    
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
    OdotBundle_s timePointStreamToOSC(const SymbolTimePoint* tpoint);
    
    int lookupTimePoint(float t);
    Point<float> lookupPathPoint(const Symbol* s, const float t);
    Point<float> lookupPathPoint(const Symbol* s, const int pathIDX, const float t, const float start, const float dur);
    Point<float> lookupPathPoint(const Symbol* s, string& path_base_addr , const float t);
    
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
    void groupLookup(const Symbol* s,
                     const string& output_prefix,
                     double parent_x,
                     double parent_y,
                     float time_ratio,
                     OdotBundle& bndl);
    
    vector<const Symbol* > getNoteOffs(const SymbolTimePoint* prev_tpoint , const SymbolTimePoint* tpoint);
	
    /**
     * Says if a symbol is referenced in a time point.
     *
     * @param symbol    the symbol to search in the time point.
     *
     * @param timePoint the time point in which to look for the symbol.
     *
     * @return          <code>true</code> if symbol is referenced in
     *					timePoint, <code>false</code> otherwise.
     */
    bool isSymbolInTimePoint(const Symbol* symbol, const SymbolTimePoint* timePoint);
    
    pair<size_t, int> getVoiceNumberState(const Symbol* s, const SymbolTimePoint* tpoint);
    pair<size_t, int> setNoteOff(const Symbol* s);
    vector<tuple<size_t, const Symbol*, const Symbol*> > getNoteOffs(const SymbolTimePoint* p);

    void resetTimes();

    inline void reset()
    {
        symbol_time_points.clear();
        prev_timepoint = nullptr;
    }
    
private:
    vector<unique_ptr<SymbolTimePoint> > symbol_time_points;
    
    Score*                       score_ptr = nullptr;
    const SymbolTimePoint*       prev_timepoint = nullptr;
    
    vector<pair<const Symbol*, const Symbol*> > voice_staff_vector;
    
    int                          current_point = 0;
    float                        current_time = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimePointArray)
};

#endif
