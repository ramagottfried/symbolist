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
    bool operator() (const shared_ptr<Symbol> a, const shared_ptr<Symbol> b)
    {
        auto a_t = a->getTime();
        auto b_t = b->getTime();
        return (a_t < b_t);
    }
};

class Score
{
    vector<shared_ptr<Symbol> > score_symbols;
    TimePointArray              time_points;
    SortedStaves                staves;
    ScoreSorter                 score_sorter;
    
public:
    
    Score();
    Score(Score& src);
    
    /**
     * Copy constructor with raw pointer passed as argument.
     * This constructor is only called by the make_shared function.
     */
    Score(Score* src);
    Score(const OdotBundle_s& s_bundle) ;
    ~Score();
    
    size_t getSize();
    
    shared_ptr<Symbol> getSymbol(int n);
    int getSymbolPosition(shared_ptr<Symbol> s);
    
    void addSymbol(shared_ptr<Symbol> s);
    void removeSymbol(shared_ptr<Symbol> s);
    void removeAllSymbols();
        
    void importScoreFromOSC( const OdotBundle_s& s_bundle );
    
    //    void sortScore();
    
    void print() const;
    
    void addSymbolTimePoints( shared_ptr<Symbol> s );
    void removeSymbolTimePoints( shared_ptr<Symbol> s );
    
    OdotBundle_s getSymbolsAtTime( float t );

    //odot_bundle *getScoreBundle();
    
    OdotBundle_s getScoreBundle_s();

    const Array<shared_ptr<Symbol>> getSymbolsByValue(const string& address, const string& value);

    const TimePointArray* getTimePointArray() const {
        
        return &time_points;
        
    }
    
    const StringArray getStaves();
    void addStaff( shared_ptr<Symbol> s );
    
    void updateStaves(shared_ptr<Symbol> moved_stave);
    void updateStavesAndTimepoints();
    shared_ptr<Symbol> getStaveAtTime( float time );
    const shared_ptr<Symbol> getStaveByID( const string& id );

    OdotBundle_s getDurationBundle();

    
    int getNameCount( string& name )
    {
        int count = 0;
        for( shared_ptr<Symbol> s : score_symbols )
        {
            s->print();
            if( s != NULL && s->getName() == name )
                count++;
        }
    
        return count;
    }
    
    bool idExists( string& idStr )
    {
        for( shared_ptr<Symbol> s : score_symbols )
        {
            if( s->getID() == idStr )
                return true;
        }
        
        return false;
    }
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (Score)
};








