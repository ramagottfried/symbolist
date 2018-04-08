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
    bool operator() (const std::unique_ptr<Symbol>& a, const std::unique_ptr<Symbol>& b)
    {
        auto a_t = a->getTime();
        auto b_t = b->getTime();
        return (a_t < b_t);
    }
};

class Score
{
    vector<unique_ptr<Symbol> > score_symbols;
    TimePointArray              time_points;
    SortedStaves                staves;
    ScoreSorter                 score_sorter;
    
public:
    
    /***************************************
     *             CONSTRUCTORS            *
     ***************************************/
    Score();
    Score(Score& src);
    
    /**
     * Copy constructor with raw pointer passed as argument.
     * This constructor is only called by the make_shared function.
     */
    Score(Score* src);
    Score(const OdotBundle_s& s_bundle) ;
    ~Score();
    
    /**********************************************
     *             GETTERS AND SETTERS            *
     **********************************************/
    inline TimePointArray* getTimePoints() { return &time_points; };
    const TimePointArray* getTimePointArray() const
    {
        return &time_points;
    }
    
    
    size_t getSize();
    
    Symbol* getSymbol(int n);
    int getSymbolPosition(Symbol* s);
    
    /**
     * Creates a new empty symbol in the score.
     *
     * @return a pointer to the newly create symbol.
     */
    Symbol* createSymbol();
    Symbol* addSymbol(Symbol* s);
    void removeSymbol(Symbol* s);
    void removeAllSymbols();
        
    void importScoreFromOSC( const OdotBundle_s& s_bundle );
    
    //    void sortScore();
    
    void print() const;
    
    void addSymbolTimePoints( Symbol* s );
    void removeSymbolTimePoints( Symbol* s );
    
    OdotBundle_s getSymbolsAtTime( float t );

    //odot_bundle *getScoreBundle();
    
    OdotBundle_s getScoreBundle_s();
    
    string getJSON();


    const Array<Symbol*> getSymbolsByValue(const string& address, const string& value);
    
    const StringArray getStaves();
    void addStaff( Symbol* s );
    
    void updateStaves(Symbol* moved_stave);
    void updateStavesAndTimepoints();
    Symbol* getStaveAtTime( float time );
    const Symbol* getStaveByID( const string& id );

    OdotBundle_s getDurationBundle();
    
    
    int getNameCount( string& name )
    {
        int count = 0;
        for(auto iteratorToSymbol = score_symbols.begin(); iteratorToSymbol != score_symbols.end(); iteratorToSymbol++)
            if( (*iteratorToSymbol)->getName() == name )
                count++;
    
        return count;
    }
    
    bool idExists( string& idStr )
    {
        for(auto iteratorToSymbol = score_symbols.begin(); iteratorToSymbol != score_symbols.end(); iteratorToSymbol++)
            if( (*iteratorToSymbol)->getID() == idStr )
                return true;
        
        return false;
    }
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (Score)
};








