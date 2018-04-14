#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>
#include "types.h"
#include "Symbol.h"
#include "TimePointArray.h"
#include "SortedStaves.hpp"

#include "SVGFileIO.hpp"

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
    Score(const OdotBundle_s& s_bundle) ;
    ~Score();
    
    /**********************************************
     *             GETTERS AND SETTERS            *
     **********************************************/
    inline TimePointArray* getTimePoints() { return &time_points; };
    inline const TimePointArray* getTimePointArray() const
    {
        return &time_points;
    }
    
    /**
     * Gets the count of symbols contained in the score.
     *
     * @return the size of the score_symbols vector.
     */
    size_t getSize();
    
    /**
     * Gets the symbol at index n in the score.
     *
     * @param n             the index of the symbol to retrieve
     *                      in the score.
     *
     * @throws length_error If n is bigger than the score size.
     *
     * @return              a pointer to the symbol at
     *                      index n in the score.
     */
    Symbol* getSymbol(int n);
    
    int getSymbolPosition(Symbol* s);
    
    /**
     * Creates a new symbol in the score.
     *
     * @return a pointer to the newly created symbol.
     */
    Symbol* createSymbol();
    
    /**
     * Adds a new symbol to the score.
     *
     * Actually, a new symbol is created by copying
     * the one in parameter, then it is added to the score.
     * If the symbol in parameter already exists in the score
     * then the symbol is updated with the values of the incoming
     * symbol, and a pointer to the currently existing symbol
     * in the score is returned.
     *
     * @param symbol a pointer to the symbol which will be copied
     *               to create a new symbol in the score.
     *
     * @return       a pointer to the newly created symbol.
     */
    Symbol* addSymbol(Symbol* symbol);
    
    /**
     * Removes a symbol from the score.
     *
     * Looks for the symbol passed as an argument in the score,
     * and removes it if it exists.
     *
     * @param symbol            a pointer to the symbol to be removed
     *                          from the score.
     *
     * @throws invalid_argument If the pointer passed as an argument is
     *                          <code>NULL</code> or if the referenced
     *                          symbol is not in the score.
     *
     * @throws logic_error      If the method is called while the score
     *                          is empty (there are no symbols to remove).
     */
    void removeSymbol(Symbol* symbol);
    void removeAllSymbols();
        
    void importScoreFromOSC( const OdotBundle_s& s_bundle );
    
    void print() const;
    
    void addSymbolTimePoints( Symbol* s );
    void removeSymbolTimePoints( Symbol* s );
    
    OdotBundle_s getSymbolsAtTime( float t );
    
    OdotBundle_s getScoreBundle_s();
    
    string getJSON();

    string getSVG();
    
    
    /**
     * Imports the Score from an SVG file on disk.
     *
     *
     * @param filename          name of file to write, including path
     *
     * @throws invalid_argument (nothing yet)
     *
     * @throws logic_error      (nothing yet)
     */
    int importSVG( const char * filename )
    {
        SVGFileIO svg;
        svg.read( filename );
        
        return 0;
    }
    
    /**
     * Exports the Score from to SVG file on disk.
     *
     *
     * @param filename          name of file to write, including path
     *
     * @throws invalid_argument (nothing yet)
     *
     * @throws logic_error      (nothing yet)
     */
    int exportSVG( const char * filename )
    {
        SVGFileIO svg;
        svg.write( score_symbols, filename );
        
        return 0;
    }
    
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








