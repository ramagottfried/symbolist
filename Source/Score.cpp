#include "Score.h"

#include <iostream>
#include <algorithm>
#include <vector>

Score::Score()
{
    cout << "score " << this << " " << score_symbols.size() << endl;
}

Score::Score(Score& src)
{
    for( shared_ptr<Symbol> symbol : src.score_symbols )
    {
        shared_ptr<Symbol> new_sym = make_shared<Symbol>( symbol->get_o_ptr() );
        score_symbols.push_back( new_sym );
        addStaff( new_sym );
    }
    
    updateStavesAndTimepoints();
    
    cout << "copying score " << this << " " << score_symbols.size() << " n staves " << staves.size() << endl;
}

Score::Score(Score* src)
{
    for( shared_ptr<Symbol> symbol : src->score_symbols )
    {
        shared_ptr<Symbol> new_sym = make_shared<Symbol>( symbol->get_o_ptr() );
        score_symbols.push_back( new_sym );
        addStaff( new_sym );
    }
    
    updateStavesAndTimepoints();
    
    cout << "copying score " << this << " " << score_symbols.size() << " n staves " << staves.size() << endl;
}

Score::Score( const OdotBundle_s& s_bundle  )
{
    importScoreFromOSC( s_bundle );
}

Score::~Score() {}

void Score::print() const
{
    int count = 1;
    for( auto s : score_symbols )
    {
        cout << "symbol : " << count << endl;
        s->print();
    }
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::removeAllSymbols()
{
    score_symbols.clear();
    time_points.clear();
    staves.clear();
}


/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::addSymbol(shared_ptr<Symbol> symbol)
{
    // Calls the sort function to properly insert the new symbol
    score_symbols.push_back(symbol);
    sort(score_symbols.begin(), score_symbols.end(), score_sorter);
    
    bool newstaff = staves.addStaff( symbol ) ;
    time_points.addSymbolTimePoints( symbol );

    if( newstaff )
    {
        for( auto s : score_symbols )
        {
            if( s->getSaff() == symbol->getID() ) // this should look up by name not nameID, in the timepoints the staves should be combined,... although then I guess the types of clef could change? leaving as id for now, but this is uninituitive to set from outside the editor
            {
                //time_points.removeSymbolTimePoints(s);
                time_points.addSymbolTimePoints( s );
            }
        }
    }
    
  //  symbol->setID( symbol->getType() + "_" + (String)getSize() );
}



/***********************************
 * Removes a Symbol from the Score
 ***********************************/
void Score::removeSymbol(shared_ptr<Symbol> symbol)
{
    assert( score_symbols.empty() == false ); ///< what do you want to remove ?
    
    for( auto it = score_symbols.begin(); it != score_symbols.end(); it++)
    {
        if( symbol == (*it) )
        {
            staves.removeStaff( symbol );
            score_symbols.erase(it);
            return;
        }
    }
    
    assert( false );    ///< not found
}

/***********************************
 * Create Single Bundle from Score Bundles
 ***********************************/
OdotBundle_s Score::getScoreBundle_s()
{
    // for now merging flat array into bundle and then serializing
    // probalby in the future we should optimize this, and/or provide mechanisms for outputting the score as a hierarchy
    
    OdotBundle bndl;
    
    long count = 0;
    string prefix = "/symbol/";
    for( auto sym : score_symbols )
    {
        bndl.addMessage( prefix + to_string(count), *sym );
        count++;
    }
    
    return bndl.serialize();
}

/***********************************
 * Get active symbols at time
 ***********************************/
OdotBundle_s Score::getSymbolsAtTime( float t )
{
    return time_points.getSymbolsAtTime( t );
}

void Score::removeSymbolTimePoints( shared_ptr<Symbol> s )
{
    time_points.removeStaffAndSymbolTimePoints( s );
}

void Score::addSymbolTimePoints( shared_ptr<Symbol> s )
{
    time_points.addSymbolTimePoints( s );
}


OdotBundle_s Score::getDurationBundle()
{
    shared_ptr<SymbolTimePoint> lastTimePoint = time_points.back();
    if( !lastTimePoint )
        return NULL;
    
    OdotBundle bndl;
    bndl.addMessage("/time/duration", lastTimePoint->time );
    
    return bndl.serialize();
}

/***********************************
 * Get the Nth Symbol of the Score
 ***********************************/
shared_ptr<Symbol> Score::getSymbol(int n)
{
    if (n < score_symbols.size()) { return score_symbols[n]; }
    else { return NULL; }
}

/***********************************
 * Get the number of symbols
 ***********************************/
size_t Score::getSize()
{
    return score_symbols.size();
}

/***********************************
 * Returns the position of a Symbol in the Score
 ***********************************/
int Score::getSymbolPosition(shared_ptr<Symbol> s)
{
    auto iteratorToSymbol = find(score_symbols.begin(), score_symbols.end(), s);
    return static_cast<int>(distance(score_symbols.begin(), iteratorToSymbol));
}

const Array<shared_ptr<Symbol>> Score::getSymbolsByValue( const string& address, const string& value )
{
    Array<shared_ptr<Symbol>> matched;
    for (auto s : score_symbols )
    {
        OdotMessage val = s->getMessage( address );
        
        if( val[0].getType() == OdotAtom::O_ATOM_STRING && val[0].getString() == value )
        {
            matched.add( s );
        }
    }
    return matched;
}


const shared_ptr<Symbol> Score::getStaveByID( const string& id )
{
    return staves.getStaveByID( id );
}

/***********************************
 * OSC encoding/decoding
 ***********************************/

void Score::importScoreFromOSC( const OdotBundle_s& s_bundle )
{
    removeAllSymbols();
    
    OdotBundle bundle( s_bundle ); //<< deserializes the bundle
    
    std::cout << "===IMPORTRING OSC (" << bundle.size() << " messages)" << std::endl;
    for( auto msg : bundle.getMessageArray() )
    {
        if( msg.getAddress().find("/symbol") == 0 && msg[0].getType() == OdotAtom::O_ATOM_BUNDLE )
        {
            shared_ptr<Symbol> s = make_shared<Symbol>( msg.getBundle().get_o_ptr() );
            addSymbol(s);
        }
    }
    
    std::cout << "===IMPORT DONE" << std::endl;
}

/***********************************
 * Staff/Stave handling
 ***********************************/

void Score::addStaff( shared_ptr<Symbol> s )
{
    staves.addStaff( s );
}

/***********************************
 * called when staff is moved
 ***********************************/

void Score::updateStaves(shared_ptr<Symbol> moved_stave)
{

    string type = moved_stave->getMessage("/type").getString();
    
    if( type != "staff" )
        return;
    
    // 1. remove time points for moved stave
    // 2. resort the staves
    // 3. add time points for sybols on the stave
    
    // remove (this is done already in the modifySymbolInScore symbolist handler function)
    // time_points.removeStaffAndSymbolTimePoints(moved_stave);
    
    // sort staves and set times for each stave
    staves.resetTimes();
    
    // add the symbols
    
    String staff_id = moved_stave->getID();
    for( auto s : score_symbols )
    {
        if( s->getSaff() == staff_id )
        {
            time_points.addSymbolTimePoints(s);
        }
    }
    
    // time_points.resetTimes();
}

/*
 *  called when a stave is moved to update all timepoints, could be optimized in the future
 */

void Score::updateStavesAndTimepoints()
{
    
    // sort staves and set times for each stave
    staves.resetTimes();
    time_points.reset();
    
    // add the symbols
    
    for( auto s : score_symbols )
    {
        time_points.addSymbolTimePoints(s);
    }
    
}

shared_ptr<Symbol> Score::getStaveAtTime( float time )
{
    return staves.getStaveAtTime(time);
}


const StringArray Score::getStaves()
{
    return staves.getStaveNames();
}


/*
void Score::deleteOdotBundleArray(odot_bundle** bundle_array, int size)
{
    for (int i = 0; i < size; i++) {
        cout << "delete " << i << " " << bundle_array[i] << endl;
        delete bundle_array[i] ;
    }
    delete bundle_array ;
}
*/

