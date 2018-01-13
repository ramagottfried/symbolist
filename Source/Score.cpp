
#include "Score.h"

#include <iostream>
#include <algorithm>
#include <vector>


Score::Score() : time_points(this)
{
    cout << "score " << this << " " << score_symbols.size() << endl;
}

Score::Score(Score& src) : time_points(this)
{
    
    for( Symbol* s : src.score_symbols )
    {
        score_symbols.add( new Symbol(*s) );
    }
    
    time_points = src.time_points;
    
    
    staves = src.staves;
    
    cout << "copying score " << this << " " << score_symbols.size() << endl;
}

Score::Score( int n, odot_bundle **bundle_array ) : time_points(this)
{
    importScoreFromOSC( n, bundle_array );
}

Score::~Score() {}


/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::removeAllSymbols()
{
    score_symbols.clear(1);
    time_points.clear(1);
    staves.clear();
}


/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::addSymbol(Symbol *symbol)
{
    score_symbols.addSorted( score_sorter, symbol );
    
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
void Score::removeSymbol(Symbol *symbol)
{
    assert( score_symbols.isEmpty() == false ); ///< what do you want to remove ?
    
    for( int i = 0; i < score_symbols.size(); i++ )
    {
        if( symbol == score_symbols[i] )
        {
            staves.removeStaff( symbol );
            score_symbols.remove(i);
            return;
        }
    }
    
    assert( false );    ///< not found
}


/***********************************
 * Create Single Bundle from Score Bundles
 ***********************************/
odot_bundle *Score::getScoreBundle()
{
    OSCBundle bndl;
    int count = 0;
    // maybe log number of symbols here... but will have to check it when loading
    String prefix = "/symbol/";
    for( auto sym : score_symbols )
    {
        auto s_bndl = *(sym->getOSCBundle());
        
        for ( auto osc : s_bndl )
        {
            OSCMessage msg = osc.getMessage();
            String newaddr = prefix + String(count) + msg.getAddressPattern().toString();
            msg.setAddressPattern(newaddr);
            bndl.addElement(msg);
        }

        count++;

    }
    
    OSCWriter w ;
    w.writeBundle( bndl );
    size_t size = w.getDataSize();
    
    odot_bundle *bundle = new odot_bundle;
    bundle->len = static_cast<long>(size);
    bundle->data = new char[size];
    
    std::memcpy(bundle->data, w.getData() ,size );
    return bundle;
}

/***********************************
 * Get active symbols at time
 ***********************************/
odot_bundle *Score::getSymbolsAtTime( float t )
{
    return time_points.getSymbolsAtTime( t );
}

void Score::removeSymbolTimePoints( Symbol *s )
{
    time_points.removeStaffAndSymbolTimePoints( s );
}

void Score::addSymbolTimePoints( Symbol *s )
{
    time_points.addSymbolTimePoints( s );
}


odot_bundle* Score::getDurationBundle()
{
    auto last = time_points.getLast();
    if( !last )
        return NULL;
    
    OSCBundle bndl;
    bndl.addElement( OSCMessage("/time/duration", (float)last->time ));
    
    OSCWriter w ;
    w.writeBundle( bndl );
    size_t size = w.getDataSize();
    
    odot_bundle *bundle = new odot_bundle;
    bundle->len = static_cast<long>(size);
    bundle->data = new char[size];
    
    std::memcpy(bundle->data, w.getData() ,size );
    return bundle;

}

/***********************************
 * Get the Nth Symbol of the Score
 ***********************************/
Symbol *Score::getSymbol(int n)
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
int Score::getSymbolPosition(Symbol *s)
{
    return score_symbols.indexOfSorted(score_sorter, s );
}

const Array<Symbol*> Score::getSymbolsByValue( const String& address, const String& value )
{
    Array<Symbol*> matched;
    for (auto s : score_symbols )
    {
        OSCArgument val = s->getOSCMessageValue( address );
        if( val.isString() && val.getString() == value )
        {
            matched.add( s );
        }
    }
    return matched;
}


const Symbol* Score::getStaveByID( const String& id )
{
    return staves.getStaveByID( id );
}

/***********************************
 * OSC encoding/decoding
 ***********************************/

void Score::importScoreFromOSC(int n, odot_bundle **bundle_array)
{
    removeAllSymbols();
    std::cout << "===IMPORTRING OSC (" << n << " symbols)" << std::endl;
    for (int i = 0; i < n ; i++) {
        Symbol *s = new Symbol();
        s->importFromOSC( bundle_array[i] );
        addSymbol(s);
    }
    std::cout << "===IMPORT DONE" << std::endl;
}

/***********************************
 * Staff/Stave handling
 ***********************************/

void Score::addStaff( Symbol *s )
{
    staves.addStaff( s );
}

/***********************************
 * called when staff is moved
 ***********************************/

void Score::updateStaves(Symbol *moved_stave)
{

    int pos = moved_stave->getOSCMessagePos("/type");
    if( moved_stave->getOSCMessageValue(pos).getString() != "staff" )
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

Symbol *Score::getStaveAtTime( float time )
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

