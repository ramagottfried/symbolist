
#include "Score.h"

#include <iostream>
#include <algorithm>
#include <vector>


Score::Score() : time_points(this) {}

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
}


/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::addSymbol(Symbol *symbol)
{
    score_symbols.addSorted( score_sorter, symbol );
    time_points.addSymbolTimePoints( symbol );
    
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
    time_points.removeSymbolTimePoints( s );
}

void Score::addSymbolTimePoints( Symbol *s )
{
    time_points.addSymbolTimePoints( s );
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


const StringArray Score::getStaves()
{
    StringArray staves;
    
    for( auto s : score_symbols )
    {
        int pos = s->getOSCMessagePos("/objectType");
        if( pos != -1 && s->getOSCMessageValue(pos).getString() == "staff" )
        {
            pos = s->getOSCMessagePos("/name");
            if( pos != -1 )
            {
                staves.add( s->getOSCMessageValue(pos).getString() );
            }
        }
        
    }
    return staves;
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

/*
void Score::sortScore()
{
    auto sorted = score_symbols;
    std::sort(sorted.begin(),
              sorted.end(),
              [](Symbol *a, Symbol *b) { return (a->getTime() < b->getTime()); } );
    
    for (auto e : sorted )
    {
        std::cout << e->getOSCMessageValue("/x").getFloat32() << " " << e->getTime() << std::endl;
    }
    
}



*/
