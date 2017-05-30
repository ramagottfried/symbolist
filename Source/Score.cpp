
#include "Score.h"

#include <iostream>
#include <algorithm>
#include <vector>



Score::Score(){}

Score::Score( int n, odot_bundle **bundle_array )
{
    importScoreFromOSC( n, bundle_array );
}

Score::~Score()
{
    for ( int i = 0; i < symbols.size(); i++ ) { delete symbols[i]; }
}


/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::removeAllSymbols()
{
    for ( int i = 0; i < symbols.size(); i++ ) { delete symbols[i]; }
    symbols.clear();
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::addSymbol(Symbol *symbol)
{
    symbols.emplace_back(symbol);
}

/***********************************
 * Removes a Symbol from the Score
 ***********************************/
void Score::removeSymbol(Symbol *symbol)
{
    std::vector<Symbol*>::iterator it = std::find(symbols.begin(), symbols.end(), symbol);
    cout << "iterator " << *it << endl;
}

/***********************************
 * Get the Nth Symbol of the Score
 ***********************************/
Symbol *Score::getSymbol(int n)
{
    if (n < symbols.size()) { return symbols[n]; }
    else { return NULL; }
}

/***********************************
 * Get the number of symbols
 ***********************************/
size_t Score::getSize()
{
    return symbols.size();
}

/***********************************
 * Returns the position of a Symbol in the Score
 ***********************************/
int Score::getSymbolPosition(Symbol *s)
{
    std::vector<Symbol*>::iterator it = std::find(symbols.begin(), symbols.end(), s);
    // cout << "iterator " << it - symbols.begin() << endl;
    if (it >= symbols.end()) return -1;
    else return static_cast<int>( it - symbols.begin() );
}

/***********************************
 * OSC encoding/decoding
 ***********************************/

void Score::importScoreFromOSC(int n, odot_bundle **bundle_array)
{
    removeAllSymbols();
    std::cout << "importing OSC (" << n << " symbols)" << std::endl;
    for (int i = 0; i < n ; i++) {
        Symbol *s = new Symbol();
        s->importFromOSC( bundle_array[i] );
        addSymbol(s);
    }
    std::cout << "OSC import done" << std::endl;
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



