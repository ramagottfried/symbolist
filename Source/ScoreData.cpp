
#include "ScoreData.h"
#include "OSCParser.h"


#include <iostream>


//===============================================================
// SYMBOL
//===============================================================

Symbol::Symbol(OSCBundle b)
{
    osc_bundle = b;
}

int Symbol::getOSCMessagePos(const char* address)
{
    String addr(address);
    for (int i = 0; (i < osc_bundle.size()) ; i++)
    {
        if ( osc_bundle.operator[](i).getMessage().getAddressPattern().toString().equalsIgnoreCase(addr) )
        {
            return i;
        }
    }
    return -1;
}


OSCArgument Symbol::getOSCMessageValue(int pos)
{
    OSCBundle::Element e = getOSCBundle()->operator[](pos);
    return e.getMessage().operator[](0);
}


//===============================================================
// SCORE
//===============================================================

Score::Score(){}

Score::Score( int n, odot_bundle **bundle_array )
{
    importScoreFromOSC( n, bundle_array );
}

Score::~Score()
{
    for ( int i = 0; i < symbols.size(); i++ ) { delete symbols[i]; }
}


void Score::updateContents( int n, odot_bundle** bundle_array )
{
    cout << "clearing score content" << endl;
    for ( int i = 0; i < symbols.size(); i++ ) { delete symbols[i]; }
    symbols.clear();
    cout << "updating score content" << endl;
    importScoreFromOSC( n, bundle_array );
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::addSymbol(Symbol *symbol)
{
    symbols.emplace_back(symbol);
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


void Score::importScoreFromOSC(int n, odot_bundle **bundle_array)
{
    std::cout << "importing OSC" << std::endl;
    
    for (int i = 0; i < n ; i++) {

        odot_bundle *bundle = bundle_array[i];
        std::cout << "decoding " << bundle->len << " bytes : " << bundle->data << std::endl;

        OSCParser p ( bundle->data, bundle->len );
        Symbol *s = new Symbol(p.readBundle());
        addSymbol(s);

    }
    std::cout << "importing OSC done !" << std::endl;
}




