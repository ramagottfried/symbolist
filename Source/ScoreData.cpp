
#include "ScoreData.h"
#include "OSCIO.h"


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
    OSCBundle::Element e = osc_bundle.operator[](pos);
    return e.getMessage().operator[](0);
}


odot_bundle* Symbol::exportToOSC()
{
    OSCWriter w ;
    w.writeBundle( osc_bundle );
    odot_bundle *ob = new odot_bundle;
    ob->len = static_cast<long>(w.getDataSize());
    ob->data = new char[w.getDataSize()];
    std::strcpy(ob->data,static_cast<const char*>(w.getData()));
    std::cout << "encoding " << ob->len << " bytes : " << ob->data << std::endl;
    return ob;
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

/***********************************
 * OSC encoding/decoding
 ***********************************/

void Score::importScoreFromOSC(int n, odot_bundle **bundle_array)
{
    std::cout << "importing OSC" << std::endl;
    
    for (int i = 0; i < n ; i++) {

        odot_bundle *bundle = bundle_array[i];
        std::cout << "decoding " << bundle->len << " bytes : " << bundle->data << std::endl;

        OSCReader r ( bundle->data, bundle->len );
        Symbol *s = new Symbol(r.readBundle());
        addSymbol(s);
    }
    std::cout << "import OK !" << std::endl;
}

void Score::updateScoreFromOSC( int n, odot_bundle** bundle_array )
{
    for ( int i = 0; i < symbols.size(); i++ ) { delete symbols[i]; }
    symbols.clear();
    importScoreFromOSC( n, bundle_array );
}


/*
void Score::deleteOdotBundleArray(odot_bundle** bundle_array, int size)
{
    for (int i = 0; i < size; i++)
    {
        cout << "delete " << i << " " << bundle_array[i] << endl;
        delete bundle_array[i] ;
    }
    delete bundle_array ;
}
*/



