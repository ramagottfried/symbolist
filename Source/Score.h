#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>
#include "types.h"
#include "Symbol.h"


using namespace std;


//============================
// SCORE
//============================

class Score {

    public:
    
        Score();
        Score( int n, odot_bundle** bundle_array ) ;
        ~Score();

        size_t getSize();
        Symbol *getSymbol(int n);
        int getSymbolPosition(Symbol* s);
    
        void addSymbol(Symbol *s);
        void removeSymbol(Symbol *s);
        void removeAllSymbols();
    
        void importScoreFromOSC( int n, odot_bundle** bundle_array );
    
    private:
    
        vector<Symbol*> symbols;
    
};


