#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>
#include "types.h"

using namespace std;

//============================
// SYMBOL
//============================

class Symbol {
    
    public:
    
        Symbol();
        Symbol(OSCBundle b);
    
        OSCBundle getOSCBundle () { return osc_bundle; }
    
        int getOSCMessagePos(const char* address);
        OSCArgument getOSCMessageValue(int pos);
        odot_bundle* exportToOSC();
        void importFromOSC(odot_bundle *bundle);
    
    private:
        
        OSCBundle osc_bundle;
        t_rect m_rect;
};



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
    
        void addSymbol(Symbol *s);
        void removeAllSymbols();
    
        void importScoreFromOSC( int n, odot_bundle** bundle_array );
    
    private:
    
        vector<Symbol*> symbols;
    
};


