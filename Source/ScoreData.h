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
        Symbol(OSCBundle b);
    
        OSCBundle getOSCBundle () { return osc_bundle; }
    
        int getOSCMessagePos(const char* address);
        OSCArgument getOSCMessageValue(int pos);
        odot_bundle* exportToOSC();
    
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
    
        void importScoreFromOSC( int n, odot_bundle** bundle_array );
        void updateScoreFromOSC( int n, odot_bundle** bundle_array ) ;
    
        // static void deleteOdotBundleArray(odot_bundle** bundle_array, int size);
    
    private:
    
        vector<Symbol*> symbols;
    
};


