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
    
        OSCBundle *getOSCBundle () { return &osc_bundle; }
    
        int getOSCMessagePos(const char* address);
        OSCArgument getOSCMessageValue(int pos);

    
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

        void addSymbol(Symbol *system);
        size_t getSize();
        Symbol *getSymbol(int n);

    private:
    
        vector<Symbol*> symbols;
    
        void importScoreFromOSC(int n, odot_bundle** bundle_array);
    
};





/****************
 * STAVE
 ****************/
/*
 class Stave {
 
 public:
 
 Stave();
 Stave( t_rect rect );
 ~Stave();
 void addSymbol(Symbol *symbol);
 
 private:
 
 vector<Symbol*> m_symbol;
 t_rect m_rect;
 };
 */

/****************
 * SYSTEM
 ****************/
/*
 class System {
 
 public:
 
 System();
 System( t_rect rect );
 ~System();
 
 void addStave(Stave *stave);
 Stave *getStave(int n);
 
 private:
 vector<Stave*> m_stave;
 t_rect m_rect;
 };
 */


