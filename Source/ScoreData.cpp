
#include "ScoreData.h"
//#include "OSCParser.h"


#include <iostream>


//===============================================================
// SYMBOL
//===============================================================

Symbol::Symbol(OSCBundle b){
    osc_bundle = b;
}


//===============================================================
// STAVE
//===============================================================

Stave::Stave(){}

Stave::Stave( t_rect rect ) {
    m_rect = rect;
}

Stave::~Stave() {
    for ( int i = 0; i < m_symbol.size(); i++ ) {
        delete m_symbol[i];
    }
}

/***********************************
 * Add a new Symbol in the Stave
 ***********************************/
void Stave::addSymbol(Symbol *symbol) {
    m_symbol.emplace_back(symbol);
}



//================================================================
// SYSTEM
//================================================================

System::System(){}

System::System( t_rect rect ) {
    m_rect = rect;
}

System::~System() {
    for ( int i = 0; i < m_stave.size(); i++ ) {
        delete m_stave[i];
    }
}

/***********************************
 * Add a new Stave in the System
 ***********************************/
void System::addStave(Stave *stave) {
    m_stave.emplace_back(stave);
}

/***********************************
 * Get the Nth Stave of the System
 ***********************************/
Stave *System::getStave( int n ) {
    if (n > m_stave.size()) {
        cout << "add stave " << endl;
        while (n > m_stave.size()) {
            addStave(new Stave());
        }
    }
    return m_stave[n-1] ;
}


//===============================================================
// SCORE
//===============================================================

Score::Score(){}

Score::Score( int n, void **bundle_array ) {
    importScoreFromOSC( n, bundle_array );
}

Score::~Score(){
    for ( int i = 0; i < m_system.size(); i++ ) {
        delete m_system[i];
    }
}

/***********************************
 * Add a new System in the Score
 ***********************************/
void Score::addSystem(System *system) {
    m_system.emplace_back(system);
}


/***********************************
 * Get the Nth System of the Score
 ***********************************/
System *Score::getSystem(int n) {
    if (n > m_system.size()) {
        while (n > m_system.size()) {
            addSystem(new System());
        }
    }
    return m_system[n-1];
}



void Score::importScoreFromOSC(int n, void **bundle_array)
{
    std::cout << "importing OSC" << std::endl;
    
    OSCReceiver *r = new OSCReceiver();

    for (int i = 0; i < n ; i++) {
        void* bundle = bundle_array[i];
        int n_stave = 1;
        int n_system = 1;
        // search for stave and system in the OSC messages inside oscb and reassign these values
        
        //Symbol oscb = Symbol(r->parseBundle(bundle));
        Symbol oscb = Symbol(OSCBundle());
        System* sys = getSystem(n_system);
        Stave* sta = sys->getStave(n_stave);
    
        std::cout << "importing OSC symbol in system " << n_system << " / stave " << n_stave << std::endl;
        
        sta->addSymbol(&oscb);

    }
    delete r;
    std::cout << "importing OSC done !" << std::endl;
    
}


