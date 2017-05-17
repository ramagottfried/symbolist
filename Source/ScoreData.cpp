
#include "ScoreData.h"
#include "OSCParser.h"


#include <iostream>


//===============================================================
// SYMBOL
//===============================================================

Symbol::Symbol(OSCBundle b){
    osc_bundle = b;
}


//===============================================================
// SCORE
//===============================================================

Score::Score(){}

Score::Score( int n, odot_bundle **bundle_array ) {
    importScoreFromOSC( n, bundle_array );
}

Score::~Score(){
//    for ( int i = 0; i < m_system.size(); i++ ) { delete m_system[i];}
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
 void Score::addSymbol(Symbol *symbol) {
    symbols.emplace_back(symbol);
     
     // search for stave and system in the OSC messages inside oscb and reassign these values
     //int n_stave = 1;
     //int n_system = 1;
     //System* sys = getSystem(n_system);
     //Stave* sta = sys->getStave(n_stave);
     //std::cout << "importing OSC symbol in system " << n_system << " / stave " << n_stave << std::endl;
}


/***********************************
 * Get the Nth Symbol of the Score
 ***********************************/
Symbol *Score::getSymbol(int n) {
    if (n > symbols.size()) {
        return symbols[n-1];
    } else { return new Symbol(OSCBundle()); }
}



void Score::importScoreFromOSC(int n, odot_bundle **bundle_array)
{
    std::cout << "importing OSC" << std::endl;
    
    for (int i = 0; i < n ; i++) {

        odot_bundle *bundle = bundle_array[i]; // static_cast<odot_bundle*>(bundle_array[i]);
        
        std::cout << "decoding " << bundle->len << " bytes : " << bundle->data << std::endl;

        OSCParser p ( bundle->data, bundle->len );
        Symbol oscb = Symbol(p.readBundle());
        addSymbol(&oscb);

    }
    std::cout << "importing OSC done !" << std::endl;
}




//===============================================================
// STAVE
//===============================================================
/*
 Stave::Stave(){}
 
 Stave::Stave( t_rect rect ) {
 m_rect = rect;
 }
 
 Stave::~Stave() {
 for ( int i = 0; i < m_symbol.size(); i++ ) {
 delete m_symbol[i];
 }
 }
 */
/***********************************
 * Add a new Symbol in the Stave
 ***********************************/
/*
 void Stave::addSymbol(Symbol *symbol) {
 m_symbol.emplace_back(symbol);
 }
 */


//================================================================
// SYSTEM
//================================================================
/*
 System::System(){}
 
 System::System( t_rect rect ) {
 m_rect = rect;
 }
 
 System::~System() {
 for ( int i = 0; i < m_stave.size(); i++ ) {
 delete m_stave[i];
 }
 }
 */

/***********************************
 * Add a new Stave in the System
 ***********************************/
/*
 void System::addStave(Stave *stave) {
 m_stave.emplace_back(stave);
 }
 */

/***********************************
 * Get the Nth Stave of the System
 ***********************************/
/*
 Stave *System::getStave( int n ) {
 if (n > m_stave.size()) {
 cout << "add stave " << endl;
 while (n > m_stave.size()) {
 addStave(new Stave());
 }
 }
 return m_stave[n-1] ;
 }
 */


