
#include "ScoreData.h"
#include "OSCParser.h"


#include <iostream>


//===============================================================
// SYMBOL
//===============================================================

Symbol::Symbol(OSCBundle b){
    osc_bundle = b;
}

int Symbol::getOSCMessagePos(const char* address){
    
    String addr(address);
    for (int i = 0; (i < osc_bundle.size()) ; i++) {

        if ( osc_bundle.operator[](i).getMessage().getAddressPattern().toString().equalsIgnoreCase(addr) ) {
            return i;
        }
    }
    return -1;
}


OSCArgument Symbol::getOSCMessageValue(int pos){
    
    OSCBundle::Element e = getOSCBundle()->operator[](pos);
    return e.getMessage().operator[](0);

}


//===============================================================
// SCORE
//===============================================================

Score::Score(){}

Score::Score( int n, odot_bundle **bundle_array ) {
    importScoreFromOSC( n, bundle_array );
}

Score::~Score(){
    for ( int i = 0; i < symbols.size(); i++ ) { delete symbols[i];}
}

/***********************************
 * Add a new Symbol in the Score
 ***********************************/
void Score::addSymbol(Symbol *symbol) {
    symbols.emplace_back(symbol);
}


/***********************************
 * Get the Nth Symbol of the Score
 ***********************************/
Symbol *Score::getSymbol(int n) {
    if (n < symbols.size()) {
        return symbols[n];
    } else {
        return NULL;
    }
}

/***********************************
 * Get the number of symbols
 ***********************************/
size_t Score::getSize() {
    return symbols.size();
}


void Score::importScoreFromOSC(int n, odot_bundle **bundle_array)
{
    std::cout << "importing OSC" << std::endl;
    
    for (int i = 0; i < n ; i++) {

        odot_bundle *bundle = bundle_array[i]; // static_cast<odot_bundle*>(bundle_array[i]);
        
        //std::cout << "decoding " << bundle->len << " bytes : " << bundle->data << std::endl;

        OSCParser p ( bundle->data, bundle->len );
        Symbol *s = new Symbol(p.readBundle());
        addSymbol(s);

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


