#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include <vector>

using namespace std;

struct t_rect
{
    vector<float> pos;
    float w, h;
};

/****************
 * SYMBOL
 ****************/

class Symbol {
    
    public:
    Symbol(OSCBundle b);
    
    private:
    OSCBundle osc_bundle;
    t_rect m_rect;
};


/****************
 * STAVE
 ****************/

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


/****************
 * SYSTEM
 ****************/

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


/****************
 * SCORE
 ****************/

struct odot_bundle
{
    long len;
    char *data;
};

class Score {

    public:
    
        Score();
        Score( int n, void **bundle_array ) ;
        ~Score();
    
        void addSystem(System *system);
        System *getSystem(int n);
    
    private:
    
        vector<System*> m_system;
        void importScoreFromOSC(int n, void** bundle_array);
    
};
