
#ifndef SortedStaves_hpp
#define SortedStaves_hpp

#include "Symbol.h"

/*
 time points should stay as a flat list, but maybe they should point to the staff that they're attached to
 
 when a symbol is attached to a staff, that staff is used as the offset into time
 
 when an object is converted to a staff (or allocated as a staff objectType),
 it gets sorted and then gets time/start and duration, which then is used to set the symbol's time point
 
 
 ... also, when a staff is moved, the time points may need to be recalculated
 ... and this also means that when loading a score, if there is a staff referenced by an object it should probably already exist
 ... if it doesn't: search for it?
 ... similarly, whenever a new staff is created, do we need to find all symbols referring to it?
 ... this is where it might make more sense to have a hierarchy on load: i.e. /staff/1/symbol/0/...
 ... where a Staff is a different kind of object (and OSC namespace) that contains sub symbols...
 
 ... this doesn't mean you have to konw what the staves are before composing, but the OSC bundle should be reconstructed to work in this way...
 
 */

class SortedStaves
{
    vector<Symbol* > staves;

public:
    
    SortedStaves();
    ~SortedStaves();
    
    void clear();
    size_t size();
    void erase(Symbol* s);
    void removeStaff(Symbol* s);
    static bool compareStaves(Symbol* a, Symbol* b);
    void resetTimes();
    bool addStaff(Symbol* s);
    
    StringArray getStaveNames();
    
    Symbol* getStaveAtTime(float time);
    const Symbol* getStaveByID( const String& id );
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SortedStaves)
};

#endif /* SortedStaves_hpp */
