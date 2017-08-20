#pragma once

#include "Symbol.h"

using namespace std;

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

/*
class Staff
{
public:
    Staff( Symbol *s ){ m_sym = s; }
    ~Staff(){}
    
    Symbol *getSymbol(){ return m_sym; }
    
private:
    Symbol *m_sym;
};

struct StaffSorter
{
    static int compareElements (const Symbol* a, const Symbol* b)
    {
        //auto a = a_staff->getSymbol();
        //auto b = b_staff->getSymbol();
        
        auto a_x = Symbol::getOSCValueAsFloat( a->getSymbol()getOSCMessageValue("/x") );
        auto a_y = Symbol::getOSCValueAsFloat( a->getOSCMessageValue("/y") );
        auto a_x2 = a_x + Symbol::getOSCValueAsFloat( a->getOSCMessageValue("/w") );
        
        
        auto b_x = Symbol::getOSCValueAsFloat( b->getOSCMessageValue("/x") );
        auto b_y = Symbol::getOSCValueAsFloat( b->getOSCMessageValue("/y") );
        auto b_x2 = b_x + Symbol::getOSCValueAsFloat( b->getOSCMessageValue("/w") );
        
        if( a_y < b_y && a_x2 < b_x )
            return -1;
        else
        {
            if( a_y == b_y && a_x == b_x )
                return 0;
            else
                return 1;
        }
        
    }
};


// redo this to use just a vector of pointers to symbols, and then sort the pointers based on on the sorting algo above

class StaffArray : public OwnedArray<Symbol>
{
public:
    
    StaffArray() = default;
    ~StaffArray() = default;
    
    void addStaff( Symbol *s)
    {
        int pos = s->getOSCMessagePos("/objectType");
        if( pos == -1 || s->getOSCMessageValue(pos).getString() != "staff" )
            return;
        
        if( contains(s) )
            removeObject(s);
        
        addSorted(staff_sorter, new Symbol(s) );
        
        float time = 0.0f;
        for( int i = 0; i < size(); i++ )
        {
            Symbol *sym = (*this)[i];
            float w = Symbol::getOSCValueAsFloat( sym->getOSCMessageValue("/w") );

            sym->setOSCAddrAndValue( "/time/start", time );
            time += sym->pixelsToTime(w) ;
            sym->setOSCAddrAndValue( "/time/duration", time );
            
        }
    }
    
    void removeStaff( Symbol *s)
    {
        removeObject(s);
    }
    
private:    
    StaffSorter     staff_sorter;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (StaffArray)
};
*/

class SortedStaves
{
public:
    
    SortedStaves(){};
    ~SortedStaves()
    {
        staves.clear();
    }
    
    void clear()
    {
        staves.clear();
    }
    
    size_t size()
    {
        return staves.size();
    }
    
    void erase( Symbol *s )
    {
        staves.erase( remove( staves.begin(), staves.end(), s ), staves.end() );
    }
    
    void removeStaff( Symbol *s)
    {
        erase(s);
        assignTimes();
    }
    
    static bool compareStaves (const Symbol* a, const Symbol* b)
    {
        auto a_x = Symbol::getOSCValueAsFloat( a->getOSCMessageValue("/x") );
        auto a_y = Symbol::getOSCValueAsFloat( a->getOSCMessageValue("/y") );
        auto a_x2 = a_x + Symbol::getOSCValueAsFloat( a->getOSCMessageValue("/w") );
        
        
        auto b_x = Symbol::getOSCValueAsFloat( b->getOSCMessageValue("/x") );
        auto b_y = Symbol::getOSCValueAsFloat( b->getOSCMessageValue("/y") );
        auto b_x2 = b_x + Symbol::getOSCValueAsFloat( b->getOSCMessageValue("/w") );
        
        if( a_y < b_y && a_x2 < b_x )
            return -1;
        else
        {
            if( a_y == b_y && a_x == b_x )
                return 0;
            else
                return 1;
        }
        
    }
    
    void assignTimes()
    {
        float time = 0.0f;
        for( auto it = staves.begin(); it != staves.end(); it++)
        {
            cout << time << " ";
            Symbol *sym = *it;
            
            float w = Symbol::getOSCValueAsFloat( sym->getOSCMessageValue("/w") );
            
            sym->setOSCAddrAndValue( "/time/start", time );
            time += sym->pixelsToTime(w) ;
            sym->setOSCAddrAndValue( "/time/duration", time );
            
            cout << time << endl;
        }
        
    }
    
    void addStaff( Symbol *s)
    {
        int pos = s->getOSCMessagePos("/objectType");
        if( pos == -1 || s->getOSCMessageValue(pos).getString() != "staff" )
            return;
        
        removeStaff(s);
        
        cout << "adding staff " << endl;
        staves.emplace_back(s);
        
        sort( staves.begin(), staves.end(), compareStaves );
        
        assignTimes();
        
        // not the right place to deal with duplicate names.. maybe duplicates should be allowed?
        cout << "setting staff name -- size: " << staves.size() << endl;
        String name;
        auto name_pos = s->getOSCMessagePos("/name");
        if( name_pos != -1 )
        {
            name = s->getOSCMessageValue(name_pos).getString();
        }
        
        if( name.isEmpty() ) // for now allow  name == s->getID()
        {
            s->setOSCAddrAndValue( "/name", "staff_" + (String)staves.size() );
        }
         
    }
    
   
    
private:
    
    vector<Symbol*>     staves;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (SortedStaves)
};

