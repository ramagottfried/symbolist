
#include "SortedStaves.hpp"

SortedStaves::SortedStaves(){}

SortedStaves::~SortedStaves()
{
    staves.clear();
}

void SortedStaves::clear()
{
    staves.clear();
}

size_t SortedStaves::size()
{
    return staves.size();
}

void SortedStaves::erase( Symbol *s )
{
    staves.erase( remove( staves.begin(), staves.end(), s ), staves.end() );
}

void SortedStaves::removeStaff( Symbol *s)
{
    erase(s);
    resetTimes();
}

bool SortedStaves::compareStaves ( Symbol* a, Symbol* b )
{
    auto a_x = a->getMessage("/x").getFloat() ;
    auto a_y = a->getMessage("/y").getFloat() ;
    auto a_x2 = a_x + a->getMessage("/w").getFloat();
    
    
    auto b_x = b->getMessage("/x").getFloat() ;
    auto b_y = b->getMessage("/y").getFloat() ;
    // auto b_x2 = b_x + Symbol::getOSCValueAsFloat( b->getOSCMessageValue("/w") );
    
    /*
    cout << "sorting: " << a << " " << b << endl;
    cout << "\t a_x " << a_x << " a_y " << a_y << " a_x2 " << a_x2 << endl;;
    cout << "\t b_x " << b_x << " b_y " << b_y << endl;
    
    cout << "test a_y < b_y " << (a_y < b_y) << endl;
    cout << "test a_x2 < b_x " << (a_x2 < b_x) << endl;
    */
    
    if( a_y < b_y && b_x < a_x2 )
        return true;
    
    return false;
    
    /*
     {
     if( a_y == b_y && a_x == b_x )
     return 0;
     else
     return 1;
     }
     */
}

void SortedStaves::resetTimes()
{
    sort( staves.begin(), staves.end(), compareStaves );

    float time = 0.0f;
    for( auto it = staves.begin(); it != staves.end(); it++)
    {
        
        Symbol *sym = *it;
                
        float w = sym->getMessage("/w").getFloat();
        
        sym->addMessage( "/time/start", time );
        
        cout << "staff time " << time << " ";
        
        time += sym->pixelsToTime(w) ;
        
        sym->addMessage( "/time/duration", time );
        
        cout << time << endl;
        
    }
    
    
    
}

bool SortedStaves::addStaff( Symbol *s)
{
    if( s->getMessage("/type").getString() != "staff" )
        return 0;
    
    removeStaff(s);
    
    cout << "adding staff " << endl;
    staves.emplace_back(s);
    
    resetTimes();
    
    // probably not the right place to deal with names.. maybe duplicates should be allowed?
    
    cout << "setting staff name -- size: " << staves.size() << endl;
    string name = s->getMessage("/name").getString();
    if( name.empty() ) // for now allow  name == s->getID()
    {
        s->addMessage( "/name", "staff_" + to_string(staves.size() ) );
    }
    
    return 1;
    
}

Symbol *SortedStaves::getStaveAtTime(float time)
{
    
    if( staves.size() == 0 || time < 0 || time > staves.back()->getEndTime() )
        return NULL;
    
    // cout << "end time " << staves.back()->getEndTime() << endl;
    
    for( auto s : staves )
    {
        if( time >= s->getTime() && time <= s->getEndTime() )
            return s;
    }
    
    return NULL;
}

const Symbol *SortedStaves::getStaveByID( const String& id )
{
    if( staves.size() == 0 || id.isEmpty() )
        return NULL;
    
    for( auto s : staves )
    {
        if( s->getID() == id )
            return s;
    }
    
    return NULL;
}

StringArray SortedStaves::getStaveNames()
{
    StringArray names;
    
    for( auto s : staves )
    {
        names.add( s->getID() );
    }
    
    return names;
}
