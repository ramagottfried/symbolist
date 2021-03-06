
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

void SortedStaves::erase( Symbol* s )
{
    staves.erase( remove( staves.begin(), staves.end(), s ), staves.end() );
}

void SortedStaves::removeStaff( Symbol* s)
{
    erase(s);
    resetTimes();
}

bool SortedStaves::compareStaves ( Symbol* a, Symbol* b )
{
    auto a_x = a->getMessage("/x").getFloat();
    auto a_y = a->getMessage("/y").getFloat();
    auto a_x2 = a_x + a->getMessage("/w").getFloat();
    
    
    auto b_x = b->getMessage("/x").getFloat() ;
    auto b_y = b->getMessage("/y").getFloat() ;
    
    return a_y < b_y && b_x < a_x2;
    
}

void SortedStaves::resetTimes()
{
    sort(staves.begin(), staves.end(), compareStaves);

    float time = 0.0f;
    for( auto it = staves.begin(); it != staves.end(); it++)
    {
        Symbol* staff = *it;
                
        float w = staff->getMessage("/w").getFloat();
        staff->addMessage( "/time/start", time );
        
        DEBUG_FULL("Staff's start time = " << time)
        time += staff->pixelsToTime(w) ;
        
        staff->addMessage( "/time/duration", time );
        DEBUG_INLINE(", end time = " << time << endl)
        
    }
    
}

bool SortedStaves::addStaff(Symbol* s)
{
    if( s->getMessage("/type").getString() != "staff" )
        return false;
    
    removeStaff(s);
    
    DEBUG_FULL("Adding staff " << endl)
    staves.emplace_back(s);
    
    resetTimes();
    
    // Probably not the right place to deal with names.. maybe duplicates should be allowed?
    DEBUG_FULL("Setting staff name -- staff size: " << staves.size() << endl)
    string name = s->getMessage("/name").getString();
    if( name.empty() ) // For now allow  name == s->getID()
        s->addMessage( "/name", "staff_" + to_string(staves.size()) );
    
    return true;
    
}

Symbol* SortedStaves::getStaveAtTime(float time)
{
    
    if( staves.size() == 0 || time < 0 || time > staves.back()->getEndTime() )
        return NULL;
    
    for( auto s : staves )
    {
        if( time >= s->getTime() && time <= s->getEndTime() )
            return s;
    }
    
    return NULL;
}

const Symbol* SortedStaves::getStaveByID( const String& id )
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
