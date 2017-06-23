
#include "TimePointArray.h"

void TimePointArray::printTimePoints()
{
    cout << "-------- timepoint list ----------" << endl;
    int count = 0;
    for( auto t : (*this) )
    {
        cout << "timepoint " << count << " " << t->time << " nsyms " << t->symbols_at_time.size() << endl;
        
        int symcount = 0;
        for( auto sym : t->symbols_at_time )
        {
            cout << symcount++ << " " << sym << " " << sym->getOSCBundle().size() << endl;
        }
        count++;
    }
}


void TimePointArray::removeSymbolTimePoints( Symbol *s)
{

    float start_t = s->getTime();
    
    bool match = false;
    int idx = getTimePointInsertIndex( start_t, match );
    
    if( !match )
    {
        cout << " could not find existing timepoint at time " << start_t << endl;
        printTimePoints();
        return;
    }
    
    float t = start_t;
    float end_t = s->getEndTime();
    
    cout << s << " " << t << " " << end_t << endl;
    
    vector<SymbolTimePoint *> points_to_remove;
    
    while ( idx < size() )
    {
        t = (*this)[idx]->time;
        
        if( t > end_t ) break;
        
        vector<Symbol*> *vec = &((*this)[idx]->symbols_at_time);

        // remove this symbol
        vec->erase( std::remove( vec->begin(), vec->end(), s), vec->end());

        // if no other symbol at this time point start or end here, we should fully delete it
        int other_start_or_end = 0;
        if( t == start_t || t == end_t )
        {
            for (auto it = vec->begin(); it != vec->end(); it++ )
            {
                cout << "start/end test for " << (*it) << " t " << t << " == " << (*it)->getTime() << " " << (*it)->getEndTime() << endl;
                if( (*it)->getTime() == t || (*it)->getEndTime() == t )
                    other_start_or_end = 1;
            }
            if (!other_start_or_end)
                points_to_remove.emplace_back( (*this)[idx] );

        }
        
        cout << "vector size " << vec->size() << " at " << t << " " << other_start_or_end  <<  " " << end_t << endl;
        if( vec->size() == 0 )
            points_to_remove.emplace_back( (*this)[idx] );
        
        idx++;
    }
    
    
    for (auto rm = points_to_remove.begin(); rm != points_to_remove.end(); ++rm)
    {
        removeObject( *rm, true );
    }
    
    printTimePoints();
}


void TimePointArray::addSymbolTimePoints( Symbol *s )
{
    float start_t = s->getTime();
    float end_t = s->getEndTime();
    
    addSymbol_atTime( s, start_t );
    addSymbol_atTime( s, end_t );
    
    printTimePoints();

}

void TimePointArray::addSymbol_atTime( Symbol *s, float time)
{
    bool match;
    int idx = getTimePointInsertIndex( time, match );
    if( match )
    {
        (*this)[ idx ]->addSymbol( s );
    }
    else
    {
        auto newTimePoint = insert( idx, new SymbolTimePoint( s, time ) );
        
        if( idx - 1 >= 0 )
        {
            auto prevTimePoint = (*this)[idx-1];

            auto vec = prevTimePoint->symbols_at_time;
            
            for( auto prev_s = vec.begin(); prev_s != vec.end(); prev_s++ )
            {
                if( s == *prev_s ) continue;
                
                if( (*prev_s)->hitTest( time ) )
                {
                    newTimePoint->addSymbol( *prev_s );
                }
                else if ( s->hitTest( prevTimePoint->time ) )
                {
                    prevTimePoint->addSymbol( s );
                }

            }
    
        }
    }
    
}

void TimePointArray::printBundle(OSCBundle bndl)
{
    std::cout << "\t==== TIMEPOINT OSC BUNDLE ====" << std::endl;
    for (auto osc : bndl )
    {
        OSCMessage msg = osc.getMessage();
        std::cout << "\t" << msg.getAddressPattern().toString();
        
        for (auto arg : msg )
        {
            if( arg.isString() )
                std::cout << " " << arg.getString();
            else if( arg.isFloat32() )
                std::cout << " " << (String)arg.getFloat32();
            else if( arg.isInt32() )
                std::cout << " " << (String)arg.getInt32();
            else if( arg.isBlob() )
                std::cout << " " << "blob";
        }
        
        std::cout << std::endl;
    }
    std::cout << "\t====-===-======-====" << std::endl;
    
}

odot_bundle* symbolBundleToOdot( const OSCBundle& osc )
{
    OSCWriter w ;
    w.writeBundle( osc );
    size_t size = w.getDataSize();
    
    odot_bundle *bundle = new odot_bundle;
    
    bundle->len = static_cast<long>(size);
    bundle->data = new char[size];
    std::memcpy(bundle->data, w.getData() ,size );
    
    return bundle;
}

odot_bundle *TimePointArray::timePointToOSC(const SymbolTimePoint *tpoint  )
{
    const vector<Symbol*> vec = tpoint->symbols_at_time;
    
    OSCBundle bndl;
    int count = 0;
    String prefix = "/symbolsAtTime/";
    for (auto s : vec )
    {
        // ignore symbols if after endpoint
        if( current_time <= s->getEndTime() )
        {
            auto s_bndl = s->getOSCBundle();

            for ( auto osc : s_bndl )
            {
                OSCMessage msg = osc.getMessage();
                
                String newaddr = prefix + String(count) + msg.getAddressPattern().toString();
                
                msg.setAddressPattern(newaddr);
                
                bndl.addElement(msg);

            }
            
            count++;
        }
        else
        {
//             cout << "skipped sym " << s << " endpt: " << s->getEndTime() << endl;
        }
    }
    
    return symbolBundleToOdot( bndl );
}

int TimePointArray::lookupTimePoint( float t )
{
    int idx = (current_point < 0 ) ? 0 : current_point;
    float p_time = current_time;

    if ( p_time < t ) // moving forward
    {
        // maybe check one step before doing the loop?
        
        for ( ; idx < size(); idx++ ) // step forward
        {
            p_time = (*this)[ idx ]->time;
            
            if (p_time > t) // if point time is >, we want the previous one
            {
                current_time = t;
                current_point = idx - 1;
                return current_point;
            }
        }
        
        if( idx >= size() )
            current_point = size() - 1;
        else
            cout << "shouldn't happen " << current_point <<  endl;
        
        current_time = t;
        return current_point;
        
    }
    else if ( p_time > t )
    {
        for ( ; idx >= 0; idx-- ) // step backward
        {
            p_time = (*this)[ idx ]->time;
            
            if (p_time < t) // if point time is <, we want the previous one
            {
                current_time = t;
                current_point = idx;
                return current_point;
            }
        }
        
        // if we run off the end, we are before the first time point
        current_point = idx;
        current_time = t;
        
        return current_point;
    }

    // otherwise, we're not moving
    current_time = t;
    return current_point;
}

odot_bundle *TimePointArray::getSymbolsAtTime( float t )
{
    if (size() == 0 )
        return nullptr;
    
    int idx = lookupTimePoint( t );
    current_point = idx;

//    cout << "timepoint number: " << current_point << endl;
    if( idx >= 0 )
    {
        auto tpoint = (*this)[idx];

      //  cout << "for " << t <<" closest timepoint: " << idx << " at " << tpoint->time << " with: " << tpoint->symbols_at_time.size() << " overlaping" << endl;
        
        return timePointToOSC( tpoint );
    }
    
    return nullptr;
}

int TimePointArray::getTimePointInsertIndex( float t, bool& match )
{
    match = false;
    
    int firstElement = 0, lastElement = size();
    while (firstElement < lastElement)
    {
        if (compareTimes ( (*this)[firstElement]->time, t ) == 0)
        {
            match = true;
            break;
        }
        else
        {
            const int halfway = (firstElement + lastElement) >> 1;
            
            if (halfway == firstElement)
            {
                if (compareTimes ((*this)[halfway]->time, t ) >= 0)
                {
                    ++firstElement;
                }
                break;
            }
            else if (compareTimes ((*this)[halfway]->time, t ) >= 0)
            {
                firstElement = halfway;
            }
            else
            {
                lastElement = halfway;
            }
        }
    }
    
    return firstElement;
}
