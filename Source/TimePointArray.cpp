
#include "TimePointArray.h"
#include "Score.h"


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
            cout << symcount++ << " " << sym << " " << sym->getOSCBundle()->size() << endl;
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
    
//    cout << s << " " << t << " " << end_t << endl;
    
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
              //  cout << "start/end test for " << (*it) << " t " << t << " == " << (*it)->getTime() << " " << (*it)->getEndTime() << endl;
                if( (*it)->getTime() == t || (*it)->getEndTime() == t )
                    other_start_or_end = 1;
            }
            if (!other_start_or_end)
                points_to_remove.emplace_back( (*this)[idx] );

        }
        
    //    cout << "vector size " << vec->size() << " at " << t << " " << other_start_or_end  <<  " " << end_t << endl;
        if( vec->size() == 0 )
            points_to_remove.emplace_back( (*this)[idx] );
        
        idx++;
    }
    
    
    for (auto rm = points_to_remove.begin(); rm != points_to_remove.end(); ++rm)
    {
        removeObject( *rm, true );
    }
    
//    printTimePoints();
}


void TimePointArray::addSymbolTimePoints( Symbol *s )
{
    // 0) check if the symbol has a staff reference, if not ignore it
    // 1) if attached to a staff, calculate start & end times based on staff ( for now: start = x, end = x+w )
    // 2) add start and end points to array
    // 3) check previous start-1 and end-1 points for continuing symbols to add to new start/end points
    // 4) iterate forward from start to end point and add this symbol to all preexisting time points
    
    
    
    // for the staff process, we might want to look this up here
/*
    int staff_pos = s->getOSCMessagePos( "/staff" );
    if( staff_pos != -1 )
    {
        String staff_name = s->getOSCMessageValue( staff_pos ).getString();
        if( staff_name.isNotEmpty() )
        {
            String addr = "/name";
            auto found_staves = score_ptr->getSymbolsByValue( addr, staff_name );
            
            Symbol *staff = found_staves.getFirst();
            
            staff_x = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/x") );
            staff_y = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/y") );
            
        }
    }
  */
    
    
    int staff_pos = s->getOSCMessagePos( "/staff" );
    if( staff_pos == -1 || s->getOSCMessageValue( staff_pos ).getString().isEmpty() ) return;
    
    float start_t = s->getTime();
    float end_t = s->getEndTime();
    
    int start_idx = addSymbol_atTime( s, start_t );
    int end_idx = addSymbol_atTime( s, end_t );
    
    for( int i = (start_idx + 1); i < end_idx; i++ )
    {
        (*this)[ i ]->addSymbol( s );
    }
    
    
//    printTimePoints();

}

int TimePointArray::addSymbol_atTime( Symbol *s, float time)
{
    
    bool match;
    int idx = getTimePointInsertIndex( time, match );
    if( match )
    {
        (*this)[ idx ]->addSymbol( s ); // if it's an exact match we don't need to check the previous point
    }
    else
    {
        auto newTimePoint = insert( idx, new SymbolTimePoint( s, time ) );  // otherwise, create new point and check previous for continuing points

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
           /*     else if ( s->hitTest( prevTimePoint->time ) )
                {
                    prevTimePoint->addSymbol( s );
                }
            */
            }
    
        }
    }
    return idx;
    
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

Point<float> TimePointArray::lookupPathPoint( const Symbol *s, const float t )
{
    OSCBundle b = *s->getOSCBundle();
    
    int path_oscpos = s->getOSCMessagePos("/path");
    if( s->symbol_parse_error( path_oscpos, "/path" ) ) return Point<float>();
    
    int pathlen_oscpos = s->getOSCMessagePos("/pathlength");
    if( s->symbol_parse_error( pathlen_oscpos, "/pathlength" ) ) return Point<float>();
    
    
    String path_str = b[path_oscpos].getMessage()[0].getString();
    Path p;
    p.restoreFromString( path_str );

    float length = s->getOSCValueAsFloat( b[pathlen_oscpos].getMessage()[0] );
    float path_time = t * length;;
    
    return p.getPointAlongPath( path_time );
}

Point<float> TimePointArray::lookupPathPoint( const Symbol *s, const int pathIDX, const float t, const float start, const float dur )
{
    OSCBundle b = *(s->getOSCBundle());
    
    String path_addr = "/path/" + String(pathIDX) + "/str" ;
    int path_oscpos = s->getOSCMessagePos( path_addr );
    if( s->symbol_parse_error( path_oscpos, path_addr) ) return Point<float>();
    
    String pathlen_addr = "/path/" + String(pathIDX) + "/length" ;
    int pathlen_oscpos = s->getOSCMessagePos( pathlen_addr );
    if( s->symbol_parse_error( pathlen_oscpos, pathlen_addr ) ) return Point<float>();

    
    String path_str = s->getOSCMessageValue(path_oscpos).getString();
    Path p;
    p.restoreFromString( path_str );

    float length = s->getOSCValueAsFloat( s->getOSCMessageValue(pathlen_oscpos) );
    float path_time = ((t - start) / dur) * length;;
//    cout << t << " " << ((t - start) / dur) << " " << length << endl;

    return p.getPointAlongPath( path_time );
}

odot_bundle *TimePointArray::timePointStreamToOSC(const SymbolTimePoint *tpoint  )
{
    OSCBundle bndl;
    bndl.addElement( OSCMessage("/time/lookup", (float)current_time));
    
    if( tpoint != nullptr )
    {
        const vector<Symbol*> vec = tpoint->symbols_at_time;

        int count = 0;
        String prefix = "/symbolsAtTime/";
        for (auto s : vec )
        {
            // ignore symbols if after endpoint
            if( current_time <= s->getEndTime() )
            {
                
                float staff_x = 0, staff_y = 0;
                
                int staff_pos = s->getOSCMessagePos( "/staff" );
                if( staff_pos != -1 )
                {
                    String staff_name = s->getOSCMessageValue( staff_pos ).getString();
                    if( staff_name.isNotEmpty() )
                    {
                        String addr = "/name";
                        auto found_staves = score_ptr->getSymbolsByValue( addr, staff_name );
                        
                        Symbol *staff = found_staves.getFirst();
                        
                        staff_x = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/x") );
                        staff_y = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/y") );
                        
                    }
                }
                
                
                String s_prefix = prefix + String(count);

                float offset_time = current_time - s->getTime();

                if( s->getType() == "path" )
                {
                    // maybe move this part below to another function...
                    int npath_oscpos = s->getOSCMessagePos( "/num_sub_paths" );
                    if( s->symbol_parse_error( npath_oscpos, "/num_sub_paths") )
                    {
                        cout << "error /num subpaths not found" << endl;
                    }
                    else
                    {
                        int npaths = Symbol::getOSCValueAsInt( s->getOSCMessageValue(npath_oscpos) );

                        OSCBundle b = *(s->getOSCBundle());
                        
                        for( int i = 0; i < npaths; i++)
                        {
                            
                            String start_addr = "/path/" + String(i) + "/time/start" ;
                            int start_oscpos = s->getOSCMessagePos( start_addr );
                            s->symbol_parse_error( start_oscpos, start_addr );
                            
                            float start = s->getOSCValueAsFloat( s->getOSCMessageValue(start_oscpos) );
                            
                            String duration_addr = "/path/" + String(i) + "/time/duration" ;
                            int dur_oscpos = s->getOSCMessagePos( duration_addr );
                            s->symbol_parse_error( dur_oscpos, duration_addr );
                            
                            float dur = s->getOSCValueAsFloat( s->getOSCMessageValue(dur_oscpos) );
                            
                            if (offset_time >= start && offset_time < (start + dur) )
                            {
                                auto xy = lookupPathPoint( s, i, offset_time, start, dur );
                                bndl.addElement( OSCMessage( s_prefix + "/path/" + (String)i + "/lookup/xy", xy.x - staff_x, xy.y - staff_y ) );
                            }

                        }
                    }

                }
                float time_ratio = (current_time - s->getTime()) / s->getDuration() ;
                bndl.addElement( OSCMessage( s_prefix + "/time/ratio", time_ratio ) );
                
                auto s_bndl = *(s->getOSCBundle());

                for ( auto osc : s_bndl )
                {
                    OSCMessage msg = osc.getMessage();
                    
                    String msg_addr = msg.getAddressPattern().toString();
                    String newaddr = s_prefix + msg_addr;

                    if( msg_addr == "/x" )
                    {
                        bndl.addElement( OSCMessage( newaddr, Symbol::getOSCValueAsFloat( msg[0] ) - staff_x )  );
                    }
                    else if( msg_addr == "/y" )
                    {
                        bndl.addElement( OSCMessage( newaddr, Symbol::getOSCValueAsFloat( msg[0] ) - staff_y )  );
                    }
                    else
                    {
                        msg.setAddressPattern(newaddr);
                        bndl.addElement(msg);
                    }
                    

                }
                
                count++;
            }
            else
            {
    //             cout << "skipped sym " << s << " endpt: " << s->getEndTime() << endl;
            }
        }
    }
    //printBundle(bndl);
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
            
            if (p_time < t) // if point time is <, we want this one
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
    
    int idx = lookupTimePoint( t ); //<< also sets current time
    current_point = idx;

    if( idx >= 0 )
    {
        SymbolTimePoint *tpoint = (*this)[idx];
        
        return timePointStreamToOSC( tpoint );
    }
    
    return timePointStreamToOSC( nullptr );
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
