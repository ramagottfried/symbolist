
#include "TimePointArray.h"
#include "Score.h"


void TimePointArray::printTimePoints()
{
    cout << "-------- timepoint list ----------" << endl;
    for(int i = 0; i < size(); i++ )
    {
        auto t = (*this)[i];
        cout << "timepoint " << i << " " << t->time << " nsyms " << t->symbols_at_time.size() << endl;
        
        int symcount = 0;
        for( auto sym : t->symbols_at_time )
        {
            cout << symcount++ << " " << sym << " " << sym->getOSCBundle()->size() << endl;
        }
    }
}

void TimePointArray::removeStaffAndSymbolTimePoints( Symbol *s )
{
    if( s->getType() == "staff" )
    {
        for( int i = 0; i < size(); i++ )
        {
            auto t = (*this)[i];
            if( t->staff_ref == s )
            {
                remove(i);
            }
        }
    }
    else
        removeSymbolTimePoints(s);
}

void TimePointArray::removeSymbolTimePoints( Symbol *s )
{

    float start_t = s->getTime();
    
    if( start_t == -1 )
    {
        //cout << " start time is not set skipping " << endl;
        return;
    }
    
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
    
    //cout << "\n\nremoving time points for " << s << " start " << t << " end " << end_t << " idx " << idx << " of " << size() << endl;
    
    vector<SymbolTimePoint *> points_to_remove;
    
    while ( idx < size() )
    {
        t = (*this)[idx]->time;
        
        if( t > end_t && !f_almost_equal( t, end_t ) )
        {
            //cout << " t " << t << " > or != " << end_t << endl;
            break;
        }
        
        vector<Symbol*> *vec = &((*this)[idx]->symbols_at_time);

        // remove this symbol
        vec->erase( std::remove( vec->begin(), vec->end(), s), vec->end());

        // if no other symbol at this time point start or end here, we should fully delete it
        int other_start_or_end = 0;
        if( f_almost_equal( t, start_t ) || f_almost_equal( t, end_t ) )
        {
            for (auto it = vec->begin(); it != vec->end(); it++ )
            {
//                cout << "start/end test for " << (*it) << " t " << t << " == " << (*it)->getTime() << " " << (*it)->getEndTime() << endl;
                if( f_almost_equal( (*it)->getTime(), t ) || f_almost_equal( (*it)->getEndTime(), t ) )
                    other_start_or_end = 1;
            }
            if (!other_start_or_end)
                points_to_remove.emplace_back( (*this)[idx] );

        }
        
        // cout << "vector size " << vec->size() << " at " << t << " " << other_start_or_end  <<  " " << end_t << endl;
        if( vec->size() == 0 )
            points_to_remove.emplace_back( (*this)[idx] );
        
        idx++;
    }
    
    
    for (auto rm = points_to_remove.begin(); rm != points_to_remove.end(); ++rm)
    {
        removeObject( *rm, true );
    }
    
  // printTimePoints();
}

// this is only reseting the /time/start for the symbols, not the timepoints, the timepoints should probably be deleted and then recreated, since we reset the symbol when it is modified in the score...
void TimePointArray::resetTimes()
{

    for( int i = 0; i < size(); i++ )
    {
        auto t = (*this)[i];
        
        Symbol *staff = t->staff_ref;
        
        float staff_x = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/x") );
        float staff_start_t = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/time/start") );

        auto vec = t->symbols_at_time;
        
        float sym_start_t = 0;
        for( auto it = vec.begin(); it != vec.end(); it++ )
        {
            Symbol *s = *it;
            float start_x = Symbol::getOSCValueAsFloat( s->getOSCMessageValue("/x") ) - staff_x;
            float dur_x = Symbol::getOSCValueAsFloat( s->getOSCMessageValue("/w") );
            
            sym_start_t = staff_start_t + s->pixelsToTime(start_x);
            
            s->setTimeAndDuration(sym_start_t, s->pixelsToTime(dur_x) );
        }
        
    }
}


void TimePointArray::addSymbolTimePoints( Symbol *s )
{
    // 0) check if the symbol has a staff reference, if not ignore it
    // 1) if attached to a staff, calculate start & end times based on staff ( for now: start = x, end = x+w )
    // 2) add start and end points to array
    // 3) check previous start-1 and end-1 points for continuing symbols to add to new start/end points
    // 4) iterate forward from start to end point and add this symbol to all preexisting time points
    
    //cout << "TimePointArray::addSymbolTimePoints " << s << endl;
    
    int staff_pos = s->getOSCMessagePos( "/staff" );
    if( staff_pos == -1 || s->getOSCMessageValue( staff_pos ).getString().isEmpty() )
        return;

    String staff_name = s->getOSCMessageValue( staff_pos ).getString();

    auto found_staves = score_ptr->getSymbolsByValue( "/id", staff_name );
    if( found_staves.isEmpty() )
        return;
    
    
    Symbol *staff = found_staves.getFirst();
    
    float staff_x = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/x") );
    
    float start_x = Symbol::getOSCValueAsFloat( s->getOSCMessageValue("/x") ) - staff_x;
    float dur_x = Symbol::getOSCValueAsFloat( s->getOSCMessageValue("/w") );
    float end_x = start_x + dur_x;

    float staff_start = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/time/start") );
    
    float start_t = staff_start + s->pixelsToTime(start_x);
    float end_t = staff_start + s->pixelsToTime(end_x);
    
    s->setTimeAndDuration(start_t, s->pixelsToTime(dur_x) );
    
    cout << "adding timepoints on " << staff_name << " " << staff_start << " t start " << start_t << endl;
    
    int start_idx = addSymbol_atTime( s, start_t, staff );
    int end_idx = addSymbol_atTime( s, end_t, staff );
    
    for( int i = (start_idx + 1); i < end_idx; i++ )
    {
        (*this)[ i ]->addSymbol( s );
    }
    
    // printTimePoints();

}

int TimePointArray::addSymbol_atTime( Symbol *s, float time, Symbol *staff)
{
    
    bool match;
    int idx = getTimePointInsertIndex( time, match );
    if( match )
    {
        (*this)[ idx ]->addSymbol( s ); // if it's an exact match we don't need to check the previous point
    }
    else
    {
        auto newTimePoint = insert( idx, new SymbolTimePoint( s, time, staff ) );  // otherwise, create new point and check previous for continuing points

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

Point<float> TimePointArray::lookupPathPoint( const Symbol *s, String& path_base_addr , const float t )
{
    OSCBundle b = *(s->getOSCBundle());
    
    String path_addr = path_base_addr + "/str" ;
    int path_oscpos = s->getOSCMessagePos( path_addr );
    if( s->symbol_parse_error( path_oscpos, path_addr) ) return Point<float>();
    
    
    String pathlen_addr = path_base_addr + "/length" ;
    int pathlen_oscpos = s->getOSCMessagePos( pathlen_addr );
    if( s->symbol_parse_error( pathlen_oscpos, pathlen_addr ) ) return Point<float>();
    
    
    String path_str = s->getOSCMessageValue(path_oscpos).getString();
    Path p;
    p.restoreFromString( path_str );
    
    float length = s->getOSCValueAsFloat( s->getOSCMessageValue(pathlen_oscpos) );
//    float path_time = ((t - start) / dur) * length;;
    //    cout << t << " " << ((t - start) / dur) << " " << length << endl;
    
    return p.getPointAlongPath( t * length );
}

vector<const Symbol *> TimePointArray::getNoteOffs( const SymbolTimePoint *prev_tpoint , const SymbolTimePoint *tpoint   )
{
    
    vector<const Symbol *> off_vec;
    if( prev_tpoint )
    {
        for (auto prv : prev_tpoint->symbols_at_time )
        {
            bool found = false;
            
            if( tpoint )
            {
                for( auto s : tpoint->symbols_at_time )
                {
                    
                    if( prv == s)
                    {
                        found = true;
                        break;
                    }
                }
            }
            
            if( !found )
            {
                off_vec.emplace_back( prv );
            }
        }
    }
    
    return off_vec;
}


bool TimePointArray::isNewSym( const Symbol *s , const SymbolTimePoint *prev_tpoint   )
{
    if( prev_tpoint )
    {
        for (int i = 0; i < prev_tpoint->symbols_at_time.size(); i++ )
        {
            auto prv = prev_tpoint->symbols_at_time[i];
            if( s == prv )
                return false;
        }
    }
    
    return true;
}


void TimePointArray::groupPathLookup( const Symbol *s, const String& output_prefix, const String& groupsymbol_addr, float time_ratio, OSCBundle& bndl )
{
    // all paths within group are read in terms of the time span of the top level group
    // paths are scaled in terms of their bounding box, *or* if the bounding box of the group containing them
    // --- in the case of path within a group within a group, the scaling would be in terms of the first containing group
    

    // for example subsymbol_addr could be "/subsymbol/1/subsymbol/2"

    // cout << "groupPathLookup " << groupsymbol_addr << endl;
    
    String group_name;
    int groupname_pos = s->getOSCMessagePos( groupsymbol_addr + "/name" );
    if( groupname_pos != -1 )
    {
        group_name = "/" + s->getOSCMessageValue(groupname_pos).getString();
    }
    
    int nsym_oscpos = s->getOSCMessagePos( groupsymbol_addr + "/numsymbols" );
    if( s->symbol_parse_error( nsym_oscpos, groupsymbol_addr + "/numsymbols") )
    {
        cout << "error number of grouped symbols not found" << endl;
    }
    else
    {
        int nsymbols = Symbol::getOSCValueAsInt( s->getOSCMessageValue(nsym_oscpos) );
        
        for( int subsym_idx = 0; subsym_idx < nsymbols; subsym_idx++)
        {
            String subsym_addr = groupsymbol_addr + "/subsymbol/" + String(subsym_idx+1);
            
            // cout << "    subsym_addr " << subsym_addr << endl;

            int pos = s->getOSCMessagePos( subsym_addr  + "/type" );
            if( pos != -1 )
            {
                if( s->getOSCMessageValue(pos).getString() == "path" )
                {
                    // maybe move this part below to another function...
                    int npath_oscpos = s->getOSCMessagePos( subsym_addr + "/num_sub_paths" );
                    if( s->symbol_parse_error( npath_oscpos, subsym_addr + "/num_sub_paths") )
                    {
                        cout << "error /num subpaths not found" << endl;
                    }
                    else
                    {
                        int npaths = Symbol::getOSCValueAsInt( s->getOSCMessageValue(npath_oscpos) );
                        
                        for( int p_idx = 0; p_idx < npaths; p_idx++)
                        {
                            auto path_addr = subsym_addr + "/path/" + String(p_idx);
                            auto xy = lookupPathPoint(s, path_addr, time_ratio );
                            
                            float w = 0, h = 0;
                            
                            int w_pos = s->getOSCMessagePos(groupsymbol_addr + "/w" );
                            if( w_pos != -1 )
                            {
                                w = s->getOSCValueAsFloat( s->getOSCMessageValue(w_pos) );
                            }
                            int h_pos = s->getOSCMessagePos(groupsymbol_addr + "/h" );
                            if( h_pos != -1 )
                            {
                                h = s->getOSCValueAsFloat( s->getOSCMessageValue(h_pos) );
                            }
                            
                            if( w > 0 && h > 0 )
                            {
                                
                                String path_name;
                                int name_pos = s->getOSCMessagePos(subsym_addr + "/name");
                                if( name_pos != -1 )
                                    path_name = "/" + s->getOSCMessageValue(name_pos).getString();
                                
//                                cout << "name: " << path_name << endl;
                                // use group name first, otherwise, if there is path name, use that.
                                if( group_name.isNotEmpty() || path_name.isNotEmpty() )
                                    bndl.addElement( OSCMessage( output_prefix + group_name + path_name + "/lookup/xy", xy.x / w, xy.y / h) );
                                else
                                    bndl.addElement( OSCMessage( output_prefix + "/path/" + (String)p_idx + "/lookup/xy", xy.x / w, xy.y / h) );
                                    

                            }
                            
                        }
                    }
                }
                else if( s->getOSCMessageValue(pos).getString() == "group" )
                {
                    groupPathLookup(s, output_prefix + group_name, subsym_addr, time_ratio, bndl );
                }
            }
            
        }
    }
}


odot_bundle *TimePointArray::timePointStreamToOSC(const SymbolTimePoint *tpoint  )
{
    OSCBundle bndl;
    bndl.addElement( OSCMessage("/time/lookup", (float)current_time));
    
    String prefix = "/symbolsAtTime/";

    if( tpoint != nullptr )
    {
        const vector<Symbol*> vec = tpoint->symbols_at_time;
        
        int count = 0;
        for (auto s : vec )
        {
            
            // ignore symbols if after endpoint
            if( current_time <= s->getEndTime() )
            {
                String staff_name;
                float staff_x = 0, staff_y = 0;
                
                int staff_pos = s->getOSCMessagePos( "/staff" );
                if( staff_pos != -1 )
                {
                    staff_name = s->getOSCMessageValue( staff_pos ).getString();
                    if( staff_name.isNotEmpty() )
                    {
                        // cout << "staff name " << staff_name << endl;
                        
                        String addr = "/id";
                        auto found_staves = score_ptr->getSymbolsByValue( addr, staff_name );
                        
                        if( found_staves.size() > 0 )
                        {
                            Symbol *staff = found_staves.getFirst();
                            
                            staff_x = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/x") );
                            staff_y = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/y") );
                        }
                    }
                }
                
                String s_prefix = "/staff/" + staff_name + "/" + String(count);
                
                float time_ratio = (current_time - s->getTime()) / s->getDuration() ;
                bndl.addElement( OSCMessage( s_prefix + "/time/ratio", time_ratio ) );
                
                float offset_time = current_time - s->getTime();

                String toplevel_name;
                int name_pos = s->getOSCMessagePos( "/name" );
                if( name_pos != -1 )
                {
                    toplevel_name = s->getOSCMessageValue(name_pos).getString();
                }
                
                
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
                                bndl.addElement( OSCMessage( s_prefix + "/path/" + (String)i + "/lookup/xy", xy.x, xy.y ) );
                            }

                        }
                    }

                }
                else if( s->getType() == "group" )
                {
                    groupPathLookup(s, s_prefix, String(), time_ratio, bndl);
                }
                
                auto s_bndl = *(s->getOSCBundle());

                for ( auto osc : s_bndl )
                {
                    OSCMessage msg = osc.getMessage();
                    
                    String msg_addr = msg.getAddressPattern().toString();
                    String newaddr = s_prefix + "/" + toplevel_name + msg_addr;

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
                
                if ( isNewSym(s, prev_timepoint ) )
                    bndl.addElement( OSCMessage( s_prefix + "/state", 1 ) );
                else
                    bndl.addElement( OSCMessage( s_prefix + "/state", 0 ) );

                
                count++;
            }
            else
            {
    //             cout << "skipped sym " << s << " endpt: " << s->getEndTime() << endl;
            }
        }
    }
    
    auto offs = getNoteOffs( prev_timepoint, tpoint );
    for( int i = 0; i < offs.size(); i++ )
    {
        String s_prefix = prefix + String(i);
        bndl.addElement( OSCMessage( s_prefix + "/state", -1 ) );
    }
    
    prev_timepoint = tpoint;
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
