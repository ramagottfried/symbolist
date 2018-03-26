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
            cout << symcount++ << " " << sym << " " << sym->size() << endl;
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
        
        float staff_x = staff->getMessage("/x").getFloat();
        float staff_start_t = staff->getMessage("/time/start").getFloat();

        auto vec = t->symbols_at_time;
        
        float sym_start_t = 0;
        for( auto it = vec.begin(); it != vec.end(); it++ )
        {
            Symbol *s = *it;
            float start_x = s->getMessage("/x").getFloat() - staff_x;
            float dur_x = s->getMessage("/w").getFloat();
            
            sym_start_t = staff_start_t + s->pixelsToTime(start_x);
            
            s->setTimeAndDuration(sym_start_t, s->pixelsToTime(dur_x) );
        }
        
    }
    
    voice_staff_vector.clear();
    current_point = 0;
}

void TimePointArray::addSymbolTimePoints( Symbol *s )
{
    // 0) check if the symbol has a staff reference, if not ignore it
    // 1) if attached to a staff, calculate start & end times based on staff ( for now: start = x, end = x+w )
    // 2) add start and end points to array
    // 3) check previous start-1 and end-1 points for continuing symbols to add to new start/end points
    // 4) iterate forward from start to end point and add this symbol to all preexisting time points
    
    //cout << "TimePointArray::addSymbolTimePoints " << s << endl;
    
    string staff_name = s->getMessage( "/staff" ).getString();

    if( staff_name.size() == 0 )
        return;

    auto found_staves = score_ptr->getSymbolsByValue( "/id", staff_name );
    if( found_staves.isEmpty() )
        return;
    
    Symbol *staff = found_staves.getFirst();
    
    float staff_x = staff->getMessage("/x").getFloat();
    
    float start_x = s->getMessage("/x").getFloat() - staff_x;
    float dur_x = s->getMessage("/w").getFloat();
    float end_x = start_x + dur_x;

    float staff_start = staff->getMessage("/time/start").getFloat();
    
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
    current_point = 0;
    voice_staff_vector.clear();

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
                
                if( (*prev_s)->hitTestTime( time ) )
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


Point<float> TimePointArray::lookupPathPoint( const Symbol *s, const float t )
{
    string path_str = s->getMessage( "/path" ).getString();
    if( path_str.size() == 0 )
        return Point<float>();
    
    float length = s->getMessage( "/pathlength" ).getFloat();
    if( length == 0 )
        return Point<float>();
    
    Path p;
    p.restoreFromString( path_str.c_str() );
    
    return p.getPointAlongPath( t * length );
}
/*
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
*/

Point<float> TimePointArray::lookupPathPoint( const Symbol *s, string& path_base_addr , const float t )
{
    string path_str = s->getMessage( path_base_addr + "/str" ).getString();
    if( path_str.size() == 0 )
        return Point<float>();
    
    float length = s->getMessage( path_base_addr + "/length" ).getFloat();
    if( length == 0 )
        return Point<float>();
    
    Path p;
    p.restoreFromString( path_str.c_str() );
    
    return p.getPointAlongPath( t * length );
}

vector<const Symbol *> TimePointArray::getNoteOffs( const SymbolTimePoint *prev_tpoint , const SymbolTimePoint *tpoint   )
{
    
    vector<const Symbol *> off_vec;
    if( prev_tpoint != nullptr )
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
        
        for (size_t i = 0; i < prev_tpoint->symbols_at_time.size(); i++ )
        {
            auto prv = prev_tpoint->symbols_at_time[i];
            if( s == prv )
                return false;
        }
    }
    
    return true;
}

pair<size_t, int> TimePointArray::setNoteOff( const Symbol *s)
{
    size_t i = 0;
    for( ; i < voice_staff_vector.size(); i++ )
    {
        if(voice_staff_vector[i].first == s )
        {
            voice_staff_vector[i].first = NULL;
            return pair<size_t, bool>(i, -1); // was playing, and now is off
        }
    }
    
    return pair<size_t, bool>(-1, -1); // not found (already turned off)

}

pair<size_t, int> TimePointArray::getVoiceNumberState( const Symbol *s, const SymbolTimePoint *tpoint )
{
    size_t i = 0;
    for( ; i < voice_staff_vector.size(); i++ )
    {
        if(voice_staff_vector[i].first == s )
        {
            if( current_time > s->getEndTime() )
                return pair<size_t, bool>(i, -1); // was playing but is now past end
            else
                return pair<size_t, bool>(i, 0); // still playing

        }
    }
    
    // new voice, find first unused value in vector, and log the symbol and staff reference
    for( i = 0; i < voice_staff_vector.size(); i++ )
    {
        if(voice_staff_vector[i].first == NULL )
        {
            voice_staff_vector[i] = pair<const Symbol*, const Symbol*>( s, tpoint->staff_ref );
            return pair<size_t, bool>(i, 1); // new voice
        }
    }
    
    // no open voices, make a new one (i was incremented already by the loop)
    voice_staff_vector.emplace_back( pair<const Symbol*, const Symbol*>( s, tpoint->staff_ref ) );
    return pair<size_t, bool>(i, 1); // new voice
}

vector< tuple<size_t, const Symbol*, const Symbol*> > TimePointArray::getNoteOffs( const SymbolTimePoint *p )
{
    vector< tuple<size_t, const Symbol*, const Symbol*>> offs;
    
    if( p )
    {
        bool found;
        for( size_t i = 0; i < voice_staff_vector.size(); i++)
        {
            if( !voice_staff_vector[i].first )
                continue;
            
            found = false;
            for( auto s : p->symbols_at_time )
            {
                if( s == voice_staff_vector[i].first )
                {
                    found = true;
                    break;
                }
            }
            
            if( !found )
            {
                offs.emplace_back( tuple<size_t, const Symbol*, const Symbol*>(i, voice_staff_vector[i].first, voice_staff_vector[i].second ));
                voice_staff_vector[i].first = NULL;
            }
        }
    }
    else
    {
        for( size_t i = 0; i < voice_staff_vector.size(); i++)
        {
            if( !voice_staff_vector[i].first )
                continue;
            
            offs.emplace_back( tuple<size_t, const Symbol*, const Symbol*>(i, voice_staff_vector[i].first, voice_staff_vector[i].second ));
            voice_staff_vector[i].first = NULL;
        }
    }
    return offs;
}

void TimePointArray::groupLookup(const Symbol *s,
                                 const string& output_prefix,  // parent prefix to prepend
                                 double parent_x,
                                 double parent_y,
                                 float time_ratio,
                                 OdotBundle& bndl)
{
    //I'm not sure we need this groupsymbol address anymore since we're using subbundles now
    string group_name = "/" + s->getMessage("/name").getString();
    
    vector<OdotMessage> msg_array = s->getMessageArray();
    
    float this_x = parent_x, this_y = parent_y;
    
    // iterate group symbol bundle messages
    // adjust positions to be relative to the parent, and do path lookups
    for ( auto msg : msg_array )
    {
        string msg_addr = msg.getAddress();
        string newaddr = output_prefix + group_name + msg_addr;

        if( msg_addr == "/x" )
        {
            this_x = msg[0].getFloat() - parent_x;
            bndl.addMessage( newaddr, this_x );
            //cout << "pre subsymbol * added \t >> newaddr: " << newaddr << endl;
        }
        else if( msg_addr == "/y" )
        {
            this_y = msg[0].getFloat() - parent_y;
            bndl.addMessage( newaddr, this_y ) ;
            //cout << "pre subsymbol * added \t >> newaddr: " << newaddr << endl;
        }
        else if( msg_addr.find("/subsymbol") == 0 && msg[0].getType() == OdotAtom::O_ATOM_BUNDLE )
        {
            string type = s->getMessage("/type").getString();
            if( type == "group" )
            {
                groupLookup(s, output_prefix + group_name + msg_addr, this_x, this_y, time_ratio, bndl );
            }
            else // ** note: this requires that groups cannot contain higher level types of groups (staves, etc.) **
            {
                // if not a group, add the bundle with name prefix
                string subsymbol_name = s->getMessage("/name").getString();
                
                vector<OdotMessage> sub_msg_array = msg.getBundle().getMessageArray();
                for( auto sub_msg : sub_msg_array )
                {
                    sub_msg.rename( output_prefix + group_name + "/" + subsymbol_name + sub_msg.getAddress() );
                    bndl.addMessage( sub_msg );
                }
                
                if( type == "path" )
                {
                    // maybe move this part below to another function...
                    int npaths = s->getMessage( "/num_sub_paths" ).getInt();
                    for( int p_idx = 0; p_idx < npaths; p_idx++)
                    {
                        auto path_addr = "/path/" + to_string(p_idx);
                        auto xy = lookupPathPoint(s, path_addr, time_ratio );
                        
                        float w = s->getMessage( "/w" ).getFloat();
                        float h = s->getMessage( "/h" ).getFloat();
                        
                        if( w > 0 && h > 0 )
                        {
                            // use group name first, otherwise, if there is subsymbol_name name, use that.
                            if( !group_name.empty() || !subsymbol_name.empty() )
                                bndl.addMessage( output_prefix + group_name + msg_addr +  "/" + subsymbol_name + "/lookup/xy", xy.x / w, xy.y / h) ;
                            else
                                bndl.addMessage( output_prefix + msg_addr + "/path/" + to_string(p_idx) + "/lookup/xy", xy.x / w, xy.y / h) ;
                            
                        }
                    }
                }
                
            }
        }
        else
        {
            // cout << "not subsymbol " << msg_addr << endl;
            msg.rename( newaddr );
            bndl.addMessage( msg );
            //cout << "pre subsymbol * added \t >> newaddr: " << newaddr << endl;
        }
    }

    
}

OdotBundle_s TimePointArray::timePointStreamToOSC(const SymbolTimePoint *tpoint  )
{
    OdotBundle bndl;
    bndl.addMessage("/time/lookup", current_time ) ;
    bndl.addMessage("/time/end", getLast()->time ) ;
    
    string prefix = "/symbolsAtTime/";

    if( tpoint != nullptr )
    {
        const vector<Symbol*> vec = tpoint->symbols_at_time;
        
        int count = 0;
        for (auto s : vec )
        {

            const Symbol *staff = tpoint->staff_ref;
            string staff_name = staff->getName();

            string toplevel_name = s->getMessage( "/name" ).getString();
            
            if( current_time > s->getEndTime() )
            {
                pair<size_t, int> voice_num_state = setNoteOff( s );
                if( voice_num_state.first != -1 ) // voice number is set to -1 if not found
                {
                    string s_prefix = "/staff/" + staff->getName() + "/voice/" + to_string(voice_num_state.first) + "/" + toplevel_name;
                    bndl.addMessage( s_prefix + "/state", -1 );
                }
            }
            else
            {
                pair<size_t, int> voice_num_state = getVoiceNumberState( s, tpoint );
                string s_prefix = "/staff/" + staff->getName() + "/voice/" + to_string(voice_num_state.first) + "/" + toplevel_name;
                
                bndl.addMessage( s_prefix + "/state", voice_num_state.second ) ;
                // staff is already stored in timepoint so we could probably removed the staff check here...
                
                //String staff_name = "default";
                //String staff_id;
                float staff_x = 0, staff_y = 0;
                staff_x = staff->getMessage("/x").getFloat();
                staff_y = staff->getMessage("/y").getFloat();

                float time_ratio = (current_time - s->getTime()) / s->getDuration() ;
                bndl.addMessage( s_prefix + "/time/ratio", time_ratio );
                
                float offset_time = current_time - s->getTime();
            
                if( s->getType() == "path" )
                {
                    int npaths = s->getMessage( "/num_sub_paths" ).getInt();

                    for( int i = 0; i < npaths; i++)
                    {
                        auto path_addr = "/path/" + to_string(i);
                        auto xy = lookupPathPoint(s, path_addr, time_ratio );
                        bndl.addMessage( s_prefix + "/path/" + to_string(i) + "/lookup/xy", xy.x, xy.y );

                    }

                }
                else if( s->getType() == "group" )
                {
                    string group_prefix = "/staff/" + staff->getName() + "/voice/" + to_string(voice_num_state.first);
                    groupLookup(s, group_prefix, staff_x, staff_y, time_ratio, bndl );
                }
                else
                {
                
                    auto s_bndl_msgs = s->getMessageArray();

                    for ( auto msg : s_bndl_msgs )
                    {
                        
                        string msg_addr = msg.getAddress();
                        string newaddr = s_prefix + msg_addr;
                        
                        if( msg_addr == "/x" )
                        {
                            bndl.addMessage( newaddr, msg[0].getFloat() - staff_x ) ;
                        }
                        else if( msg_addr == "/y" )
                        {
                            bndl.addMessage( newaddr, msg[0].getFloat() - staff_y ) ;
                        }
                        else
                        {
                            msg.rename(newaddr);
                            bndl.addMessage(msg);
                        }
                    }
                }
                
                count++;
            }
           // else
            //{
    //            ;
    //             cout << "skipped sym " << s << " endpt: " << s->getEndTime() << endl;
            //}
        }
    }
    
    vector< tuple<size_t, const Symbol*, const Symbol*> > offs = getNoteOffs( tpoint );
    for( int i = 0; i < offs.size(); i++ )
    {
        string s_prefix = "/staff/" + get<2>(offs[i])->getName() + "/voice/" + to_string(get<0>(offs[i])) +"/"+ get<1>(offs[i])->getName();
        bndl.addMessage( s_prefix + "/state", -1 );
    }
    
    prev_timepoint = tpoint;

    return bndl.serialize();
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

OdotBundle_s TimePointArray::getSymbolsAtTime( float t )
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
