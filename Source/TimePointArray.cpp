#include "TimePointArray.h"

void TimePointArray::printTimePoints()
{
    cout << "-------- timepoint list ----------" << endl;
    for (int i = 0; i < symbol_time_points.size(); i++)
    {
        auto t = symbol_time_points[i];
        cout << "Timepoint n°" << i << ", time = " << t.time << ", nsyms = " << t.symbols_at_time.size() << endl;
        
        int symcount = 0;
        for( auto sym : t.symbols_at_time )
        {
           cout << "\tSymbol n°" << (symcount++) << ", address = " << sym.get_o_ptr() << ", start = " << sym.getMessage("/time/start").getFloat() << ", end = " << sym.getMessage("/time/end").getFloat() << endl;
        }
    }
}

int TimePointArray::addSymbol(OdotBundle& symbol, OdotBundle& staff)
{
    
    float time = symbol.getMessage("/time/start").getFloat();
    
    bool match;
    int insertIndex = getTimePointInsertIndex(time, match);
    
    if (match)
        // If it's an exact match we don't need to check the previous point.
        symbol_time_points[insertIndex].addSymbol(symbol);
    else
    {
        // Otherwise, create new point and check previous for continuing points
        auto newTimePoint = symbol_time_points.insert( symbol_time_points.begin() + insertIndex,
                                                       SymbolTimePoint(symbol, time, staff) );

        if( insertIndex - 1 >= 0 )
        {
            auto prevTimePoint = symbol_time_points[ insertIndex - 1 ];

            auto prevTimePoint_vec = prevTimePoint.symbols_at_time;
			
            /* Adds to the new time point all symbols beginning before and ending
             * after the specified time.
             */
            for (auto& symbolAtPreviousTime : prevTimePoint_vec )
            {
                if (symbol == symbolAtPreviousTime) continue;
				
                /* Checks if time is between the start and end timex
				 * of the symbol at time point n-1
                 */
                if ( timeHitTest( symbolAtPreviousTime, time ) )
                    newTimePoint->addSymbol( symbolAtPreviousTime );
            }
    
        }
    }
	
    return insertIndex;
    
}

bool TimePointArray::isSymbolInTimePoint( OdotBundle& symbol , const SymbolTimePoint& timePoint )
{
    if( timePoint.size() > 0 )
    {
        for (size_t i = 0; i < timePoint.size(); i++ )
        {
            if( symbol == timePoint.symbols_at_time[i] )
                return true;
        }
    }
    
    return false;
}


vector<float> TimePointArray::lookupPathPoint( const OdotBundle& symbol, const float t )
{
    string path_str = symbol.getMessage( "/path" ).getString();
    if( path_str.size() == 0 )
        return vector<float>();
    
    float length = symbol.getMessage( "/pathlength" ).getFloat();
    if( length == 0 )
        return vector<float>();

    /* this could be optimized */
    /* removing JUCE reference:
     
     Path p;
     p.restoreFromString( path_str.c_str() );
     
     return p.getPointAlongPath( t * length );
     */
    return vector<float>();
    
}
/*
Point<float> TimePointArray::lookupPathPoint( const OdotBundle& symbol, const int pathIDX, const float t, const float start, const float dur )
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

vector<float> TimePointArray::lookupPathPoint( const OdotBundle& symbol, string& path_base_addr , const float t )
{
    string path_str = symbol.getMessage( path_base_addr + "/str" ).getString();
    if( path_str.size() == 0 )
        return vector<float>();
    
    float length = symbol.getMessage( path_base_addr + "/length" ).getFloat();
    if( length == 0 )
        return vector<float>();
    
    /* this could be optimized */
    /* removing JUCE reference:
    
    Path p;
    p.restoreFromString( path_str.c_str() );
    
    return p.getPointAlongPath( t * length );
     */
    
    return vector<float>();
}


vector<const OdotBundle >
TimePointArray::getNoteOffs( const SymbolTimePoint& prev_tpoint , const SymbolTimePoint& tpoint   )
{
    vector<const OdotBundle > off_vec;
    if( prev_tpoint.size() > 0 )
    {
        for (auto& prv : prev_tpoint.symbols_at_time )
        {
            bool found = false;
            
            if (tpoint.size() > 0 )
                for (auto& s : tpoint.symbols_at_time)
                    if (prv == s)
                    {
                        found = true;
                        break;
                    }
            
            if( !found )
                off_vec.emplace_back( prv );
        }
    }
    
    return off_vec;
}

pair<size_t, int> TimePointArray::setNoteOff(const OdotBundle& symbol)
{
    
    for(size_t i = 0; i < voice_staff_vector.size(); i++ )
    {
        if(voice_staff_vector[i].first == symbol )
        {
            voice_staff_vector[i].first.clear();
            return pair<size_t, bool>(i, -1); // was playing, and now is off
        }
    }
    
    return pair<size_t, bool>(-1, -1); // not found (already turned off)

}

pair<size_t, int> TimePointArray::getVoiceNumberState(const OdotBundle& symbol, const SymbolTimePoint& tpoint)
{
    size_t i = 0;
    for( ; i < voice_staff_vector.size(); i++ )
    {
        if(voice_staff_vector[i].first == symbol )
        {
            if( current_time > symbol.getMessage("/time/end").getFloat() )
                return pair<size_t, bool>(i, -1); // was playing but is now past end
            else
                return pair<size_t, bool>(i, 0); // still playing

        }
    }
    
    // new voice, find first unused value in vector, and log the symbol and staff reference
    for( i = 0; i < voice_staff_vector.size(); i++ )
    {
        if(voice_staff_vector[i].first.size() == 0 )
        {
            voice_staff_vector[i] = make_pair(symbol, tpoint.staff_ref);
            return pair<size_t, bool>(i, 1); // new voice
        }
    }
    
    // no open voices, make a new one (i was incremented already by the loop)
    voice_staff_vector.emplace_back( make_pair( symbol, tpoint.staff_ref ) );
    return pair<size_t, bool>(i, 1); // new voice
}

vector<TimePointArray::ID_SYM_STAFF>
TimePointArray::getNoteOffs(const SymbolTimePoint& p)
{
    vector<ID_SYM_STAFF> offs;
    
    if(p.size() > 0)
    {
        bool found;
        for( size_t i = 0; i < voice_staff_vector.size(); i++)
        {
            if( voice_staff_vector[i].first.size() == 0 )
                continue;
            
            found = false;
            for( auto s : p.symbols_at_time )
            {
                if( s == voice_staff_vector[i].first )
                {
                    found = true;
                    break;
                }
            }
            
            if(!found)
            {
                offs.emplace_back( ID_SYM_STAFF(i, voice_staff_vector[i].first, voice_staff_vector[i].second) );
                voice_staff_vector[i].first.clear();
            }
        }
    }
    else
    {
        for(size_t i = 0; i < voice_staff_vector.size(); i++)
        {
            if(voice_staff_vector[i].first.size() == 0)
                continue;
            
            offs.emplace_back( ID_SYM_STAFF(i, voice_staff_vector[i].first, voice_staff_vector[i].second) );
            voice_staff_vector[i].first.clear();
        }
    }
    return offs;
}

void TimePointArray::groupLookup(const OdotBundle& symbol,
                                 const string& output_prefix,  // parent prefix to prepend
                                 double parent_x,
                                 double parent_y,
                                 float time_ratio,
                                 OdotBundle& bndl)
{
    //I'm not sure we need this groupsymbol address anymore since we're using subbundles now
    string group_name = "/" + symbol.getMessage("/name").getString();
    
    vector<OdotMessage> msg_array = symbol.getMessageArray();
    
    float this_x = parent_x, this_y = parent_y;
    
    // iterate group symbol bundle messages
    // adjust positions to be relative to the parent, and do path lookups
    for ( auto& msg : msg_array )
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
            string type = symbol.getMessage("/type").getString();
            if( type == "group" )
            {
                groupLookup(symbol, output_prefix + group_name + msg_addr, this_x, this_y, time_ratio, bndl );
            }
            else // ** note: this requires that groups cannot contain higher level types of groups (staves, etc.) **
            {
                // if not a group, add the bundle with name prefix
                string subsymbol_name = symbol.getMessage("/name").getString();
                
                vector<OdotMessage> sub_msg_array = msg.getBundle().getMessageArray();
                for( auto sub_msg : sub_msg_array )
                {
                    sub_msg.rename( output_prefix + group_name + "/" + subsymbol_name + sub_msg.getAddress() );
                    bndl.addMessage( sub_msg );
                }
                
                if( type == "path" )
                {
                    // maybe move this part below to another function...
                    int npaths = symbol.getMessage( "/num_sub_paths" ).getInt();
                    for( int p_idx = 0; p_idx < npaths; p_idx++)
                    {
                        auto path_addr = "/path/" + to_string(p_idx);
                        auto xy = lookupPathPoint(symbol, path_addr, time_ratio );
                        
                        float w = symbol.getMessage( "/w" ).getFloat();
                        float h = symbol.getMessage( "/h" ).getFloat();
                        
                        if( w > 0 && h > 0 )
                        {
                            // use group name first, otherwise, if there is subsymbol_name name, use that.
                            if( !group_name.empty() || !subsymbol_name.empty() )
                                bndl.addMessage( output_prefix + group_name + msg_addr +  "/" + subsymbol_name + "/lookup/xy", xy[0] / w, xy[1] / h) ;
                            else
                                bndl.addMessage( output_prefix + msg_addr + "/path/" + to_string(p_idx) + "/lookup/xy", xy[0] / w, xy[1] / h) ;
                            
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


OdotBundle TimePointArray::getSymbolsAtTime( float t )
{
    if (symbol_time_points.size() == 0 )
        return OdotBundle();
    
    /* note: lookupTimePoint() also sets current_time */
    int idx = lookupTimePoint( t );
    current_point = idx;
    
    if( idx >= 0 )
    {
        SymbolTimePoint& tpoint = symbol_time_points[idx];
        
        return timePointStreamToOSC( tpoint );
    }
    
    return timePointStreamToOSC( SymbolTimePoint() );
}

OdotBundle TimePointArray::timePointStreamToOSC(const SymbolTimePoint& tpoint  )
{
    OdotBundle bndl;
    bndl.addMessage("/time/lookup", current_time ) ;
    bndl.addMessage("/time/end", m_duration ) ;
    
    string prefix = "/symbolsAtTime/";

    if( tpoint.size() > 0 )
    {
        const vector<OdotBundle>& symbolsAtTPoint = tpoint.symbols_at_time;
        
        int count = 0;
        for (auto symbol : symbolsAtTPoint )
        {
			/* If the current symbol posseses a /expr odot message
		     * then apply the content (normally odot expressions) on it.
			 */
			if (symbol.addressExists("/expr"))
                symbol.applyExpr( symbol.getMessage("/expr").getString() );
			
            const OdotBundle& staff = tpoint.staff_ref;
            string staffName = staff.getMessage("/name").getString();

            string toplevelName = symbol.getMessage( "/name" ).getString();
            
            if( current_time > symbol.getMessage("/time/end").getFloat() )
            {
                pair<size_t, int> voiceNumState = setNoteOff( symbol );
                if( voiceNumState.first != -1 ) // voice number is set to -1 if not found
                {
                    string symbolPrefix = "/staff/" + staffName + "/voice/" + to_string(voiceNumState.first) + "/" + toplevelName;
                    bndl.addMessage( symbolPrefix + "/state", -1 );
                }
            }
            else
            {
                pair<size_t, int> voiceNumState = getVoiceNumberState( symbol, tpoint );
                string symbolPrefix = "/staff/" + staffName + "/voice/" + to_string(voiceNumState.first) + "/" + toplevelName;
                
                bndl.addMessage( symbolPrefix + "/state", voiceNumState.second ) ;
                // staff is already stored in timepoint so we could probably removed the staff check here...
                
                //String staff_name = "default";
                //String staff_id;
                float staffXCoordinate = 0, staffYCoordinate = 0;
                staffXCoordinate = staff.getMessage("/x").getFloat();
                staffYCoordinate = staff.getMessage("/y").getFloat();

                float starttime = symbol.getMessage("/time/start").getFloat();
                float endtime = symbol.getMessage("/time/end").getFloat();
                float duration = endtime - starttime;
                
                float timeRatio = ( current_time - starttime ) / duration ;
                bndl.addMessage( symbolPrefix + "/time/ratio", timeRatio );
                
                const string& type = symbol.getMessage("/type").getString();
                
                if( type == "path" )
                {
                    int npaths = symbol.getMessage( "/num_sub_paths" ).getInt();

                    for( int i = 0; i < npaths; i++)
                    {
                        auto path_addr = "/path/" + to_string(i);
                        auto xy = lookupPathPoint(symbol, path_addr, timeRatio );
                        bndl.addMessage( symbolPrefix + "/path/" + to_string(i) + "/lookup/xy", xy[0], xy[1] );

                    }

                }
                else if( type == "group" )
                {
                    string group_prefix = "/staff/" + staffName + "/voice/" + to_string(voiceNumState.first);
                    groupLookup(symbol, group_prefix, staffXCoordinate, staffYCoordinate, timeRatio, bndl );
                }
                else
                {
                
                    auto symbolBundleMessages = symbol.getMessageArray();

                    for ( auto msg : symbolBundleMessages )
                    {
                        
                        string msgAddress = msg.getAddress();
                        string newaddr = symbolPrefix + msgAddress;
						
                        if( msgAddress == "/x" )
                        {
                        	// COMMENT THIS FOR NOW, FOR MASTER ORAL PRESENTATION.
                            bndl.addMessage( newaddr, msg[0].getFloat() - staffXCoordinate ) ;
                        }
                        else if( msgAddress == "/y" )
                        {
                        	// COMMENT THIS FOR NOW, FOR MASTER ORAL PRESENTATION.
                        	// Symbols which are above the staff have positive /y values.
                            bndl.addMessage( newaddr, staffYCoordinate - msg[0].getFloat());
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
        }
    }
    
    vector< ID_SYM_STAFF > offs = getNoteOffs(tpoint);
    for( int i = 0; i < offs.size(); i++ )
    {
        string s_prefix = "/staff/" +
                            get<2>(offs[i]).getMessage("/name").getString() +
                            "/voice/" + to_string(get<0>(offs[i])) +
                            "/"+ get<1>(offs[i]).getMessage("/name").getString();
        
        bndl.addMessage( s_prefix + "/state", -1 );
    }
    
    prev_timepoint = tpoint;

    return bndl;
}

int TimePointArray::lookupTimePoint( float t )
{
    int idx = (current_point < 0 ) ? 0 : current_point;
    float p_time = current_time;

    if ( p_time < t ) // moving forward
    {
        // maybe check one step before doing the loop?
        
        for ( ; idx < symbol_time_points.size(); idx++ ) // step forward
        {
            p_time = symbol_time_points[idx].time;
            
            if (p_time > t) // if point time is >, we want the previous one
            {
                current_time = t;
                current_point = idx - 1;
                return current_point;
            }
        }
        
        if( idx >= symbol_time_points.size() )
            current_point = static_cast<int>(symbol_time_points.size() - 1);
        else
            cout << "Shouldn't happen " << current_point << endl;
        
        current_time = t;
        return current_point;
        
    }
    else if ( p_time > t )
    {
        for ( ; idx >= 0; idx-- ) // step backward
        {
            p_time = symbol_time_points[idx].time;
            
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


int TimePointArray::getTimePointInsertIndex( float timeToMatch, bool& match )
{
    match = false;
    
    int firstElement = 0, lastElement = static_cast<int>(symbol_time_points.size());
    while (firstElement < lastElement)
    {
        if (compareTimes(symbol_time_points[firstElement].time, timeToMatch) == 0)
        {
            match = true;
            break;
        }
        else
        {
        	// Equivalent to "(firstElement + lastElement) / 2"
            const int halfway = (firstElement + lastElement) >> 1;
			
            /* If true no match is found, but firstElement carries the index of
             * the time point the nearest from timeToMatch.
             */
            if (halfway == firstElement)
            {
                if (compareTimes(symbol_time_points[halfway].time, timeToMatch) >= 0)
                {
                    ++firstElement;
                }
                break;
            }
            else if (compareTimes(symbol_time_points[halfway].time, timeToMatch) >= 0)
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
