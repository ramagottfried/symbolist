
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

void TimePointArray::groupLookup( const Symbol *s,
                                 const String& output_prefix,
                                 const String& groupsymbol_addr,
                                 double parent_x,
                                 double parent_y,
                                 float time_ratio,
                                 OSCBundle& bndl)
{

    // s                = main root symbol (not subbundle)
    
    // output_prefix    = prefix to be added to this level (previous level prefix + group name culled from the calling function)
    
    // groupsymbol_addr = root name for this level of group (i.e. if a group contains another group:  /group/subsymbol/0/ would be the root address for the sub group at /subsymbol/0
    
    // time ratio       = toplevel group time point
    
    // bndl             = toplevel bundle to write into
    
    
    // all paths within group are read in terms of the time span of the top level group
    // paths are scaled in terms of their bounding box, *or* if the bounding box of the group containing them
    // --- in the case of path within a group within a group, the scaling would be in terms of the first containing group
    

    // for example subsymbol_addr could be "/subsymbol/1/subsymbol/2"

    // cout << "groupPathLookup " << groupsymbol_addr << endl;
    
    
  //  cout << "in output_prefix" << output_prefix << " groupsymbol_addr " << groupsymbol_addr << endl;
    
    String group_name;
    int groupname_pos = s->getOSCMessagePos( groupsymbol_addr + "/name" );
    if( groupname_pos != -1 )
    {
        group_name = "/" + s->getOSCMessageValue(groupname_pos).getString();
    }
    
    OSCBundle s_bndl = *(s->getOSCBundle());
    
    float this_x = parent_x, this_y = parent_y;
    
    for ( auto osc : s_bndl )
    {
        OSCMessage msg = osc.getMessage();
        String msg_addr = msg.getAddressPattern().toString();
        
        if( groupsymbol_addr.isNotEmpty() && !msg_addr.startsWith(groupsymbol_addr) )
        {
            continue;
        }
        
        String newaddr = output_prefix + group_name + msg_addr.fromFirstOccurrenceOf(groupsymbol_addr, false, true);;
       /*
        cout << "pre subsymbol * " << " output prefix: " <<  output_prefix << endl;;
        cout << "pre subsymbol * \t group_name " <<  group_name<< endl;
        cout << "pre subsymbol * \t msg_addr: " << msg_addr<< endl;
        */
        if( msg_addr == "/x" )
        {
            this_x = Symbol::getOSCValueAsFloat( msg[0] ) - parent_x;
            bndl.addElement( OSCMessage( newaddr, this_x ) );
            //cout << "pre subsymbol * added \t >> newaddr: " << newaddr << endl;
        }
        else if( msg_addr == "/y" )
        {
            this_y = Symbol::getOSCValueAsFloat( msg[0] ) - parent_y;
            bndl.addElement( OSCMessage( newaddr, this_y ) );
            //cout << "pre subsymbol * added \t >> newaddr: " << newaddr << endl;
        }
        else if( !msg_addr.startsWith(groupsymbol_addr + "/subsymbol") )
        {
            // cout << "not subsymbol " << msg_addr << endl;
            msg.setAddressPattern(newaddr);
            bndl.addElement(msg);
            //cout << "pre subsymbol * added \t >> newaddr: " << newaddr << endl;
        }
    }

    
    
    
    int nsym_oscpos = s->getOSCMessagePos( groupsymbol_addr + "/numsymbols" );
    if( s->symbol_parse_error( nsym_oscpos, groupsymbol_addr + "/numsymbols") )
    {
        cout << "error number of grouped symbols not found" << endl;
        return;
    }
    
    int nsymbols = Symbol::getOSCValueAsInt( s->getOSCMessageValue(nsym_oscpos) );
    
    
    // iterate subsymbols and look for paths or subgroups
    for( int subsym_idx = 0; subsym_idx < nsymbols; subsym_idx++)
    {
        String subsym_id = String(subsym_idx+1);
        String subsym_addr = groupsymbol_addr + "/subsymbol/" + subsym_id;
        
        // cout << "    subsym_addr " << subsym_addr << endl;

        int subsymtype_pos = s->getOSCMessagePos( subsym_addr  + "/type" );
        if( subsymtype_pos != -1 )
        {
            
            String subsymbol_name;
            int subname_pos = s->getOSCMessagePos(subsym_addr + "/name");
            if( subname_pos != -1 )
                subsymbol_name = s->getOSCMessageValue(subname_pos).getString();
            
            
            StringArray subsym_addr_tok;
            subsym_addr_tok.addTokens(subsym_addr, "/", "");
            subsym_addr_tok.removeEmptyStrings();
            
            int subsym_last_element_idx = subsym_addr_tok.size() - 1;
            /*
            cout << "* " << subsym_id << " output prefix: " <<  output_prefix << endl;;
            cout << "\t group_name " <<  group_name<< endl;
            cout << "\t subsym_addr: " << subsym_addr<< endl;
            cout << "\t >> next prefix: " << output_prefix + group_name + "/subsymbol/" + subsym_id << endl;
             */
            
            if( s->getOSCMessageValue(subsymtype_pos).getString() == "group" )
            {
                groupLookup(s, output_prefix + group_name + "/subsymbol/" + subsym_id , subsym_addr, this_x, this_y, time_ratio, bndl );
            }
            else
            {
                
                // do name insertion here for all subsymbols at this level
                
                // iterate bundle and match addresses for this subsymbol
                // add to output bundle with /name inserted
                
                for ( auto osc : s_bndl )
                {
                    OSCMessage msg = osc.getMessage();
                    String msg_addr = msg.getAddressPattern().toString();
                    
                    if( !msg_addr.startsWith(subsym_addr) )
                    {
                        continue;
                    }
                    
                    StringArray msg_tok;
                    msg_tok.addTokens (msg_addr, "/", "");
                    msg_tok.removeEmptyStrings();
                    
                    //cout << "\t test msg_addr " << msg_addr << endl;

                    msg_tok.insert( subsym_last_element_idx+1, subsymbol_name );
                    String newaddr = output_prefix + group_name + "/" + msg_tok.joinIntoString("/", subsym_last_element_idx - 1);
                    
                    //cout << "\t + subsymbol msg_addr " << msg_addr<< endl;
                    //cout << "\t + newaddr " << newaddr << endl;

                    msg.setAddressPattern( newaddr );
                    bndl.addElement( msg );
                    
                }

                // add path lookup if it's a path
                
                if( s->getOSCMessageValue(subsymtype_pos).getString() == "path" )
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

                                // use group name first, otherwise, if there is subsymbol_name name, use that.
                                if( group_name.isNotEmpty() || subsymbol_name.isNotEmpty() )
                                    bndl.addElement( OSCMessage( output_prefix + group_name +"/subsymbol/"+subsym_id+  "/"+subsymbol_name + "/lookup/xy", xy.x / w, xy.y / h) );
                                else
                                    bndl.addElement( OSCMessage( output_prefix + +"/subsymbol/"+subsym_id+ "/path/" + (String)p_idx + "/lookup/xy", xy.x / w, xy.y / h) );
                                    

                            }
                        }
                    }
                }
            }
        }
    }
}


odot_bundle *TimePointArray::timePointStreamToOSC(const SymbolTimePoint *tpoint  )
{
    OSCBundle bndl;
    bndl.addElement( OSCMessage("/time/lookup", (float)current_time) );
    bndl.addElement( OSCMessage("/time/end", (float)getLast()->time) );
    
    String prefix = "/symbolsAtTime/";

    if( tpoint != nullptr )
    {
        const vector<Symbol*> vec = tpoint->symbols_at_time;
        
        int count = 0;
        for (auto s : vec )
        {

            const Symbol *staff = tpoint->staff_ref;
            String staff_name = staff->getName();

            String toplevel_name;
            int name_pos = s->getOSCMessagePos( "/name" );
            if( name_pos != -1 )
            {
                toplevel_name = s->getOSCMessageValue(name_pos).getString();
            }
            
            if( current_time > s->getEndTime() )
            {
                pair<size_t, int> voice_num_state = setNoteOff( s );
                if( voice_num_state.first != -1 ) // voice number is set to -1 if not found
                {
                    String s_prefix = "/staff/" + staff->getName() + "/voice/" + String(voice_num_state.first) + "/" + toplevel_name;
                    bndl.addElement( OSCMessage( s_prefix + "/state", -1 ) );
                }
            }
            else
            {
                pair<size_t, int> voice_num_state = getVoiceNumberState( s, tpoint );
                String s_prefix = "/staff/" + staff->getName() + "/voice/" + String(voice_num_state.first) + "/" + toplevel_name;
                
                bndl.addElement( OSCMessage( s_prefix + "/state", (int)voice_num_state.second ) );
                // staff is already stored in timepoint so we could probably removed the staff check here...
                
                //String staff_name = "default";
                //String staff_id;
                float staff_x = 0, staff_y = 0;
                staff_x = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/x") );
                staff_y = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/y") );
                /*
                int staff_pos = s->getOSCMessagePos( "/staff" );
                if( staff_pos != -1 )
                {
                    staff_id = s->getOSCMessageValue( staff_pos ).getString();
                    if( staff_id.isNotEmpty() )
                    {
                        // cout << "staff name " << staff_name << endl;
                        const Symbol *staff = score_ptr->getStaveByID( staff_id );
                        
                        if( staff != NULL )
                        {
                            staff_x = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/x") );
                            staff_y = Symbol::getOSCValueAsFloat( staff->getOSCMessageValue("/y") );
                            staff_name = staff->getName();
                        }
                    }
                }
                */
                
                float time_ratio = (current_time - s->getTime()) / s->getDuration() ;
                bndl.addElement( OSCMessage( s_prefix + "/time/ratio", time_ratio ) );
                
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
                    String group_prefix = "/staff/" + staff->getName() + "/voice/" + String(voice_num_state.first);
                    groupLookup(s, group_prefix, String(), staff_x, staff_y, time_ratio, bndl );
                }
                else
                {
                
                    OSCBundle s_bndl = *(s->getOSCBundle());

                    for ( auto osc : s_bndl )
                    {
                        OSCMessage msg = osc.getMessage();
                        
                        String msg_addr =  msg.getAddressPattern().toString();
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
        String s_prefix = "/staff/" + get<2>(offs[i])->getName() + "/voice/" + String(get<0>(offs[i])) +"/"+ get<1>(offs[i])->getName();
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
