#include "OdotBundleRef.hpp"
#include "OdotBundle.hpp"

#include "osc_bundle_iterator_u.h"
#include "osc_bundle_iterator_s.h"
#include "osc_strfmt.h"
#include "osc_match.h"
#include "osc_mem.h"
#include "osc_parser.h"

OdotBundleRef::OdotBundleRef( t_osc_bndl_u * src )
{
    m_bndl = src;
}

OdotBundleRef::OdotBundleRef( const OdotBundleRef& src )
{
    m_bndl = src.get_o_ptr();
}

OdotBundleRef::OdotBundleRef( const OdotBundleRef* src )
{
    m_bndl = src->get_o_ptr();
}

OdotBundleRef::OdotBundleRef( const OdotBundle& src )
{
    m_bndl = src.get_o_ptr();
}

OdotBundleRef::OdotBundleRef( const OdotBundle* src )
{
    m_bndl = src->get_o_ptr();
}

OdotBundleRef& OdotBundleRef::operator=( const OdotBundleRef& src )
{
    m_bndl = src.get_o_ptr();
    return *this;
}

OdotBundleRef& OdotBundleRef::operator=( OdotBundleRef& src )
{
    m_bndl = src.get_o_ptr();
    return *this;
}

OdotBundleRef& OdotBundleRef::operator=( const OdotBundle& src )
{
    m_bndl = src.get_o_ptr();
    return *this;
}

void OdotBundleRef::addMessage( vector<OdotMessage> msg_vec )
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    for( int i = 0; i < msg_vec.size(); i++ )
        addMessage( msg_vec[i] );
}

/* get first OSC Messages matching this address (full match) */
OdotMessage OdotBundleRef::getMessage( const char * address ) const
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    t_osc_bndl_it_u *it = osc_bndl_it_u_get( get_o_ptr() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *current_message = osc_bndl_it_u_next(it);
        int po, ao;
        int r = osc_match( address, osc_message_u_getAddress( current_message ), &po, &ao );
        
        if(r == (OSC_MATCH_ADDRESS_COMPLETE | OSC_MATCH_PATTERN_COMPLETE))
        {
            osc_bndl_it_u_destroy(it);
            return OdotMessage( current_message );
        }
    }
    osc_bndl_it_u_destroy(it);
    return OdotMessage();
}


void OdotBundleRef::unionWith( const OdotBundleRef& other, bool passive )
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    t_osc_bndl_u *internal = get_o_ptr();
    
    if( !passive )
        osc_bundle_u_union( internal, other.get_o_ptr(), &internal );
    else
        osc_bundle_u_union( other.get_o_ptr(), internal, &internal );
    
}

void OdotBundleRef::applyExpr( const OdotExpr& expr )
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");
    
    int error = 0;
    t_osc_expr *f = expr.get_o_ptr();
    while(f){
        t_osc_atom_ar_u *av = NULL;
        error = osc_expr_u_eval( f, get_o_ptr(), &av, this);
        if(av){
            osc_atom_array_u_free(av);
        }
        if(error)
        {
            break;
        }
        f = osc_expr_next(f);
    }
}


void OdotBundleRef::addMessage( const OdotMessage& msg )
{
    if( !get_o_ptr() ) throw std::runtime_error("error: adding messages to null bundle!");

    // makes a copy which will be owned by the bundle,
    // this way the incoming msg is still available to be used by the caller if needed
    OdotMessage msg_cpy( msg );
    osc_bundle_u_replaceMessage( get_o_ptr(), msg_cpy.release() );
}

void OdotBundleRef::addMessage( t_osc_msg_u * msg )
{
    if( !get_o_ptr() ) throw std::runtime_error("error: adding messages to null bundle!");

    osc_bundle_u_replaceMessage( get_o_ptr(), msg );
}

void OdotBundleRef::setFromString( const string& str )
{
    t_osc_bndl_u * bndl = nullptr;
    osc_parser_parseString( (long)str.size(), (char *)str.c_str(), &bndl );
    set_o_ptr( bndl );
}

OdotBundleRef::OdotBundleRef( const string& str )
{
    setFromString( str );
}

void OdotBundleRef::clear()
{
    osc_bundle_u_clear( get_o_ptr() );
}


bool OdotBundleRef::addressExists( const string& address ) const
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    return addressExists(address.c_str());
}

bool OdotBundleRef::addressExists( const char * address ) const
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    int res;
    osc_bundle_u_addressExists( get_o_ptr(), (char *)address, 1, &res );
    return (res == 1);
}


vector<OdotMessage> OdotBundleRef::matchAddress( const char * address, int fullmatch ) const
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    vector<OdotMessage> ar;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( get_o_ptr() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *current_message = osc_bndl_it_u_next(it);
        int po, ao;
        int r = osc_match( address, osc_message_u_getAddress( current_message ), &po, &ao );
        if( fullmatch )
        {
            if(r != (OSC_MATCH_ADDRESS_COMPLETE | OSC_MATCH_PATTERN_COMPLETE))
            {
                continue;
            }
        }
        else
        {
            if(r == 0 || (((r & OSC_MATCH_PATTERN_COMPLETE) == 0) && address[po] != '/'))
            {
                continue;
            }
        }
        ar.emplace_back( OdotMessage( current_message ) );
    }
    osc_bndl_it_u_destroy(it);
    return ar;
    
}


vector<OdotMessage> OdotBundleRef::getMessageArray() const
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    vector<OdotMessage > ar;
    ar.reserve( size() );
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( get_o_ptr() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        ar.emplace_back( OdotMessage( osc_bndl_it_u_next(it) ) );
    }
    osc_bndl_it_u_destroy(it);
    return ar;
}

string OdotBundleRef::getJSON()
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    string JSON = "{";
    bool add_comma = false;
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( get_o_ptr() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        if( add_comma )
            JSON += ",";
        
        JSON += OdotMessage( osc_bndl_it_u_next(it) ).getJSON();
        
        add_comma = true;
    }
    osc_bndl_it_u_destroy(it);
    
    JSON += "}";
    return JSON;
}


void OdotBundleRef::print( int level ) const
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += ".\t";
    
    cout << indent << "==== ODOT BUNDLE ====" << endl;
    cout << indent << "   ( " << get_o_ptr() << " )" << endl;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( get_o_ptr() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        //cout << "||| " << msg << " :" << endl;

        cout << indent << osc_message_u_getAddress(msg);
        
        char buf[256];
        char *buf_ptr = buf;
        int argcount = osc_message_u_getArgCount(msg);
        for( int i = 0; i < argcount; i++ )
        {
            t_osc_atom_u *a = osc_message_u_getArg(msg, i);
            if( osc_atom_u_getTypetag(a) == OSC_BUNDLE_TYPETAG )
            {
                cout << indent << "\t{ \n";
                OdotBundleRef( osc_atom_u_getBndl(a) ).print( level+1 );
                cout << indent << " } ";
            }
            else
            {
                osc_atom_u_getString( a, 256, &buf_ptr );
                cout << "\t" << buf_ptr;
            }
        }
        cout << endl;
    }
    osc_bndl_it_u_destroy(it);
    
    cout << indent << "====-===-======-====" << endl;
    
}

void OdotBundleRef::getPrintString( string &str, int level )
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += "\t";
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( get_o_ptr() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        str += indent + osc_message_u_getAddress(msg);
        
        char buf[256];
        char *buf_ptr = buf;
        int argcount = osc_message_u_getArgCount(msg);
        for( int i = 0; i < argcount; i++ )
        {
            t_osc_atom_u *a = osc_message_u_getArg(msg, i);
            if( osc_atom_u_getTypetag(a) == OSC_BUNDLE_TYPETAG )
            {
                str += indent + "\t{ \n";
                OdotBundleRef( osc_atom_u_getBndl(a) ).getPrintString( str, level+1 );
                str += indent + " } ";
            }
            else
            {
                str += "\t";
                osc_atom_u_getString( a, 256, &buf_ptr );
                str += buf_ptr;
            }
        }
        str += "\n";
    }
    osc_bndl_it_u_destroy(it);
}

void OdotBundleRef::getPrintStringArray( vector<string> &str, int level )
{
    if( !get_o_ptr() ) throw std::runtime_error("error: null bundle");

    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += "\t";
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( get_o_ptr() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        
        string line_str = indent + osc_message_u_getAddress(msg);
        
        char buf[256];
        char *buf_ptr = buf;
        int argcount = osc_message_u_getArgCount(msg);
        
        for( int i = 0; i < argcount; i++ )
        {
            t_osc_atom_u *a = osc_message_u_getArg(msg, i);
            if( osc_atom_u_getTypetag(a) == OSC_BUNDLE_TYPETAG )
            {
                str.emplace_back( indent + "\t{" );
                OdotBundleRef( osc_atom_u_getBndl(a) ).getPrintStringArray( str, level+1 );
                str.emplace_back( indent + " } " );
            }
            else
            {
                line_str += "\t" ;
                osc_atom_u_getString( a, 256, &buf_ptr );
                line_str += buf_ptr;
            }
        }
        
        str.emplace_back( line_str );
        
    }
    osc_bndl_it_u_destroy(it);
}

