#include "OdotBundle.hpp"
#include "osc_parser.h"
#include "osc_bundle_iterator_u.h"
#include "osc_bundle_iterator_s.h"
#include "osc_strfmt.h"
#include "osc_match.h"
#include "osc_mem.h"

#include <fstream>
#include <sstream>

extern "C" {
    int odot_expr_error_handler(void *context, const char * const errorstr)
    {
        cout << errorstr << endl;
        return 0;
    }
}

OdotBundle::OdotBundle()
{
    ptr = odot::newOdotBundlePtr();
    osc_error_setHandler( odot_expr_error_handler );

//    D_(std::cout << "new bundle " << &ptr << " " << ptr.get() << std::endl;)
}

OdotBundle::OdotBundle( const OdotBundle& src )
{
//    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src.ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
    osc_error_setHandler( odot_expr_error_handler );
}

OdotBundle::OdotBundle( const OdotBundle* src )
{
    //    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src->ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
    osc_error_setHandler( odot_expr_error_handler );
}

OdotBundle::OdotBundle( const t_osc_bndl_u * src )
{
//    D_(cout << __func__  << "copy from odot pointer \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, (t_osc_bndl_u *)src );
    ptr = odot::newOdotBundlePtr( b );
    osc_error_setHandler( odot_expr_error_handler );
}

OdotBundle::OdotBundle( const OdotBundle_s& src )
{
    OdotBundle b( src.get_o_ptr() );
    ptr = odot::newOdotBundlePtr( b.release() );
    osc_error_setHandler( odot_expr_error_handler );
}

OdotBundle::OdotBundle( const t_osc_bndl_s * src )
{
    ptr = odot::newOdotBundlePtr(osc_bundle_s_deserialize(osc_bundle_s_getLen((t_osc_bndl_s *)src),
                                                           osc_bundle_s_getPtr((t_osc_bndl_s *)src)));
    osc_error_setHandler( odot_expr_error_handler );
}

OdotBundle::OdotBundle( const OdotMessage& msg )
{
    ptr = odot::newOdotBundlePtr();
    addMessage( msg );
    osc_error_setHandler( odot_expr_error_handler );
}

OdotBundle& OdotBundle::operator=( const OdotBundle& src )
{
//    D_(cout << __func__  << "copy= \n";)
    
    if( this != &src )
    {
        t_osc_bndl_u *b = osc_bundle_u_alloc();
        osc_bundle_u_copy( &b, (t_osc_bndl_u *)src.ptr.get() );
        ptr = odot::newOdotBundlePtr( b );
        osc_error_setHandler( odot_expr_error_handler );
    }
    
    return *this;
}

OdotBundle::OdotBundle( vector<OdotMessage> msg_vec )
{
    OdotBundle();
    addMessage( msg_vec );
}

OdotBundle::OdotBundle( const string& str )
{
    setFromString( str );
}

void OdotBundle::unionWith( const OdotBundle& other, bool passive )
{
    t_osc_bndl_u *unioned = osc_bundle_u_alloc();
    
    if( !passive )
        osc_bundle_u_union( ptr.get(), other.ptr.get(), &unioned );
    else
        osc_bundle_u_union( other.ptr.get(), ptr.get(), &unioned );

    ptr = odot::newOdotBundlePtr( unioned );
}

int OdotBundle::applyExpr( const OdotExpr& expr )
{
    int error = 0;
    t_osc_expr *f = expr.get_o_ptr();
    t_osc_bndl_u * bndl = ptr.get();
    while( f && bndl ){
        t_osc_atom_ar_u *av = NULL;
        error = osc_expr_u_eval( f, bndl, &av, this);
        if(av){
            osc_atom_array_u_free(av);
        }
        
        if(error)
        {
            return 1;
        }
        
        f = osc_expr_next(f);
    }
    
    return 0;
}

void OdotBundle::addMessage( vector<OdotMessage> msg_vec )
{
    for( int i = 0; i < msg_vec.size(); i++ )
        addMessage( msg_vec[i] );
}

void OdotBundle::addMessage( const OdotMessage& msg )
{
    // makes a copy which will be owned by the bundle,
    // this way the incoming msg is still available to be used by the caller if needed
    OdotMessage msg_cpy( msg );
    osc_bundle_u_replaceMessage( ptr.get(), msg_cpy.release() );
}

void OdotBundle::addMessage( t_osc_msg_u * msg )
{
    //  cout << "replace " << endl;
    osc_bundle_u_replaceMessage( ptr.get(), msg );
}

void OdotBundle::removeMessage( t_osc_msg_u * msg )
{
    osc_bundle_u_removeMsg( ptr.get(), msg );
    osc_message_u_free( msg );
    msg = NULL;
}

void OdotBundle::removeMessage( const string & addr )
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        const char * msg_addr = osc_message_u_getAddress(msg);
        if( addr == msg_addr )
        {
            osc_bundle_u_removeMsg( ptr.get(), msg );
            osc_message_u_free( msg );
            msg = NULL;
        }
        
    }
    osc_bndl_it_u_destroy(it);
}


void OdotBundle::clear()
{
    osc_bundle_u_clear( ptr.get() );
}

void OdotBundle::print( int level ) const
{
    print_imp( ptr.get(), level );
}

void OdotBundle::print_imp( t_osc_bndl_u * bndl, int level ) const
{
    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += ".\t";
    
    cout << indent << "==== ODOT BUNDLE ====" << endl;
    cout << indent << "   ( " << bndl << " )" << endl;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        
        cout << indent << osc_message_u_getAddress(msg);
        
        char buf[32768];
        char *buf_ptr = buf;
        int argcount = osc_message_u_getArgCount(msg);
        for( int i = 0; i < argcount; i++ )
        {
            t_osc_atom_u *a = osc_message_u_getArg(msg, i);
            if( osc_atom_u_getTypetag(a) == OSC_BUNDLE_TYPETAG )
            {
                cout << indent << "\t{ \n";
                print_imp( osc_atom_u_getBndl(a), level+1 );
                cout << indent << " } ";
            }
            else
            {
                osc_atom_u_getString( a, 32768, &buf_ptr );
                cout << "\t" << buf_ptr;
            }
        }
        cout << endl;
    }
    osc_bndl_it_u_destroy(it);
    
    cout << indent << "====-===-======-====" << endl;
}


void OdotBundle::getPrintString( string &str, int level )
{
    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += "\t";
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
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
                OdotBundle( osc_atom_u_getBndl(a) ).getPrintString( str, level+1 );
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

void OdotBundle::getPrintStringArray( vector<string> &str, int level )
{
    string indent = "";
    for( int i = 0; i < level; i++ )
        indent += "\t";
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
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
                OdotBundle( osc_atom_u_getBndl(a) ).getPrintStringArray( str, level+1 );
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



string OdotBundle::getJSON()
{
    string JSON = "{";
    bool add_comma = false;
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
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

bool OdotBundle::operator==( const OdotBundle& src ) const
{
    if( get_o_ptr() == src.get_o_ptr() )
        return true;
    
    if( size() != src.size() )
        return false;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        const char * addr = osc_message_u_getAddress(msg);
        if( src.getMessage(addr) != OdotMessage(msg) )
        {
            osc_bndl_it_u_destroy(it);
            return false;
        }
        
    }
    osc_bndl_it_u_destroy(it);
    return true;
}


bool OdotBundle::addressExists( const string& address ) const
{
    return addressExists(address.c_str());
}

bool OdotBundle::addressExists( const char * address ) const
{
    int res;
    osc_bundle_u_addressExists( ptr.get(), (char *)address, 1, &res );
    return (res == 1);
}

/* get first OSC Messages matching this address (full match) */
OdotMessage OdotBundle::getMessage( const string& address ) const
{
    auto addr_vec = split(address, ".");
    return getMessage_recursive(ptr.get(), addr_vec, 0);
}

void OdotBundle::assignToBundleMember_createEmpties( t_osc_bndl_u *bndl, const vector<string>& addr_vec, int level, t_osc_msg_u * msg )
{
    //    printf("creating empties argc %d\n", argc );
    t_osc_bundle_u *parent = bndl;
    for( long i = level; i < addr_vec.size()-1; i++ )
    {
        t_osc_msg_u * m = osc_message_u_allocWithAddress( (char *)addr_vec[i].c_str() );
        t_osc_bundle_u * child = osc_bundle_u_alloc();
        osc_message_u_appendBndl_u( m, child );
        osc_bundle_u_addMsg(parent, m); // new messsage, so no need to check for replace routine
        
        parent = child;
    }
    
    osc_bundle_u_addMsg(parent, msg );
    
}

void OdotBundle::assignToBundleMember_recusive( t_osc_bndl_u *bndl, const vector<string>& addr_vec, int level, t_osc_msg_u * msg )
{
    t_osc_msg_u *m = osc_bundle_u_getFirstFullMatch( bndl, (char *)addr_vec[level].c_str() );
    if( m )
    {
       // printf("found addr %s -- argc %d\n", osc_message_u_getAddress(m), level );
        
        // figure out if we're done looking or not and set the target bundle message once we're done
        if( level == addr_vec.size()-1 )
        {
            osc_message_u_clearArgs(m);
            osc_message_u_deepCopy(&m, msg );
            osc_message_u_free(msg);
            return;
        }
        // if we're not at the target yet, keep checking for sub-bundles
        
        // if the current message doesn't have a subbundle, we need to create the rest of them
        t_osc_atom_u * at = osc_message_u_getArg(m, 0);
        if( osc_atom_u_getTypetag( at ) != OSC_BUNDLE_TYPETAG )
        {
            assignToBundleMember_createEmpties( bndl, addr_vec, level+1, msg );
            return;
        }
        
        t_osc_bundle_u * sub = osc_atom_u_getBndl(at);
        assignToBundleMember_recusive( sub, addr_vec, level+1, msg );
    }
    else
    {
        assignToBundleMember_createEmpties(bndl, addr_vec, level, msg);
    }
    
}

OdotMessage OdotBundle::getMessage_recursive( t_osc_bndl_u * bndl, const vector<string>& addr_vec, int level ) const
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *current_message = osc_bndl_it_u_next(it);
        if(addr_vec[level] == osc_message_u_getAddress( current_message ) )
        {
            osc_bndl_it_u_destroy(it);

            if( level == (addr_vec.size()-1) )
            {
                return OdotMessage( current_message );
            }
            else
            {
                auto first_atom = osc_message_u_getArg(current_message, 0);
                if( osc_atom_u_getTypetag( first_atom ) == OSC_BUNDLE_TYPETAG )
                {
                    return getMessage_recursive( osc_atom_u_getBndl(first_atom), addr_vec, level+1 );
                }
                else
                {
                    return OdotMessage();
                }
            }
        }
    }
    osc_bndl_it_u_destroy(it);
    return OdotMessage();
}


vector<OdotMessage> OdotBundle::matchAddress( const char * address, int fullmatch ) const
{
    vector<OdotMessage> ar;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
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


vector<OdotMessage> OdotBundle::getMessageArray() const
{
    vector<OdotMessage > ar;
    ar.reserve( size() );
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        ar.emplace_back( OdotMessage( osc_bndl_it_u_next(it) ) );
    }
    osc_bndl_it_u_destroy(it);
    return ar;
}

void OdotBundle::setFromString( const string& str )
{
    t_osc_bndl_u * bndl = nullptr;
    if( str.size() == 0 )
        throw invalid_argument("The string being parsed is empty.");

    // strip white space at ends
    string parse_str = str.substr(str.find_first_not_of(" \t\n\r\f\v"), str.length()-1 );
    parse_str = parse_str.substr(0, parse_str.find_last_not_of(" \t\n\r\f\v") + 1);

    // remove enclosing brackets if present
    if( parse_str.front() == '{' ) {
        if( parse_str.back() == '}' )
            parse_str = parse_str.substr(1, parse_str.length() - 2 );
        else
            throw invalid_argument("The string being parsed is not a well-formed odot bundle.");
    }
    
    t_osc_err error = osc_parser_parseString( (long)parse_str.size(), (char *)parse_str.c_str(), &bndl );
    if (error == OSC_ERR_PARSER)
        throw invalid_argument("The string being parsed is not a well-formed odot bundle.");
 
    ptr = odot::newOdotBundlePtr( bndl );
    
}

void OdotBundle::setFromFile( const string& oscFilePath ) 
{
    /* Creates a file input stream to read the content
     * of the osc file which path is in parameter.
     */
    ifstream oscFile(oscFilePath);
    
    if(!oscFile)
        throw invalid_argument("Invalid path to osc file.");
    
    /* Stores the file content into a string buffer */
    stringstream fileContentBuffer;
    fileContentBuffer << oscFile.rdbuf();
    
 //   cout << fileContentBuffer.str() << endl;
    
    setFromString(fileContentBuffer.str());
    
}



/*
 *  recursively search all subbundles for address and return containing subbundle
 */
OdotBundle OdotBundle::getBundleContainingMessage( const char * address ) const
{
    return getBundleContainingMessage_imp( ptr.get(), address );

}

OdotBundle OdotBundle::getBundleContainingMessage_imp( t_osc_bndl_u * bndl, const char * address ) const
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( bndl );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *current_message = osc_bndl_it_u_next(it);
        int po, ao;
        int r = osc_match( address, osc_message_u_getAddress( current_message ), &po, &ao );
        if(r == (OSC_MATCH_ADDRESS_COMPLETE | OSC_MATCH_PATTERN_COMPLETE))
        {
            osc_bndl_it_u_destroy(it);
            return OdotBundle( bndl );
        }
        else
        {
            for( int i = 0; i < osc_message_u_getArgCount(current_message); i++ )
            {
                t_osc_atom_u * at = osc_message_u_getArg(current_message, i);
                if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
                {
                    OdotBundle sub = getBundleContainingMessage_imp( osc_atom_u_getBndl(at), address );
                    if( !sub.unbound() )
                        return sub;
                }
            }
        }
    }
    osc_bndl_it_u_destroy(it);
    return OdotBundle();
}

vector<string> OdotBundle::split(string data, string token) const
{
    vector<string> output;
    size_t pos = string::npos; // size_t to avoid improbable overflow
    do {
        pos = data.find(token);
        output.push_back(data.substr(0, pos));
        if (string::npos != pos)
            data = data.substr(pos + token.size());
            } while (string::npos != pos);
    return output;
}

/**
 *  recursively search all subbundles for address with a given value and return containing subbundle
 *  only one value for now, need to implement atom vector check
 */
/*
OdotBundle OdotBundle::getBundleContainingMessage( OdotMessage& msg ) const
{
    return getBundleContainingMessage_imp(ptr.get(), msg );
}

OdotBundle OdotBundle::getBundleContainingMessage_imp( t_osc_bndl_u * bndl, OdotMessage& msg ) const
{
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( bndl );
    const char * address = msg.getAddress().c_str();
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *current_message = osc_bndl_it_u_next(it);
        int po, ao;
        int r = osc_match( address, osc_message_u_getAddress( current_message ), &po, &ao );
        if(r == (OSC_MATCH_ADDRESS_COMPLETE | OSC_MATCH_PATTERN_COMPLETE))
        {
            
            if( msg == OdotMessage(msg) )
            {
                
            }
            
            osc_bndl_it_u_destroy(it);
            return OdotBundle(bndl);
        }
        else
        {
            for( int i = 0; i < osc_message_u_getArgCount(current_message); i++ )
            {
                t_osc_atom_u * at = osc_message_u_getArg(current_message, i);
                if( osc_atom_u_getTypetag(at) == OSC_BUNDLE_TYPETAG )
                {
                    OdotBundleRef sub = OdotBundleRef( osc_atom_u_getBndl(at) ).getBundleContainingMessage( address );
                    if( !sub.unbound() )
                        return sub;
                }
            }
        }
    }
    osc_bndl_it_u_destroy(it);
    return OdotBundleRef();
}

*/


