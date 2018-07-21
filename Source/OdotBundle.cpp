#include "OdotBundle.hpp"
#include "osc_parser.h"
#include "osc_bundle_iterator_u.h"
#include "osc_bundle_iterator_s.h"
#include "osc_strfmt.h"
#include "osc_match.h"
#include "osc_mem.h"

#include <fstream>
#include <sstream>

OdotBundle::OdotBundle() : m_select(*this)
{
    ptr = odot::newOdotBundlePtr();
//    D_(std::cout << "new bundle " << &ptr << " " << ptr.get() << std::endl;)
}

OdotBundle::OdotBundle( const OdotBundle& src ) : m_select(*this)
{
//    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src.ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const OdotBundle* src ) : m_select(*this)
{
    //    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src->ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const t_osc_bndl_u * src ) : m_select(*this)
{
//    D_(cout << __func__  << "copy from odot pointer \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, (t_osc_bndl_u *)src );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const OdotBundle_s& src ) : m_select(*this)
{
    OdotBundle b( src.get_o_ptr() );
    ptr = odot::newOdotBundlePtr( b.release() );
}

OdotBundle::OdotBundle( const t_osc_bndl_s * src ) : m_select(*this)
{
    ptr = odot::newOdotBundlePtr(osc_bundle_s_deserialize(osc_bundle_s_getLen((t_osc_bndl_s *)src),
                                                           osc_bundle_s_getPtr((t_osc_bndl_s *)src)));
}

OdotBundle::OdotBundle( const OdotMessage& msg ) : m_select(*this)
{
    ptr = odot::newOdotBundlePtr();
    addMessage( msg );
}

OdotBundle& OdotBundle::operator=( const OdotBundle& src )
{
//    D_(cout << __func__  << "copy= \n";)
    
    if( this != &src )
    {
        t_osc_bndl_u *b = osc_bundle_u_alloc();
        osc_bundle_u_copy( &b, (t_osc_bndl_u *)src.ptr.get() );
        ptr = odot::newOdotBundlePtr( b );
    }
    
    return *this;
}

OdotBundle::OdotBundle( vector<OdotMessage> msg_vec ) : m_select(*this)
{
    OdotBundle();
    addMessage( msg_vec );
}

OdotBundle::OdotBundle( const string& str ) : m_select(*this)
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

void OdotBundle::applyExpr( const OdotExpr& expr )
{
    OdotBundle_s s_bndl = serialize();
    
    char *copy = NULL;
    long copylen = osc_bundle_s_getLen( s_bndl.get_o_ptr() );
    copy = (char *)osc_mem_alloc( copylen );
    if (copy)
    {
        memcpy(copy, osc_bundle_s_getPtr( s_bndl.get_o_ptr() ), copylen);
        
        int error = 0;
        t_osc_expr *f = expr.get_o_ptr();
        while (f) {
            t_osc_atom_ar_u *av = NULL;
            error = osc_expr_eval( f, &copylen, &copy, &av, this );
			
            if (av)
				osc_atom_array_u_free(av);
		
            if (error)
                break;
			
            f = osc_expr_next(f);
        }
        
        if ( !error )
            ptr = odot::newOdotBundlePtr( osc_bundle_s_deserialize(copylen, copy) );
        
        osc_mem_free(copy);
    }
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
    print_imp( ptr.get() );
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
OdotMessage OdotBundle::getMessage( const char * address ) const
{
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
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
    t_osc_err error = osc_parser_parseString( (long)str.size(), (char *)str.c_str(), &bndl );
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


