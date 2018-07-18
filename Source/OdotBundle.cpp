#include "OdotBundle.hpp"
#include "osc_parser.h"
#include "osc_bundle_iterator_u.h"
#include "osc_bundle_iterator_s.h"
#include "osc_strfmt.h"
#include "osc_match.h"
#include "osc_mem.h"

#include <fstream>
#include <sstream>

OdotBundle::OdotBundle()
{
    ptr = odot::newOdotBundlePtr();
//    D_(std::cout << "new bundle " << &ptr << " " << ptr.get() << std::endl;)
}

OdotBundle::OdotBundle( const OdotBundle& src )
{
//    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src.ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const OdotBundle* src )
{
    //    D_(cout << __func__ << "copy \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, src->ptr.get() );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const t_osc_bndl_u * src )
{
//    D_(cout << __func__  << "copy from odot pointer \n";)
    t_osc_bndl_u *b = osc_bundle_u_alloc();
    osc_bundle_u_copy( &b, (t_osc_bndl_u *)src );
    ptr = odot::newOdotBundlePtr( b );
}

OdotBundle::OdotBundle( const OdotBundle_s& src )
{
    OdotBundle b( src.get_o_ptr() );
    ptr = odot::newOdotBundlePtr( b.release() );
}

OdotBundle::OdotBundle( const t_osc_bndl_s * src )
{
    ptr = odot::newOdotBundlePtr(osc_bundle_s_deserialize(osc_bundle_s_getLen((t_osc_bndl_s *)src),
                                                           osc_bundle_s_getPtr((t_osc_bndl_s *)src)));
}

OdotBundle::OdotBundle( const OdotMessage& msg )
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
        
        char buf[256];
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
                osc_atom_u_getString( a, 256, &buf_ptr );
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

bool OdotBundle::operator==( const OdotBundle& src )
{
    if( get_o_ptr() == src.get_o_ptr() )
        return true;
    
    if( size() != src.size() )
        return false;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        if( !src.addressExists( osc_message_u_getAddress(msg) ) )
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

/* Converts the string into char* to pass it
 * to the osc_parser_parseString function.
 */
/*
OdotBundle* OdotBundle::createOdotBundleFromString(string textToParse)
{

	t_osc_bndl_u* bundle = NULL;
	char textToParseChar [textToParse.length() + 1];
	strcpy(textToParseChar, textToParse.c_str());
	
	// Creates the OSC bundle from the parsed text
	t_osc_err error = osc_parser_parseString(strlen(textToParseChar), textToParseChar, &bundle);
	if (error == OSC_ERR_PARSER)
		throw invalid_argument("The string being parsed is not a well-formed odot bundle.");
	
	return new OdotBundle(bundle);
}
*/


/* Creates a file input stream to read the content
 * of the osc file which path is in parameter.
 */
/*
OdotBundle* OdotBundle::createOdotBundleFromFile(string oscFilePath)
{
	
	ifstream oscFile(oscFilePath);
	
	if(!oscFile)
		throw invalid_argument("Invalid path to osc file.");
	
	// Stores the file content into a string buffer
	stringstream fileContentBuffer;
	fileContentBuffer << oscFile.rdbuf();
	
	return createOdotBundleFromString(fileContentBuffer.str());
}
*/

/*
// unfinished set of functions that was going to merge the bundle keeping the msg pointers in the same mem locations
 
t_osc_msg_u * OdotBundle::lookup_osc_msg_u( const char * address )
{
    const long addr_len = strlen( address );
    t_osc_bndl_it_u *iter_bndl = osc_bndl_it_u_get( ptr.get() );
    while( osc_bndl_it_u_hasNext(iter_bndl) )
    {
        t_osc_msg_u *m = osc_bndl_it_u_next(iter_bndl);
        char *a = osc_message_u_getAddress(m);
        if( strlen(a) != addr_len ) {
            continue;
        }
        else if( !strncmp( a, address, addr_len ) )
        {
            osc_bndl_it_u_destroy(iter_bndl);
            return m;
        }
    }
    osc_bndl_it_u_destroy(iter_bndl);
    return nullptr;
}

void OdotBundle::deserializeMerge( const t_osc_bundle_s *src )
{
    long len = osc_bundle_s_getLen((t_osc_bndl_s *)src);
    char *s_ptr = osc_bundle_s_getPtr((t_osc_bndl_s *)src);
    std::cout << osc_bundle_s_format(len, s_ptr) << "+" << std::endl;
    
    t_osc_bndl_it_s *it = osc_bndl_it_s_get(len, s_ptr);
    while(osc_bndl_it_s_hasNext(it))
    {
        t_osc_msg_s *s_m = osc_bndl_it_s_next(it);
        
        // more efficient would be to iterate the pre and post s bundles and check to see if the value changed, and don't deserialize if nothing changed
        
        t_osc_msg_u *u_m = osc_message_s_deserialize(s_m);
        const char * address = osc_message_u_getAddress(u_m);
        t_osc_msg_u * dst_m = lookup_osc_msg_u( address );
        std::cout << "&& " << address << " " << dst_m << std::endl;
        if( dst_m )
        {
            osc_message_u_clearArgs( dst_m );
            osc_message_u_setArgArrayCopy( dst_m , osc_message_u_getArgArrayCopy( u_m ) );
            osc_message_u_free( u_m );
        }
        else
            osc_bundle_u_addMsg( ptr.get(), u_m );
        
    }
    osc_bndl_it_s_destroy(it);
}

void OdotBundle::addMessage( const OdotMessage& msg )
{
    OdotMessage msg_cpy( msg );
    t_osc_msg_u * existing_msg = lookup_osc_msg_u( msg_cpy.getAddress().c_str() );
    if( existing_msg ) // update existing message if exists without reallocating the pointer
    {
        auto atoms = msg_cpy.getAtoms();
        
        if( atoms.size() == 1 && atoms[0].getType() == OdotAtom::O_ATOM_BUNDLE )
        {
            // if it's a subbundle, don't move those either?
            // but then what about sub-sub bundles?
            // I guess for now we can just leave the
            
            auto iter_u_b = osc_bndl_it_u_get(atoms[0].getBundlePtr());
            
            t_osc_bndl_s * serial_copy = osc_bundle_u_serialize( atoms[0].getBundlePtr() );

//             t_osc_bndl_s *bs = osc_bundle_u_serialize(src);
//             *dest = osc_bundle_s_deserialize(osc_bundle_s_getLen(bs), osc_bundle_s_getPtr(bs));
//             osc_bundle_s_deepFree(bs);

        }
        else
        {
            osc_message_u_clearArgs( existing_msg );
            for( auto a : atoms )
            {
                osc_message_u_appendAtom( existing_msg, a.release() );
            }
            // osc_message_u_setArgArrayCopy( existing_msg , osc_message_u_getArgArrayCopy( msg_cpy.get_o_ptr() ) );
        }
    }
    else
        osc_bundle_u_addMsg( ptr.get(), msg_cpy.release() );
}
 

*/
