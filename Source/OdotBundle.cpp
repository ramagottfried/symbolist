
#include "OdotBundle.hpp"

#include "osc_bundle_iterator_u.h"

OdotBundle::OdotBundle()
{
    cout << "new " << endl;

    bundle = osc_bundle_u_alloc();
    clear();
}

OdotBundle::OdotBundle(const OdotBundle& src)
{
    cout << "clone>? " << endl;

    delete_bundle();
    osc_bundle_u_copy( &bundle, src.bundle );
}

OdotBundle::OdotBundle( t_osc_bndl_u *src ) // << annoying that this isn't a const
{
    cout << "copy internal " << endl;

    delete_bundle();
    osc_bundle_u_copy( &bundle, src );
}

OdotBundle& OdotBundle::operator= ( const OdotBundle& src )
{
    if( this != &src )
    {
        delete_bundle();
        osc_bundle_u_copy( &bundle, src.bundle );
    }
    return *this;
}

OdotBundle::~OdotBundle()
{
    delete_bundle();
}

void OdotBundle::delete_bundle()
{
    if( bundle )
    {
        osc_bundle_u_free(bundle);
        bundle = NULL;
    }
}

void OdotBundle::clear()
{
    osc_bundle_u_clear( bundle );
    
    cout << "cleared bundle " << bundle << " count now " << osc_bundle_u_getMsgCount(bundle) << endl;
}

void OdotBundle::print() const
{
    cout << "==== ODOT BUNDLE ====" << endl;
    
    t_osc_bndl_it_u *it = osc_bndl_it_u_get( bundle );
    while( osc_bndl_it_u_hasNext(it) )
    {
        t_osc_msg_u *msg = osc_bndl_it_u_next(it);
        cout << osc_message_u_getAddress(msg);

        char buf[256];
        char *buf_ptr = buf;
        int n = osc_message_u_getArgCount(msg);
        while( n-- )
        {
            osc_atom_u_getString( osc_message_u_getArg(msg, n), 256, &buf_ptr );
            cout << "\t" << buf_ptr;
        }
        cout << endl;
    }
    osc_bndl_it_u_destroy(it);
    
    cout << "====-===-======-====" << endl;

}

void OdotBundle::addOSCMessage( const String& address, const float value)
{
    const char *addr = address.getCharPointer();
    t_osc_msg_u *msg = osc_message_u_allocWithAddress( (char *)addr );
    osc_message_u_appendFloat( msg, value );
    osc_bundle_u_addMsgWithoutDups( bundle, msg );
}

void OdotBundle::addOSCMessage( const String& address, const int value)
{
    const char *addr = address.getCharPointer();
    t_osc_msg_u *msg = osc_message_u_allocWithAddress( (char *)addr );
    osc_message_u_appendInt32( msg, value );
    osc_bundle_u_addMsgWithoutDups( bundle, msg );
    
}

void OdotBundle::addOSCMessage( const String& address, const String& value)
{
    const char *addr = address.getCharPointer();
    t_osc_msg_u *msg = osc_message_u_allocWithAddress( (char *)addr );
    osc_message_u_appendString( msg, value.getCharPointer() );
    osc_bundle_u_addMsgWithoutDups( bundle, msg );
    
}

void OdotBundle::addSubbundle( const String& address, const OdotBundle& sub)
{
    const char *addr = address.getCharPointer();
    t_osc_msg_u *msg = osc_message_u_allocWithAddress( (char *)addr );
    t_osc_bndl_u *subbundle = osc_bundle_u_alloc();
    osc_bundle_u_copy( &subbundle, sub.bundle );
    osc_message_u_appendBndl_u( msg, subbundle );
    osc_bundle_u_addMsgWithoutDups( bundle, msg );
}

float OdotBundle::oscAddressGetFloat(const String& address)
{
    t_osc_msg_ar_u *msg_ar = osc_bundle_u_lookupAddress( bundle, address.getCharPointer(), 1 );
    if( !msg_ar || osc_message_array_u_getLen(msg_ar) == 0 )
    {
        cout << "error: no float at that address, returning zero" << endl;
        return 0;
    }
    
    t_osc_msg_u *m = osc_message_array_u_get(msg_ar, 0);
    t_osc_atom_u *a = osc_message_u_getArg(m, 0);
    
    return osc_atom_u_getFloat(a);
}

int OdotBundle::oscAddressGetInt(const String& address)
{
    t_osc_msg_ar_u *msg_ar = osc_bundle_u_lookupAddress( bundle, address.getCharPointer(), 1 );
    if( !msg_ar || osc_message_array_u_getLen(msg_ar) == 0 )
    {
        cout << "error: no int at that address, returning zero" << endl;
        return 0;
    }
    
    t_osc_msg_u *m = osc_message_array_u_get(msg_ar, 0);
    t_osc_atom_u *a = osc_message_u_getArg(m, 0);
    
    return osc_atom_u_getInt(a);
}

String OdotBundle::oscAddressGetString(const String& address)
{
    t_osc_msg_ar_u *msg_ar = osc_bundle_u_lookupAddress( bundle, address.getCharPointer(), 1 );
    if( !msg_ar || osc_message_array_u_getLen(msg_ar) == 0 )
    {
        cout << "error: no string at that address, returning empty string" << endl;
        return String();
    }
    
    t_osc_msg_u *m = osc_message_array_u_get(msg_ar, 0);
    t_osc_atom_u *a = osc_message_u_getArg(m, 0);

    int stringlen = osc_atom_u_getStringLen(a);
    int nn = stringlen + 1;
    char buf[nn];
    
    switch( osc_atom_u_getTypetag(a) ){
        case 's': // string
            return String(a->w.s);
        case 'i': // signed 32-bit int
            osc_strfmt_int32(buf, nn, a->w.i);
            break;
        case 'f': // 32-bit IEEE 754 float
            osc_strfmt_float32(buf, nn, a->w.f);
            break;
        case 'd': // 64-bit IEEE 754 double
            osc_strfmt_float64(buf, nn, a->w.d);
            break;
        case 'h': // signed 64-bit int
            osc_strfmt_int64(buf, nn, a->w.h);
            break;
        case 'I': // unsigned 32-bit int
            osc_strfmt_uint32(buf, nn, a->w.I);
            break;
        case 'H': // unsigned 64-bit int
            osc_strfmt_uint64(buf, nn, a->w.H);
            break;
        case 'c': // signed 8-bit char
            osc_strfmt_int8(buf, nn, a->w.c);
            break;
        case 'C': // unsigned 8-bit char
            osc_strfmt_uint8(buf, nn, a->w.C);
            break;
        case 'u': // unsigned 32-bit int
            osc_strfmt_int16(buf, nn, a->w.u);
            break;
        case 'U': // unsigned 64-bit int
            osc_strfmt_uint16(buf, nn, a->w.U);
            break;
        case 'T': // true
            osc_strfmt_bool(buf, nn, 'T');
            break;
        case 'F': // false
            osc_strfmt_bool(buf, nn, 'F');
            break;
        case 'N': // NULL
            osc_strfmt_null(buf, nn);
            break;
        case 't': // timetag
            stringlen = osc_strfmt_timetag(buf, nn, a->w.t);
            break;
        case 'b': // blob
            stringlen = osc_strfmt_blob(buf, nn, a->w.b);
            break;
    }
    
    return String(buf);
}

ScopedPointer<OdotBundle> OdotBundle::oscAddressGetBundle(const String& address)
{
    t_osc_msg_ar_u *msg_ar = osc_bundle_u_lookupAddress( bundle, address.getCharPointer(), 1 );
    if( !msg_ar || osc_message_array_u_getLen(msg_ar) == 0 )
    {
        return nullptr;
    }
    
    t_osc_msg_u *m = osc_message_array_u_get(msg_ar, 0);
    t_osc_atom_u *a = osc_message_u_getArg(m, 0);
    t_osc_bndl_u *b = osc_atom_u_getBndl(a);
    if( !b )
    {
        return nullptr;
    }
    
    return ScopedPointer<OdotBundle>( new OdotBundle(b) );
    
}

t_osc_msg_ar_u *OdotBundle::lookupAddress( const String& address)
{
    return osc_bundle_u_lookupAddress( bundle, address.getCharPointer(), 1 );
}

bool OdotBundle::addressExists( const String& address )
{
    int res;
    const char *addr = address.getCharPointer();
    osc_bundle_u_addressExists( bundle, (char *)addr, 1, &res );
    return (res == 1);
}






