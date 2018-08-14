
#include "OdotMessage.hpp"
#include "OdotBundle.hpp"

OdotMessage::OdotMessage()
{
    ptr = odot::newOdotMessagePtr();
//    D_(std::cout << "new pointer " << &ptr << " " << ptr.get() << std::endl;)
}

OdotMessage::OdotMessage( const char *name )
{
    ptr = odot::newOdotMessagePtr();
    osc_message_u_setAddress(ptr.get(), name );
//    D_(std::cout << "new pointer " << &ptr << " " << ptr.get() << std::endl;)
}

OdotMessage::OdotMessage( const string& address )
{
    ptr = odot::newOdotMessagePtr();
    osc_message_u_setAddress(ptr.get(), address.c_str() );
//    D_(std::cout << "new pointer " << &ptr << " " << ptr.get() << std::endl;)
}


OdotMessage::OdotMessage( const OdotMessage& src )
{
//    D_(cout << __func__ << " copy from object \n";)
    t_osc_msg_u *m = osc_message_u_alloc();
    osc_message_u_deepCopy(&m, src.ptr.get() );
    
    ptr = odot::newOdotMessagePtr( m );
}

OdotMessage::OdotMessage( const t_osc_msg_u * src )
{
//    D_(cout << __func__  << " copy from odot pointer \n";)
    t_osc_msg_u *m = osc_message_u_alloc();
    osc_message_u_deepCopy(&m, (t_osc_msg_u *)src );
    
    ptr = odot::newOdotMessagePtr( m );
}

OdotMessage& OdotMessage::operator=( const OdotMessage& src )
{
//    D_(std::cout << "copy= \n";)
    
    if( this != &src )
    {
        t_osc_msg_u *m = osc_message_u_alloc();
        osc_message_u_deepCopy(&m, src.ptr.get() );
        
        ptr = odot::newOdotMessagePtr( m );
    }
    
    return *this;
}

bool OdotMessage::operator!=( const OdotMessage& src ) const
{
    return !((*this) == src);
}

bool OdotMessage::operator==( const OdotMessage& src ) const
{
    if( ptr.get() == src.ptr.get() )
        return true;
    
    if( size() != src.size() )
        return false;
    
    if( getAddress() != src.getAddress() )
        return false;
    
    for( int i = 0; i < size(); i++ )
    {
        if( (*this)[i] != src[i] )
            return false;
    }
    
    return true;
    
}

OdotAtom OdotMessage::operator[](int i) const
{
    t_osc_atom_u *a = osc_message_u_getArg( ptr.get(), i );
    
    if( a )
        return OdotAtom(a);
    else
        return OdotAtom();
    
}

void OdotMessage::print() const
{
    char buf[32768];
    memset(buf, '\0', 32768);

    char *buf_ptr = buf;
    cout << ":::: ODOT MESSAGE ::::" << endl;
    cout << "   ( " << ptr.get() << " )" << endl;
    
    cout << osc_message_u_getAddress( ptr.get() );
    
    int argcount = osc_message_u_getArgCount( ptr.get() );
    for( int i = 0; i < argcount; i++ )
    {
        t_osc_atom_u *a = osc_message_u_getArg(ptr.get(), i);
        if( osc_atom_u_getTypetag(a) == OSC_BUNDLE_TYPETAG )
        {
            cout << "\t{ \n";
            OdotBundle b( osc_atom_u_getBndl(a) );
            b.print(1);
            cout << " } ";
        }
        else
        {
            osc_atom_u_getString( a, 32768, &buf_ptr );
            cout << "\t" << buf_ptr;
        }
    }

    cout << endl;
    cout << ":::::::::::::::::::::::" << endl;
}

string OdotMessage::getJSON()
{
    char buf[256];
    char *buf_ptr = buf;

    string JSON = "\"";
    JSON += osc_message_u_getAddress( ptr.get() );
    JSON += "\" : ";
    
    int argcount = osc_message_u_getArgCount( ptr.get() );

    if( argcount > 1 )
        JSON += " [ ";
    
    bool addcomma = false;
    for( int i = 0; i < argcount; i++ )
    {
        if( addcomma )
            JSON += ",";
        
        t_osc_atom_u *o_atom = osc_message_u_getArg( ptr.get() , i);
        char type = osc_atom_u_getTypetag(o_atom);
        switch (type) {
            case 's':
                osc_atom_u_getString( o_atom, 256, &buf_ptr );
                JSON += "\"";
                JSON += buf;
                JSON += "\"";
                break;
            case OSC_BUNDLE_TYPETAG:
                JSON += OdotBundle(osc_atom_u_getBndl( o_atom )).getJSON();
                break;
            default:
                osc_atom_u_getString( o_atom, 256, &buf_ptr );
                JSON += buf;
                break;
        }
        
        addcomma = true;
        
    }
    
    if( argcount > 1 )
        JSON += " ] ";
    
    return JSON;
}

void OdotMessage::appendValue( const t_osc_atom_u *atom )
{
    // note osc_message_u_append functions allocate a new atom internally
    switch ( osc_atom_u_getTypetag( (t_osc_atom_u *)atom ) )
    {
        case 'f':
            osc_message_u_appendFloat(  ptr.get(), osc_atom_u_getFloat( (t_osc_atom_u *)atom ) );
            break;
        case 'd':
            osc_message_u_appendDouble(  ptr.get(), osc_atom_u_getDouble( (t_osc_atom_u *)atom ) );
            break;
        case 'i':
            osc_message_u_appendInt32(  ptr.get(), osc_atom_u_getInt( (t_osc_atom_u *)atom ) );
            break;
        case 's':
            osc_message_u_appendString(  ptr.get(), osc_atom_u_getStringPtr( (t_osc_atom_u *)atom ) );
            break;
        case OSC_BUNDLE_TYPETAG:
            osc_message_u_appendBndl_u(  ptr.get(), osc_atom_u_getBndl( (t_osc_atom_u *)atom ) );
            break;
        default:
            break;
    }
}

void OdotMessage::appendValue( const t_osc_bndl_u * bndl )
{
    osc_message_u_appendBndl_u( ptr.get(), (t_osc_bndl_u *)bndl );
}

void OdotMessage::appendValue( OdotBundle& bndl )
{
    osc_message_u_appendBndl_u( ptr.get(), OdotBundle( bndl ).release() );
}

void OdotMessage::appendValue( OdotMessage& msg )
{
    OdotBundle bndl( msg ); // adding a message to a message adds as subbundle
    appendValue( bndl.release() ); // the allocated atom now holds the memory
}

void OdotMessage::appendValue( const OdotBundle& bndl )
{
    osc_message_u_appendBndl_u( ptr.get(), (t_osc_bndl_u *)OdotBundle( bndl ).release() );
}

vector<OdotAtom> OdotMessage::getAtoms()
{
    vector<OdotAtom> atom_array;
    for( int i = 0; i < osc_message_u_getArgCount( ptr.get() ); i++ )
    {
        atom_array.emplace_back( OdotAtom( osc_message_u_getArg( ptr.get(), i ) ) );
    }
    return atom_array;
}

OdotBundle OdotMessage::getBundle( int argIndex ) const
{
    return OdotBundle( osc_atom_u_getBndl( osc_message_u_getArg( ptr.get(), argIndex ) ) );
}
