#include "OdotAtom.hpp"
#include "OdotBundle.hpp"

OdotAtom::OdotAtom()
{
    ptr = odot::newOdotAtomPtr();
//    D_(std::cout << "new empty pointer " << &ptr << " " << ptr.get() << std::endl;)
}

OdotAtom::OdotAtom( const OdotAtom& src )
{
//    D_(cout << __func__  << " copy object \n";)
    t_osc_atom_u *a = osc_atom_u_copy( src.ptr.get() );
    ptr = odot::newOdotAtomPtr( a );
}

// assumes we are not responible for the pointer (to work with osc_message_u_getArg()
OdotAtom::OdotAtom( t_osc_atom_u * src )
{
//    D_(cout << __func__ << " copy from pointer \n";)
    t_osc_atom_u *a = osc_atom_u_copy( src );
    ptr = odot::newOdotAtomPtr( a );
}

OdotAtom& OdotAtom::operator=( const OdotAtom& src )
{
///    D_(std::cout << "copy= \n";)
    
    if( this != &src )
    {
        t_osc_atom_u *a = osc_atom_u_copy( src.ptr.get() );
        ptr = odot::newOdotAtomPtr( a );
    }
    
    return *this;
}


bool OdotAtom::operator!=( const OdotAtom& src ) const
{
    return !((*this) == src);
}

bool OdotAtom::operator==( const OdotAtom& src ) const
{
    // once there is a ref version, check for pointer address first
    // and replace pointer.get with wrapper function
    if( ptr.get() == src.ptr.get() )
        return true;
    
    if( getType() != src.getType() )
        return false;
    
    switch ( osc_atom_u_getTypetag( ptr.get() ) )
    {
        case 'f':
            return getFloat() == src.getFloat();
        case 'd':
            return getDouble() == src.getDouble();
        case 'i':
            return getInt()== src.getInt();
        case 's':
            return getString() == src.getString();
        case OSC_BUNDLE_TYPETAG:
            return getBundle() == src.getBundle();
        default:
            return false;
    }
}


OdotBundle OdotAtom::getBundle() const
{
    return OdotBundle( osc_atom_u_getBndl( ptr.get() ) );
}

t_osc_bndl_u * OdotAtom::getBundlePtr()
{
    return osc_atom_u_getBndl( ptr.get() );
}

string OdotAtom::getString() const
{
    size_t len = osc_atom_u_getStringLen( ptr.get() );
    char buf[ len+1 ];
    memset(&buf, '\0', len+1);
    char *buf_ptr = buf;
    osc_atom_u_getString( ptr.get(), len, &buf_ptr );
    return string( buf_ptr );
}

OdotAtom::OdotAtomType OdotAtom::getType() const
{
    switch ( osc_atom_u_getTypetag( ptr.get() ) )
    {
        case 'f':
            return O_ATOM_FLOAT;
        case 'd':
            return O_ATOM_DOUBLE;
        case 'i':
            return O_ATOM_INT;
        case 's':
            return O_ATOM_STRING;
        case OSC_BUNDLE_TYPETAG:
            return O_ATOM_BUNDLE;
        default:
            return O_ATOM_NONE;
    }
    
}
