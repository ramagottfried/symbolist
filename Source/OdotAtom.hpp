#pragma once
#include <iostream>
#include <string>
#include <memory>
#include "osc_atom_u.h"
#include "osc.h"
#include "OdotPointers.h"

using namespace std;

class OdotAtom
{
public:
    OdotAtom();
    OdotAtom( const OdotAtom& src );
    OdotAtom( t_osc_atom_u * src );
    OdotAtom& operator=( const OdotAtom& src );
    OdotAtom( OdotAtom&& src ) = default;
    OdotAtom& operator=( OdotAtom&& src ) = default;
    ~OdotAtom(){}
    
    inline void setValue( float v ){        osc_atom_u_setFloat( ptr.get(), v ); }
    inline void setValue( double v ){       osc_atom_u_setDouble( ptr.get(), v ); }
    inline void setValue( int v ){          osc_atom_u_setInt32( ptr.get(), v ); }
    inline void setValue( string& v ){      osc_atom_u_setString( ptr.get(), v.c_str() ); }
    inline void setValue( const char * v ){ osc_atom_u_setString( ptr.get(), v ); }
    
    template <typename T>
    OdotAtom ( T value ){
        ptr = odot::newOdotAtomPtr();
        setValue( std::forward<T>(value) );
    }
    
    inline float getFloat(){    return osc_atom_u_getFloat( ptr.get() ); }
    inline int getInt(){        return osc_atom_u_getInt( ptr.get() ); }
    inline string getString(){  return string( osc_atom_u_getStringPtr( ptr.get() ) ); }
    inline const char * getCharPtr(){  return osc_atom_u_getStringPtr( ptr.get() ); }
    
    enum OdotAtomType {
        O_ATOM_NONE,  // empty or unsupported type
        O_ATOM_FLOAT,
        O_ATOM_DOUBLE,
        O_ATOM_INT,
        O_ATOM_STRING,
        O_ATOM_BUNDLE
    };
    
    OdotAtomType getType();
    inline bool isEmpty(){ return (getType() == O_ATOM_NONE); }
    
    inline const t_osc_atom_u *get_o_ptr() { return ptr.get(); }
    inline t_osc_atom_u *release() { return ptr.release(); }
    
private:
    
    odot::OdotAtomPtr ptr;
};
