#pragma once
#include <iostream>
#include <string>
#include <memory>
#include <vector>
#include "osc_message_u.h"
#include "OdotAtom.hpp"
#include "OdotPointers.h"

// added Juce String support
#include "../JuceLibraryCode/JuceHeader.h"

using namespace std;

class OdotBundle;

class OdotMessage
{
public:
    
    /*  ======= construct, copy, move, destruct =======  */
    
    OdotMessage();
    OdotMessage( const char *name );
    OdotMessage( const string& address );
    OdotMessage( const OdotMessage& src );
    OdotMessage( const t_osc_msg_u * src );
    
    template <typename... Ts>
    OdotMessage (const char * address, Ts&&... args)
    {
        ptr = odot::newOdotMessagePtr( osc_message_u_allocWithAddress( (char *)address ) );
        using expand = int[];
        (void) expand { 0, ((void)appendValue( std::forward<Ts>(args) ), 0) ... };
    }

    template <typename... Ts>
    OdotMessage (const string& address, Ts&&... args)
    {
        ptr = odot::newOdotMessagePtr( osc_message_u_allocWithAddress( (char *)address.c_str() ) );
        using expand = int[];
        (void)expand{0, ((void)appendValue( std::forward<Ts>(args) ), 0) ... };
    }

    OdotMessage& operator=( const OdotMessage& src );
    OdotMessage( OdotMessage&& src ) = default;
    OdotMessage& operator=( OdotMessage&& src ) = default;
    ~OdotMessage(){}
    
    /* ======= get values from message ======= */
    
    string getAddress() const { return string( osc_message_u_getAddress( ptr.get() ) ); }
    
    OdotAtom operator[](int i);
    
    vector<OdotAtom> getAtoms();
    
    inline string getString(int argIndex = 0){ return string( osc_atom_u_getStringPtr( osc_message_u_getArg( ptr.get(), argIndex ) ) ); }
    inline float getFloat(int argIndex = 0){ return  osc_atom_u_getFloat( osc_message_u_getArg( ptr.get(), argIndex ) ); }
    inline int getInt(int argIndex = 0){ return  osc_atom_u_getInt( osc_message_u_getArg( ptr.get(), argIndex ) ); }
    OdotBundle getBundle(int argIndex = 0);

    
    /* ======= set message values ======= */
    
    inline void rename( const string& name ) { osc_message_u_setAddress( ptr.get(), name.c_str() ); }
    
    void appendValue( const t_osc_atom_u *atom );
    void appendValue( OdotBundle& bndl );

    void appendValue( OdotMessage& msg );
    inline void appendValue( OdotAtom& atom ){ appendValue( atom.get_o_ptr() ); }
    void appendValue( const OdotBundle& bndl );
    
    inline void appendValue( const t_osc_bndl_u * bndl ){   osc_message_u_appendBndl_u( ptr.get(), (t_osc_bndl_u *)bndl ); }
    inline void appendValue( double val ){   osc_message_u_appendDouble( ptr.get(), val );           }
    inline void appendValue( float val ){    osc_message_u_appendFloat(  ptr.get(), val );           }
    inline void appendValue( int val ){      osc_message_u_appendInt32(  ptr.get(), val );           }
    inline void appendValue( string& val ){  osc_message_u_appendString( ptr.get(), val.c_str() );   }
    inline void appendValue( const char * val ){   osc_message_u_appendString( ptr.get(), val );     }
    
    // Juce String add-on
    inline void appendValue( String& val ){   osc_message_u_appendString( ptr.get(), val.getCharPointer() );     }
    inline void appendValue( const String& val ){   osc_message_u_appendString( ptr.get(), val.getCharPointer() );     }

    
    template <typename... Ts>
    void  appendValueList(Ts&&... args)
    {
        using expand = int[];
        (void) expand { 0, ((void)appendValue( std::forward<Ts>(args) ), 0) ... };
    }
    
    /* ======= query ======= */
    
    inline int size(){ return osc_message_u_getArgCount( ptr.get() ); }
    inline bool isEmpty(){ return ( osc_message_u_getArgCount( ptr.get() ) == 0); }
    void print();
    
    /* ======= pointers ======= */
    
    t_osc_msg_u * release(){ return ptr.release(); }
    inline t_osc_msg_u *get_o_ptr(){ return ptr.get(); }

    
private:
    
    odot::OdotMessagePtr ptr;

};
