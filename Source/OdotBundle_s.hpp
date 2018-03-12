#pragma once
#include <iostream>
#include "osc_bundle_s.h"
#include "OdotPointers.h"
#include "OdotMessage.hpp"
#include "OdotAtom.hpp"

using namespace std;

class OdotBundle;

class OdotBundle_s
{
public:
    OdotBundle_s();
    OdotBundle_s( const OdotBundle_s& src );
    OdotBundle_s( const t_osc_bndl_s *src );
    
    OdotBundle_s( OdotBundle_s&& src ) = default;
    OdotBundle_s& operator=( OdotBundle_s&& src ) = default;
    
    OdotBundle_s& operator= ( const OdotBundle_s& src );
    ~OdotBundle_s(){}
    
    void clear();
    void print( int level = 0 ) const;
    
    inline const t_osc_bndl_s * get_o_ptr() { return ptr.get(); }
    inline t_osc_bndl_s * release(){ return ptr.release(); }
    
    OdotBundle deserialize();
    
private:
    
    odot::OdotBundlePtr_s ptr;
    
};


