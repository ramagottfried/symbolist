#pragma once

#include <iostream>
#include "osc_expr.h"
#include "OdotPointers.h"

using namespace std;

class OdotExpr
{
public:
    
    OdotExpr();
    OdotExpr( const OdotExpr& src );
    OdotExpr( const string& expr );
    OdotExpr( const char * expr );

    OdotExpr( t_osc_expr * src );
    OdotExpr& operator=( const OdotExpr& src );
    OdotExpr& operator=( const string& expr );
    OdotExpr& operator=( const char * expr );

    OdotExpr( OdotExpr&& src ) = default;
    OdotExpr& operator=( OdotExpr&& src ) = default;
    
    ~OdotExpr(){}
    
    inline t_osc_expr * get_o_ptr() const { return ptr.get(); }
private:
    
    odot::OdotExprPtr ptr;
    
};
