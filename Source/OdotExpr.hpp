#pragma once

#include <iostream>
#include "osc_expr.h"
#include "osc_expr_u.h"

#include "OdotPointers.h"

using namespace std;

#define OSC_EXPR_ARG_TYPE_NUMBER 0x1
#define OSC_EXPR_ARG_TYPE_LIST 0x2
#define OSC_EXPR_ARG_TYPE_STRING 0x4
#define OSC_EXPR_ARG_TYPE_ATOM 0x8
#define OSC_EXPR_ARG_TYPE_EXPR 0x10
#define OSC_EXPR_ARG_TYPE_OSCADDRESS 0x20
#define OSC_EXPR_ARG_TYPE_BOOLEAN 0x40
#define OSC_EXPR_ARG_TYPE_FUNCTION 0x80
#define OSC_EXPR_ARG_TYPE_TIMETAG 0x100

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

    void print() const;

    inline t_osc_expr * get_o_ptr() const { return ptr.get(); }
    
private:
    
    odot::OdotExprPtr ptr;
    
};
