#pragma once

#include "pugixml.hpp"
#include "Symbol.h"

class SVGFileIO
{
    vector< unique_ptr<Symbol> > m_score;
    pugi::xml_document m_doc;

    struct simple_walker;
    void doParse( pugi::xml_parse_result result );
    
public:
    
    void read( const char * filename );
    
    
};
