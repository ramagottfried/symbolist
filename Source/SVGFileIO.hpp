#pragma once

#include "pugixml.hpp"
#include "Symbol.h"

class SVGFileIO
{
    vector< unique_ptr<Symbol> > m_score;
    pugi::xml_document m_doc;

    struct m_simple_walker;
    
public:
    
    void read( const char * filename );
    
    
};
