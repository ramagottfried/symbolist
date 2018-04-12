#pragma once

#include "pugixml.hpp"
#include "Symbol.h"

class SVGFileIO
{
    pugi::xml_document m_doc;
    
public:
    
    void read( const char * filename );
    inline pugi::xml_document& getDoc(){ return m_doc; }
    
};
