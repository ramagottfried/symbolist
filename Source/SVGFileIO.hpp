#pragma once

#include "pugixml.hpp"
#include "Symbol.h"

class SVGFileIO
{
public:
    void read( const char * filename );

    
    
private:
    pugi::xml_document m_doc;

};
