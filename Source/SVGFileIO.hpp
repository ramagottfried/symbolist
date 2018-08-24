#pragma once

#include "pugixml.hpp"
#include "SVGParser.hpp"
#include "SVGEncoder.hpp"


class SVGFileIO
{
public:
    
    OdotBundle read( const char * filename );
    void write( const OdotBundle& score, const char * filename );

private:
    pugi::xml_document m_doc;

};
