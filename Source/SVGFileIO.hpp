#pragma once

#include "pugixml.hpp"
#include "SVGParser.hpp"
#include "SVGEncoder.hpp"


class SVGFileIO
{
public:
    
    vector<unique_ptr<Symbol>> read( const char * filename );
    void    write( vector<unique_ptr<Symbol>>& score, const char * filename );

private:
    pugi::xml_document m_doc;

};
