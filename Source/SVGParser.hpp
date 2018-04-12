#pragma once

#include "SVGFileIO.hpp"

class SVGParser
{

    Symbol nodeToSymbol( pugi::xml_node& node );
    Symbol parseStyle( pugi::xml_attribute& style );

public:
    vector< unique_ptr<Symbol> > parse( pugi::xml_document& doc );
    
};

