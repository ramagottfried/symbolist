#pragma once

#include "pugixml.hpp"
#include "Symbol.h"
#include "StringTools.hpp"

class SVGParser
{
public:
    vector< unique_ptr<Symbol> > parse( pugi::xml_document& doc );
 
private:
    Symbol nodeToSymbol( pugi::xml_node& node );
    Symbol parseStyle( pugi::xml_attribute& style );
    
};

