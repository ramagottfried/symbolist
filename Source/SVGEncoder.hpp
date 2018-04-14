#pragma once

#include "pugixml.hpp"
#include "Symbol.h"
#include "StringTools.hpp"

class SVGEncoder
{
public:
     pugi::xml_document encode( vector< unique_ptr<Symbol> >& score );
    
private:
     pugi::xml_node symbolToNode( pugi::xml_node& svg, Symbol& sym );
     const char * encodeStyle( const OdotBundle& style );
    
};
