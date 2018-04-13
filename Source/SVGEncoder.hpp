#pragma once

#include "SVGFileIO.hpp"

class SVGEncoder
{
public:
     pugi::xml_document encode( vector< unique_ptr<Symbol> >& score );
    
private:
     pugi::xml_node symbolToNode( Symbol& sym );
     const char * encodeStyle( const OdotBundle& style );
    
};
