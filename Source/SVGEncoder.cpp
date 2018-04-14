#include "SVGEncoder.hpp"

const char * SVGEncoder::encodeStyle( const OdotBundle& style )
{
    
    string styleCSS;
    pugi::xml_attribute styleAttr;
    
    styleAttr.set_name("style");
    
    //
    Symbol symbol;
//    string styleCSS = style.value();
    auto elements = split(styleCSS, ';' );
    for( auto estr : elements )
    {
        auto e = split(estr, ':');
        
        std::istringstream testF( e[1] );
        std::istringstream testI( e[1] );
        double f;
        int i;
        if( (testI >> i) )
        {
            symbol.addMessage( string("/") + e[0], i );
        }
        else if( (testF >> f)  )
        {
            symbol.addMessage( string("/") + e[0], f );
        }
        else
        {
            symbol.addMessage( string("/") + e[0], e[1] );
        }
    }
    
    
    return styleCSS.c_str();
}

pugi::xml_node SVGEncoder::symbolToNode( pugi::xml_node& svg, Symbol& sym )
{
    pugi::xml_node node = svg.append_child();
    
    string symbolPrefix = "/symbol";
    
    string type = sym.getMessage("/type").getString();
    
    if( type == "group" || type == "staff" )
    {
        // pausing here, and need to think about absolute vs relative coordinates
        // considering whether like SVG symbolist symbols should be stored in absolute coordinates
        
        /*
        node.set_name("g");
        node.append_attribute("id") = sym.getMessage("/id").getString().c_str();
            
            // group name
            
            if( addr.compare( 0, symbolPrefix.length(), symbolPrefix ) == 0 )
            {
                pugi::xml_node groupNode
            }
        */
    }
    else
    {
        
        for( auto msg : sym.getMessageArray() )
        {
            string addr = msg.getAddress();
            if(  addr == "/type" )
            {
                node.set_name( msg.getString().c_str() );
            }
            else if( addr == "/style" )
                node.append_attribute("style") = encodeStyle( msg.getBundle() );
            else
            {
                string attrName = split(msg.getAddress(), '/' )[0];
                node.append_attribute( attrName.c_str() ) = msg.getString().c_str();
            }
        }
    }

    return node;
}

pugi::xml_document SVGEncoder::encode( vector< unique_ptr<Symbol> >& score )
{
    pugi::xml_document doc;
    
    pugi::xml_node svg = doc.append_child("svg");
    svg.append_attribute("version") = "1.0";
    svg.append_attribute("xmlns") = "http://www.w3.org/2000/svg";
    svg.append_attribute("xmlns:xlink") = "http://www.w3.org/1999/xlink";
    svg.append_attribute("x") = "0px";
    svg.append_attribute("y") = "0px";
    svg.append_attribute("width") = "841.9px"; //<< set page size here
    svg.append_attribute("height") = "595.3px"; //<< set page size here
    
    for( int i = 0; i < score.size(); i++ )
    {
        Symbol * sym = score[i].get();
        symbolToNode( svg, *sym );
    }
    
    // make SVG header and main <svg> node
    // iterate score and make SVG nodes and make them children of <svg>
        
    return doc;
}
