#include "SVGEncoder.hpp"
#include "StringTools.hpp"

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

pugi::xml_node SVGEncoder::symbolToNode( Symbol& sym )
{
    pugi::xml_node node;
    
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
            else if( addr.compare( 0, symbolPrefix.length(), symbolPrefix ) == 0 )
            {
// remove this part, moving <g> </g> groups to the above case
                
            }
            else
            {
                string attrName = split(msg.getAddress(), '/' )[0];
                node.append_attribute( attrName.c_str() ) = msg.getString().c_str();
            }
        }
    }
    
    /*
    // from parse
    Symbol sym;
    
    sym.addMessage( "/type", node.name() );
    
    for (pugi::xml_attribute attr : node.attributes())
    {
        if( !strcmp( attr.name(), "style" ) )
            sym.addMessage("/style", parseStyle(attr) );
        else
            sym.addMessage( string("/") + attr.name(), attr.value() );
    }
    
    int count = 1;
    for ( pugi::xml_node child : node.children() )
    {
        sym.addMessage( "/symbol/" + to_string(count++), nodeToSymbol( child ) );
    }
     */
    return node;
}

pugi::xml_document SVGEncoder::encode( vector< unique_ptr<Symbol> >& score )
{
    pugi::xml_document doc;
    
    // make SVG header and main <svg> node
    // iterate score and make SVG nodes and make them children of <svg>
    
    return doc;
}
