#include "SVGEncoder.hpp"


const char * SVGEncoder::encodeStyle( const OdotBundle& style )
{
    
    string styleCSS;
    
    for(auto& m : style.getMessageArray() )
    {
        auto addr = m.getAddress();
        styleCSS += addr.substr(1, addr.length() )+":";
        styleCSS += m.getString() + ";";
    }
    
    return styleCSS.c_str();
}

pugi::xml_node SVGEncoder::objectToNode( pugi::xml_node& parent, const SymbolistRect& parent_bounds, const OdotBundle& obj, const string& type_addr )
{
    pugi::xml_node node = parent.append_child();
    
    if( type_addr == "/stave" || type_addr == "/system" || type_addr == "/page" )
    {
        
        node.set_name("g");
        
        SymbolistRect bounds( obj );
        bounds += parent_bounds.getPosition();
        
        // check for use (but JUCE doesn't support use tags)
        
        auto id = type_addr.c_str();
        
        if( type_addr == "/stave" )
        {
            
        }
        else if( type_addr == "/system" )
        {
            
        }
        else // page
        {
            
        }
        
    }
    else if( type_addr == "/symbol" )
    {
        node.set_name("path");
        node.append_attribute("id") = type_addr.c_str();

        auto path_d = obj.getMessage("/d").getString();
        
        SymbolistPath path( path_d );
        path.translate( parent_bounds.getPosition() );
    
        node.append_attribute("d") = path.toSVG().c_str();

        auto style = obj.getMessage("/style").getBundle();
        if( style.size() > 0 )
            node.append_attribute("style") = encodeStyle( style );

        
        /*
         // this older approach allows for different kinds of svg nodes
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
        */
        
    }

    return node;
}

// maybe the whole score should be in one XML file here, whith separate SVG canvases per <page> tag

vector<pugi::xml_document> SVGEncoder::encodeScore( const OdotBundle& score )
{
    
    auto pages = score.getMessage("/page").getBundle().getMessageArray();
    if( pages.size() == 0 )
        throw invalid_argument("no pages found in score");
    
    vector<pugi::xml_document> svg_vec;
    svg_vec.reserve( pages.size() );
    
    for( auto& page_msg : pages )
    {
        auto page_bndl = page_msg.getBundle();
        auto systems = page_bndl.getMessage("/system").getBundle();
        SymbolistRect page_bounds( page_bndl.getMessage("/bounds").getBundle() );
        
        pugi::xml_document doc;
        pugi::xml_node svg = createSVG(doc, page_bounds.getWidth(), page_bounds.getHeight());
        
        for( auto& sys : systems.getMessageArray() )
        {
            auto obj = sys.getBundle();
            objectToNode( svg, page_bounds, obj, "/system" );
        }
       
        // this is throwing an error
     //   svg_vec.emplace_back(doc);
    }
    
   
    
    // make SVG header and main <svg> node
    // iterate score and make SVG nodes and make them children of <svg>
        
    return svg_vec;
}

string SVGEncoder::graphicObjectToJUCE( const OdotBundle& graphic )
{
    
    string svgStr = "<svg><path d=\"" + graphic.getMessage("/d").getString() + "\"";
    
    char buf[32768];
    memset(buf, '\0', 32768);
    char *buf_ptr = buf;
    
    auto transform = graphic.getMessage("/transform").getBundle();
    if( transform.size() > 0 )
    {
        svgStr += " transform=\"";
        for(auto& m : transform.getMessageArray() )
        {
            auto addr = m.getAddress();
            svgStr += addr.substr(1, addr.length() )+"(";

            bool addComma = false;
            int argcount = osc_message_u_getArgCount( m.get_o_ptr() );
            for( int i = 0; i < argcount; i++ )
            {
                t_osc_atom_u *a = osc_message_u_getArg( m.get_o_ptr(), i);
                osc_atom_u_getString( a, 32768, &buf_ptr );
                if( addComma)
                    svgStr += ",";
                
                svgStr += buf_ptr;
                addComma = true;
            }
            
            svgStr += ") ";
        }
        svgStr += "\"";

    }
    
    auto style = graphic.getMessage("/style").getBundle();
    if( style.size() > 0 )
    {
        svgStr += " style=\"";
        for(auto& m : style.getMessageArray() )
        {
            auto addr = m.getAddress();
            svgStr += addr.substr(1, addr.length() )+":";
            svgStr += m.getString() + ";";
        }
        svgStr += "\"";
    }
    
    return (svgStr + " /></svg>");
}


/*
 *  creates SVG file from graphic bundle
 */
pugi::xml_document SVGEncoder::encodeObjectGraphic( const OdotBundle& object, const SymbolistRect& parent_bounds, const string& type_addr )
{
    pugi::xml_document doc;
    
    SymbolistRect bounds( object );
    pugi::xml_node svg = createSVG(doc, bounds.getWidth(), bounds.getHeight());
    
    objectToNode( svg, parent_bounds, object, type_addr );
    
    
    // make SVG header and main <svg> node
    // iterate score and make SVG nodes and make them children of <svg>
    
    return doc;
}

pugi::xml_node& SVGEncoder::createSVG( pugi::xml_document& doc, double w, double h )
{
    pugi::xml_node svg = doc.append_child("svg");
    svg.append_attribute("version") = "1.0";
    svg.append_attribute("xmlns") = "http://www.w3.org/2000/svg";
    svg.append_attribute("xmlns:xlink") = "http://www.w3.org/1999/xlink";
    svg.append_attribute("x") = "0px";
    svg.append_attribute("y") = "0px";
    svg.append_attribute("width") = (to_string(w)+"px").c_str(); //<< set page size here
    svg.append_attribute("height") = (to_string(h)+"px").c_str(); //<< set page size here
    return svg;
}
