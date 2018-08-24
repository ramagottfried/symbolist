#pragma once

#include "pugixml.hpp"
#include "StringTools.hpp"
#include "SymbolistPath.hpp"


class SVGEncoder
{
public:
    /**
     *  `encodeScore` takes a Score formatted bundle and ouputs a vector of SVG files one for each page.
     *
     *  @param score    `/score` bundle in Symbolist format
     *
     *  @return         vector of SVG files, stored as pugi::xml_documents
     */
    
    vector<pugi::xml_document> encodeScore( const OdotBundle& score );
    
    pugi::xml_document encodeObjectGraphic( const OdotBundle& object, const SymbolistRect& parent_bounds, const string& type_addr );

    /**
     *  `createSVG` takes a Score formatted bundle and ouputs a vector of SVG files one for each page.
     *
     *  @param doc      reference for XML doc to add SVG header to
     *
     *  @param w        canvas width
     *
     *  @param h        canvas height
     *
     *  @return         reference to new SVG XML node
     */
    pugi::xml_node& createSVG( pugi::xml_document& doc, double w, double h );

    
    static string graphicObjectToJUCE( const OdotBundle& graphic );
    
    std::string toString( pugi::xml_node node )
    {
        xml_string_writer writer;
        node.print(writer);
        
        return writer.result;
        
    }
    
private:
    pugi::xml_node objectToNode( pugi::xml_node& parent, const SymbolistRect& parent_bounds, const OdotBundle& obj, const string& type_addr );
     const char * encodeStyle( const OdotBundle& style );
    
    
    struct xml_string_writer : pugi::xml_writer
    {
        std::string result;
        
        virtual void write(const void* data, size_t size)
        {
            result.append(static_cast<const char*>(data), size);
        }
    };
    
};
