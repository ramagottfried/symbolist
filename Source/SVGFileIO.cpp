
#include "SVGFileIO.hpp"

void SVGFileIO::read( const char * filename )
{
    pugi::xml_parse_result result = m_doc.load_file( filename );
    
    if( result )
        doParse( result );
    else
    {
        std::cout << "XML [" << filename << "] parsed with errors, attr value: [" << m_doc.child("node").attribute("attr").value() << "]\n";
        std::cout << "Error description: " << result.description() << "\n";
        std::cout << "Error offset: " << result.offset << " (error at [..." << (filename + result.offset) << "]\n\n";
    }
}


struct SVGFileIO::simple_walker : pugi::xml_tree_walker
{
    virtual bool for_each( pugi::xml_node& node )
    {
        for (int i = 0; i < depth(); ++i) std::cout << "  "; // indentation
        
        std::cout << node.type() << ": name='" << node.name() << "', value='" << node.value() <<"'";
        
        std::cout << " Attrs: ";
        for (pugi::xml_attribute attr : node.attributes())
        {
            std::cout << " " << attr.name() << "=" << attr.value();
        }
        std::cout << std::endl;
        
        return true; // continue traversal
    }
};

void SVGFileIO::doParse( pugi::xml_parse_result result )
{
    simple_walker walker;
    m_doc.traverse( walker );
}


