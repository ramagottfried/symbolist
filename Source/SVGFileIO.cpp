
#include "SVGFileIO.hpp"
#include "SVGParser.hpp"

void SVGFileIO::read( const char * filename )
{
    pugi::xml_parse_result result = m_doc.load_file( filename );
    
    if( result )
    {
        SVGParser parser;
        auto importedscore = parser.parse( m_doc );
        
        cout << "new score has " << importedscore.size() << endl;
        for( int i = 0; i < importedscore.size(); i++ )
        {
            importedscore[i]->print();
        }
        
    }
    else
    {
        std::cout << "XML [" << filename << "] parsed with errors, attr value: [" << m_doc.child("node").attribute("attr").value() << "]\n";
        std::cout << "Error description: " << result.description() << "\n";
        std::cout << "Error offset: " << result.offset << " (error at [..." << (filename + result.offset) << "]\n\n";
    }
}
