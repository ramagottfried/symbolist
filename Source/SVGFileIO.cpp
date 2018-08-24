#include "SVGFileIO.hpp"


OdotBundle SVGFileIO::read( const char * filename )
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
        return OdotBundle();// importedscore;
    }
    else
    {
        DEBUG_FULL("XML [" << filename << "] parsed with errors, attr value: [" << m_doc.child("node").attribute("attr").value() << "]\n")
        DEBUG_FULL("Error description: " << result.description() << "\n")
        DEBUG_FULL("Error offset: " << result.offset << " (error at [..." << (filename + result.offset) << "]\n\n")
    }
    
    return OdotBundle();
}

void SVGFileIO::write( const OdotBundle& score, const char * filename )
{
    SVGEncoder encoder;
    pugi::xml_document svg = encoder.encodeScore( score );
    
    // print to std::cout
    // svg.save(std::cout);

    if( filename )
    {
        cout << "save file now" << endl;
    }
}
