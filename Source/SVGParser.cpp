#include "SVGParser.hpp"
#include "StringTools.hpp"

Symbol SVGParser::parseStyle( pugi::xml_attribute& style )
{
    Symbol symbol;
    string styleCSS = style.value();
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
    return symbol;
}

Symbol SVGParser::nodeToSymbol( pugi::xml_node& node )
{
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
    return sym;
}

vector< unique_ptr<Symbol> > SVGParser::parse( pugi::xml_document& doc )
{
    vector< unique_ptr<Symbol> > score;

    for( auto node : doc.child("svg").children() )
    {
        score.emplace_back( unique_ptr<Symbol>( new Symbol( nodeToSymbol( node ) ) ) );
        //score.back()->print();
    }

    return score;
}
