
#include "SymbolGroupComponent.h"
#include "MainComponent.h"

SymbolGroupComponent::SymbolGroupComponent( const Symbol& s ) : BaseComponent( s )
{
    //importGroupFromSymbol( s ); // has its own method for that
}

SymbolGroupComponent::~SymbolGroupComponent() {}



void SymbolGroupComponent::paint ( Graphics& g )
{
    g.setColour( Colours::darkcyan );
    const Rectangle<int> b = ((BaseComponent*) this)->getLocalBounds();
    const float dashLength[2] = {3.0 , 4.0};
    int ndashLengths = 2;
    g.drawDashedLine(Line<float>( b.getX(), b.getY(), b.getX() + b.getWidth(), b.getY() ), dashLength , ndashLengths );
    g.drawDashedLine(Line<float>( b.getX() + b.getWidth(), b.getY(), b.getX() + b.getWidth(), b.getY() + b. getHeight() ), dashLength , ndashLengths );
    g.drawDashedLine(Line<float>( b.getX() + b.getWidth() , b.getY() + b.getHeight() , b.getX() , b.getY() + b. getHeight() ), dashLength , ndashLengths );
    g.drawDashedLine(Line<float>( b.getX() , b.getY() + b.getHeight() , b.getX() , b.getY()), dashLength , ndashLengths );
}




int SymbolGroupComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = BaseComponent::addSymbolMessages( s, base_address );
    
    s->addOSCMessage( (String(base_address) += "/numsymbols") , (int)getNumSubcomponents() );
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        String base = String(base_address) += String("/subsymbol/") += String(i+1) ; // we start at 1 .. (?)
        messages_added += getSubcomponent(i)->addSymbolMessages( s, base );
    }
    
    return messages_added;
}

void SymbolGroupComponent::importFromSymbol( const Symbol &s )
{
    
    BaseComponent::importFromSymbol(s);
    
    int n = s.getOSCMessageValue("/numsymbols").getInt32();
    std::cout << "Importing Group of " << n << " symbols..." << std::endl;
    for (int i = 0; i < n; i++ )
    {
        String filter = "/subsymbol/" + String(i+1) ;   // we start at 1 .. (?)
        cout << filter << endl;
        Symbol sub_s = s.makeSubSymbol( filter );
        BaseComponent* c = SymbolistMainComponent::makeComponentFromSymbol( &sub_s );
        if ( c != NULL) addSubcomponent( c );
        else cout << "Error importing subsymbol #" << i << endl;
    }
}

