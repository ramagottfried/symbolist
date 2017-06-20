
#include "SymbolGroupComponent.h"

#include "PageComponent.h"

SymbolGroupComponent::SymbolGroupComponent( const Symbol& s ) : BaseComponent( s )
{
    //importGroupFromSymbol( s ); // has its own method for that
}

SymbolGroupComponent::~SymbolGroupComponent() {}



void SymbolGroupComponent::paint ( Graphics& g )
{
    BaseComponent::paint( g );
    g.setColour( Colours::lightgrey );
    const Rectangle<int> b = ((BaseComponent*) this)->getLocalBounds();
    g.drawRect(b);
}


void SymbolGroupComponent::selectComponent()
{
    if ( ! in_edit_mode )
    {
        BaseComponent::selectComponent();
        for (int i = 0; i < getNumSubcomponents(); i++ )
        {
            getSubcomponent(i)->selectComponent();
        }
    }
}

void SymbolGroupComponent::deselectComponent()
{
    BaseComponent::deselectComponent();
    for (int i = 0; i < getNumSubcomponents(); i++ )
    {
        getSubcomponent(i)->deselectComponent();
    }
}


void SymbolGroupComponent::mouseDown( const MouseEvent& event )
{
    BaseComponent::mouseDown(event);
    if ( in_edit_mode ) ScoreComponent::mouseDown(event);
}

void SymbolGroupComponent::mouseUp( const MouseEvent& event )
{
    if ( in_edit_mode ) ScoreComponent::mouseUp(event);
    else BaseComponent::mouseUp(event);
}

void SymbolGroupComponent::mouseDrag( const MouseEvent& event )
{
    if ( in_edit_mode ) ScoreComponent::mouseDrag(event);
    else BaseComponent::mouseDrag(event);
}

/*============================*
 * SYMBOL MANAGEMENT
 *============================*/

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
        //cout << "IMPORT FROM: " << filter << endl;
        Symbol sub_s = s.makeSubSymbol( filter );
        BaseComponent* c = SymbolistHandler::makeComponentFromSymbol( &sub_s );
        if ( c != NULL) addSubcomponent( c );
        else cout << "Error importing subsymbol #" << i << endl;
    }
}

