
#include "SymbolGroupComponent.h"

#include "PageComponent.h"
#include "ScoreComponent.h"


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



void SymbolGroupComponent::setEditMode( bool val )
{
    BaseComponent::setEditMode(val);
    
    if (val == false ) // exit
    {
        ScoreComponent* sc = ((ScoreComponent*)getParentComponent());
        sc->addToSelection(this);
        
        if ( getNumSubcomponents() == 1 )
        {
            sc->ungroupSelectedSymbols();
        }
        else if ( getNumSubcomponents() == 0 )
        {
            sc->deleteSelectedComponents();
        }
    }
}

/*============================*
 * SYMBOL MANAGEMENT
 *============================*/

int SymbolGroupComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = BaseComponent::addSymbolMessages( s, base_address );
    
    String addr = base_address + "/numsymbols";
    
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,     (int)getNumSubcomponents() );
        messages_added++;
    }
    
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        addr = base_address + "/subsymbol/" + String(i+1);
        
        if( s->getOSCMessagePos(addr) == -1 )
        {
            messages_added += ((BaseComponent*)getSubcomponent(i))->addSymbolMessages( s, addr );
        }
    }
    
    return messages_added;
}


void SymbolGroupComponent::importFromSymbol( const Symbol &s )
{
    
    BaseComponent::importFromSymbol(s);
    
    int pos = s.getOSCMessagePos("/numsymbols");
    
    if ( pos >= 0 )
    {
        int n_symbols = s.getOSCMessageValue(pos).getInt32();
        std::cout << "Importing Group of " << n_symbols << " symbols..." << std::endl;
        for (int i = 0; i < n_symbols; i++ )
        {
            String filter = "/subsymbol/" + String(i+1) ;   // we start at 1 .. (?)
            //cout << "IMPORT FROM: " << filter << endl;
            Symbol sub_s = s.makeSubSymbol( filter );
            BaseComponent* c = SymbolistHandler::makeComponentFromSymbol( &sub_s , false );
            if ( c != NULL) addSubcomponent( c );
            else cout << "Error importing subsymbol #" << i << endl;
        }
    }
}

