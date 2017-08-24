
#include "StaffComponent.hpp"

#include "PageComponent.h"
#include "ScoreComponent.h"


void StaffComponent::importFromSymbol( const Symbol &s )
{
    clearAllSubcomponents();
    
    BaseComponent::importFromSymbol(s);
    
    String filter = "/staffSymbol";
    
    int pos = s.getOSCMessagePos(filter+"/type");
    if( pos == -1 )
    {
        cout << "no /staffSymbol/type found" << endl;
        return;
    }
    
    Symbol sub_s = s.makeSubSymbol( filter );
    BaseComponent* c = getSymbolistHandler()->makeComponentFromSymbol( &sub_s , false );
    
    if ( c != NULL)
        addSubcomponent( c );
    else
        cout << "Error importing staffSymbol " << endl;
        
}

int StaffComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    cout << "current bundle " << endl;
    if( auto current_s = getScoreSymbolPointer() )
    {
        current_s->printBundle();
    }
    else
    {
        cout << "no current bundle " << endl;
    }
    
    int messages_added = BaseComponent::addSymbolMessages( s, base_address );
    
    cout << "addSymbolMessages pre symbol bundle, nsubs " << getNumSubcomponents() << endl;
    s->printBundle();
    
    String addr = base_address + "/staffSymbol";
    if( getNumSubcomponents() && s->getOSCMessagePos(addr) == -1 )
    {
        auto sub_c = getSubcomponent(0);
        if( sub_c )
        {
            cout << "addSymbolMessages for subcomponent at addr " << addr << endl;

            messages_added += ((BaseComponent*)sub_c)->addSymbolMessages( s, addr );
        }
        else
        {
            cout << "no subcomponent found" << endl;
        }
    }
    

    cout << "current --s-- (not attached) bundle " << endl;
    s->printBundle();
    return messages_added;
}

