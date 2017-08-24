
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
    int messages_added = BaseComponent::addSymbolMessages( s, base_address );
    
    String addr = base_address + "/staffSymbol";
    if( getNumSubcomponents() && s->getOSCMessagePos(addr) == -1 )
    {
        auto sub_c = getSubcomponent(0);
        if( sub_c )
        {
            messages_added += ((BaseComponent*)sub_c)->addSymbolMessages( s, addr );
        }
        else
        {
            cout << "no subcomponent found" << endl;
        }
    }
    
    return messages_added;
}

void StaffComponent::mouseDown( const MouseEvent& event )
{
    BaseComponent::mouseDown(event);
    
    if( is_selected )
    {
        auto page = getPageComponent();

        for( BaseComponent *c : symbols_on_staff )
        {
            page->addToSelection( c );
        }
    }
}


void StaffComponent::paint ( Graphics& g )
{
    BaseComponent::paint( g );
 
    if( draw_timepoints )
    {
        auto timepoints = getSymbolistHandler()->getTimePointArray();
        
        float start_t = getScoreSymbolPointer()->getTime();
        float end_t = start_t + getScoreSymbolPointer()->getDuration();
        
        for( auto t : (*timepoints) )
        {
            if( t->time >= start_t && t->time <= end_t )
                g.fillEllipse( (t->time - start_t) * 100.0f, getHeight() / 2, 2, 2);
        }
    }
}