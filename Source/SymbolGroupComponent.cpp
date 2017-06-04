//
//  GroupComponent.cpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#include "SymbolGroupComponent.h"

/******************
 * Management of sucomponents
 *****************/

void SymbolGroupComponent::addSubcomponent( BaseComponent *c )
{
    subcomponents.emplace_back( c ) ;
    ScoreComponent::addAndMakeVisible( c );
}

size_t SymbolGroupComponent::getNumSubcomponents()
{
    return subcomponents.size() ;
}

BaseComponent* SymbolGroupComponent::getSubcomponent( int i )
{
    return subcomponents.at(i) ;
}



int SymbolGroupComponent::addSymbolMessages( const String &base_address )
{
    int messages_added = BaseComponent::addSymbolMessages(base_address);
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        String base = String(base_address) += String("/sub_") += String(i) ;
        messages_added += getSubcomponent(i)->addSymbolMessages( base );
    }
    
    return messages_added;
}
