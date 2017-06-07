//
//  GroupComponent.cpp
//  symbolist
//
//  Created by Jean Bresson on 04/06/2017.
//
//

#include "SymbolGroupComponent.h"



void SymbolGroupComponent::paint ( Graphics& g )
{
    g.setColour( current_color );
    const Rectangle<int> b = ((BaseComponent*) this)->getLocalBounds();
    const float dashLength[2] = {3.0 , 6.0};
    int ndashLengths = 2;
    g.drawDashedLine(Line<float>( b.getX(), b.getY(), b.getX() + b.getWidth(), b.getY() ), dashLength , ndashLengths );
    g.drawDashedLine(Line<float>( b.getX() + b.getWidth(), b.getY(), b.getX() + b.getWidth(), b.getY() + b. getHeight() ), dashLength , ndashLengths );
    g.drawDashedLine(Line<float>( b.getX() + b.getWidth() , b.getY() + b.getHeight() , b.getX() , b.getY() + b. getHeight() ), dashLength , ndashLengths );
    g.drawDashedLine(Line<float>( b.getX() , b.getY() + b.getHeight() , b.getX() , b.getY()), dashLength , ndashLengths );
}




int SymbolGroupComponent::addSymbolMessages( const String &base_address )
{
    int messages_added = BaseComponent::addSymbolMessages(base_address);
    
    for (int i = 0; i < getNumSubcomponents(); i++)
    {
        String base = String(base_address) += String("/subsymbol/") += String(i) ;
        messages_added += getSubcomponent(i)->addSymbolMessages( base );
    }
    
    return messages_added;
}
