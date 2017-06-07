

#include "ScoreComponent.h"

/*****************************
 * Management of sucomponents
 * Add/remove operations apply on views only
 *****************************/

size_t ScoreComponent::getNumSubcomponents()
{
    return subcomponents.size() ;
}

BaseComponent* ScoreComponent::getSubcomponent( int i )
{
    return subcomponents.at(i) ;
}

void ScoreComponent::addSubcomponent( BaseComponent *c )
{
    subcomponents.emplace_back( c ) ;
    
    c->setComponentID(String(String(c->getSymbolType()) += String("_") += String(subcomponents.size())));

    ScoreComponent::addAndMakeVisible( c );
    c->addMouseListener(this, false);
}

void ScoreComponent::removeSubcomponent( BaseComponent *c , bool delete_it)
{
    removeChildComponent(c);
    subcomponents.erase ( std::remove(subcomponents.begin(),subcomponents.end(), c) , subcomponents.end() );
    if ( delete_it ) delete c;
}

void ScoreComponent::clearAllSubcomponents()
{
    for ( int i = 0; i < subcomponents.size(); i++ )
    {
        removeChildComponent(subcomponents[i]);
        delete subcomponents[i];
    }
    subcomponents.clear();
}
