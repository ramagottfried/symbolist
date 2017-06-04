

#include "ScoreComponent.h"


/**************************/
/* Add/remove operations on View only */
/**************************/

/* modifies the view (not the score) */
void ScoreComponent::addChildToScoreComponent( BaseComponent *c )
{
    addAndMakeVisible ( c );
    c->addMouseListener(this, false);
    subcomponents.emplace_back ( c );
    
    // the default Component ID is type_posInScore
    c->setComponentID(String(String(c->getSymbolType()) += String("_") += String(subcomponents.size())));
    
    // selected_items.addToSelection( c );
    // selected_items.addChangeListener(c);
}


/* modifies the view (not the score) */
void ScoreComponent::removeChildFromScoreComponent( BaseComponent *c , bool delete_it)
{
    removeChildComponent(c);
    subcomponents.erase ( std::remove(subcomponents.begin(),subcomponents.end(), c) ,
                       subcomponents.end() );
    if (delete_it ) delete c;
}

/* modifies the view (not the score) */
void ScoreComponent::clearAllSymbolComponents()
{
    for ( int i = 0; i < subcomponents.size(); i++ )
    {
        removeChildComponent(subcomponents[i]);
        delete subcomponents[i];
    }
    subcomponents.clear();
}
