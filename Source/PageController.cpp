#include "PageController.hpp"
#include "PageComponent.h"
#include "StaffComponent.hpp"
#include "symbolist-utils.hpp"

PageController::PageController()
{
    
}

PageController::PageController(SymbolistModel* model, PageComponent* view)
{
    setModel(model);
    setView(view);
}

BaseComponent* PageController::makeComponentFromSymbol(Symbol* s, bool attachTheSymbol)
{
    SymbolistHandler* parentController = dynamic_cast<SymbolistHandler*>(getParentController());
    if (parentController != NULL)
    {
        BaseComponent* newComponent = parentController->makeComponentFromSymbol(s, attachTheSymbol);
		
        return newComponent;
    }
    else throw logic_error("PageController has no parent controller.");
}

string PageController::createIdFromName(string& name)
{
	SymbolistHandler* parentController = dynamic_cast<SymbolistHandler*>(getParentController());
    if (parentController != NULL)
		return parentController->createIdFromName(name);
    else throw logic_error("PageController has no parent controller.");
}

int PageController::getCountOfSymbols()
{
    return static_cast<int>(getModel()->getScore()->getSize());;
}

Symbol* PageController::getSymbolAtIndex(int n)
{
    return getModel()->getScore()->getSymbol(n);;
}

Symbol* PageController::setOneSymbol(const OdotBundle_s& bundle)
{
    Symbol symbol = Symbol( bundle );
    return getModel()->addSymbolToScore(&symbol);
}

void PageController::importSymbols( const OdotBundle_s& bundle )
{
    getModel()->importSymbols( bundle );
}

void PageController::addComponentsFromScore()
{
    // Creates components from score symbols.
    Score* score = getModel()->getScore();
    DEBUG_FULL("ADDING " << score->getSize() << " SYMBOLS" << endl)
    
    for (int i = 0; i < score->getSize(); i++)
    {
        try
        {
            BaseComponent* newComponent = makeComponentFromSymbol(score->getSymbol(i), true);
            getView()->addSubcomponent(newComponent);
        }
        catch(length_error& error)
        {
            cout << error.what() << endl;
        }
        catch(logic_error& error)
        {
            cout << error.what() << endl;
        }
    }
}

void PageController::clearAllSubcomponents()
{
	getView()->clearAllSubcomponents();
}

void PageController::copySelectedToClipBoard()
{
    clipboard.clear();
    Symbol* symbol;    

    for ( auto c : getView()->getSelectedItems() )
    {
        auto bc = dynamic_cast<BaseComponent*>(c);
        if ( bc ) // skip if non-basecomponent type
		{
	    	symbol = new Symbol(*(bc)->getScoreSymbol());
	    
			/* The stored symbol should not have the same
			 * id as the base component from which it is copied.
			 */
			symbol->resetAllIds();
			clipboard.add(symbol);
		}
    }
}

void PageController::newFromClipBoard()
{
	getView()->unselectAllComponents();
	
    for( auto s : clipboard )
    {
        Symbol* newSymbol = getModel()->getScore()->addDuplicateSymbol(s);
        BaseComponent* newComponent = makeComponentFromSymbol(newSymbol, true);
		
        if ( newComponent != NULL)
        {
            getView()->addSubcomponent( newComponent );
            newComponent->toFront(true);
            getView()->addToSelection( newComponent );
        }

    }
}

StaffComponent* PageController::getStaveAtTime(float time)
{
    Symbol* staveSymbol = getModel()->getScore()->getStaveAtTime(time);
    if (staveSymbol != NULL)
    {
        Component *c = getView()->findChildWithID( staveSymbol->getID().c_str() );
        if (c)
        {
            StaffComponent *staff = dynamic_cast<StaffComponent*>(c);
            if (staff != NULL)
                return staff;
            
        }
    }
    
    return NULL;
}

OdotBundle_s PageController::getSymbolsAtTime(float time)
{
    return getModel()->getScore()->getSymbolsAtTime(time);
}

OdotBundle_s PageController::getDurationBundle()
{
    return getModel()->getScore()->getDurationBundle();
}

OdotBundle_s PageController::getScoreBundle()
{
    return getModel()->getScore()->getScoreBundle_s();
}

void PageController::removeAllSymbols()
{
    getModel()->removeAllSymbolsFromScore();
}

void PageController::removeAttachedSymbolFromScore(BaseComponent* component)
{
	SymbolistHandler* parentController = dynamic_cast<SymbolistHandler* >(
											getParentController()
										 );
	if (parentController == NULL)
		throw logic_error("PageController has no parent controller.");

	parentController->removeSymbolFromScore(component);
	
}

Symbol *PageController::createTopLevelSymbolGroup(Array<ScoreComponent *> selectedComponents)
{
		// Gets the position and bounds of the group.
        int minx = getView()->getWidth(), maxx = 0, miny = getView()->getHeight(), maxy = 0;
        for( auto it = selectedComponents.begin(); it != selectedComponents.end(); it++ )
        {
            Rectangle<int> compBounds = (*it)->getBounds();
            minx =  min( minx, compBounds.getX() );
            miny =  min( miny, compBounds.getY() );
            maxx =  max( maxx, compBounds.getRight() );
            maxy =  max( maxy, compBounds.getBottom() );
        }

		SymbolistHandler* parentController = dynamic_cast<SymbolistHandler*>(getParentController());
	
		if (parentController == NULL)
			throw logic_error("PageController has no parent controller.");
	
		// Creates a new symbol in the score and sets its properties as a symbol group.
        Symbol* symbolGroup = parentController->createSymbol();
        symbolGroup->setTypeXYWH("group", minx, miny, maxx-minx, maxy-miny);
	
        int count = 0;

        for (SymbolistComponent *c : selectedComponents)
        {
            auto selectedComponent = dynamic_cast<BaseComponent*>(c);
			
            // Checks downcast result.
            if (selectedComponent != NULL)
            {
                auto associatedSymbol = selectedComponent->getScoreSymbol();
                if (associatedSymbol->size() > 0)
                {
                	// copies bundles from subcomponent symbols and join into new group symbol
                    Point<float> symbolPos = selectedComponent->computeSymbolPosition( selectedComponent->getX() - minx,
																					   selectedComponent->getY() - miny,
                                                                                       selectedComponent->getWidth(),
                                                                                       selectedComponent->getHeight() );
					
                    // Copies bundles from subcomponent symbols and join into new group symbol.
                    associatedSymbol->addMessage("/x", symbolPos.getX());
                    associatedSymbol->addMessage("/y", symbolPos.getY());
					
                    symbolGroup->addMessage("/subsymbol/" + to_string(count++), *associatedSymbol);
                }
            }
        }
	
        return symbolGroup;
}

Symbol PageController::createNestedSymbolGroup(Array<ScoreComponent* > selectedComponents, SymbolGroupComponent* container)
{
	// Gets the position and bounds of the group.
	int minx = container->getWidth(), maxx = 0, miny = container->getHeight(), maxy = 0;
	for( auto it = selectedComponents.begin(); it != selectedComponents.end(); it++ )
	{
		Rectangle<int> compBounds = (*it)->getBounds();
		minx =  min( minx, compBounds.getX() );
		miny =  min( miny, compBounds.getY() );
		maxx =  max( maxx, compBounds.getRight() );
		maxy =  max( maxy, compBounds.getBottom() );
	}
	
	// Creating a temporary symbol, because it will not integrate the score.
	Symbol symbolGroup = Symbol();
	symbolGroup.setTypeXYWH("group", minx, miny, maxx-minx, maxy-miny);

	int count = 0;

	/* Adds a "/subsymbol" message in the group symbol bundle
	 * for each selected component.
	 */
	for (SymbolistComponent *c : selectedComponents)
	{
		auto selectedComponent = dynamic_cast<BaseComponent* >(c);
		
		// Checks downcast result.
		if (selectedComponent != NULL)
		{
			/* Creates a symbol for the selectedComponent.
			 * Adding messages to the bundle is easier then.
			 */
			Symbol subSymbol = selectedComponent->exportSymbol();
			
			if (subSymbol.size() > 0)
			{
				Point<float> symbolPos = selectedComponent->computeSymbolPosition( selectedComponent->getX() - minx,
																					   selectedComponent->getY() - miny,
                                                                                       selectedComponent->getWidth(),
                                                                                       selectedComponent->getHeight() );
				
				// Copies bundle from subcomponent symbol and join into new group symbol
				subSymbol.addMessage("/x", symbolPos.getX());
				subSymbol.addMessage("/y", symbolPos.getY());
				
				symbolGroup.addMessage( "/subsymbol/" + to_string(count++), subSymbol );
				
			}
			subSymbol.print();
			
		}
	}
	
	return symbolGroup;
	
}

Symbol* PageController::createStaff(BaseComponent* staffReferenceComponent)
{
	SymbolistHandler* parentController = dynamic_cast<SymbolistHandler* >(
											getParentController()
										 );
	if (parentController == NULL)
		throw logic_error("PageController has no parent controller.");
	
	// Calls the parent controller to create new symbol in score.
	Symbol* staffSymbol = parentController->createSymbol();
	
	staffSymbol->setTypeXYWH("staff",
							 staffReferenceComponent->getX(),
							 staffReferenceComponent->getY(),
							 staffReferenceComponent->getWidth(),
							 staffReferenceComponent->getHeight() );

	return staffSymbol;
	
}

