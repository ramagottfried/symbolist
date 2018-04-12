#include "PageController.hpp"
#include "PageComponent.h"
#include "StaffComponent.hpp"

PageController::PageController()
{
    
}

PageController::PageController(SymbolistModel* model, PageComponent* view)
{
    setModel(model);
    setView(view);
}

BaseComponent* PageController::makeComponentFromSymbol(Symbol* s, bool attach_the_symbol)
{
    SymbolistHandler* parentController = dynamic_cast<SymbolistHandler*>(getParentController());
    if (parentController != NULL)
    {
        BaseComponent* newComponent = parentController->makeComponentFromSymbol(s, attach_the_symbol);
        getView()->addSubcomponent(newComponent);
        
        return newComponent;
    }
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
    Symbol symbol = Symbol(bundle);
    return getModel()->addSymbolToScore(&symbol);
}

void PageController::importScoreFromOSC(const OdotBundle_s& bundleArray)
{
    getModel()->importScoreFromOSC(bundleArray);
}

void PageController::addComponentsFromScore()
{
    // Recreates and adds components from score symbols.
    Score* score = getModel()->getScore();
    cout << __func__ << " ADDING " << score->getSize() << " SYMBOLS" << endl;
    
    for (int i = 0; i < score->getSize(); i++)
        try
        {
            makeComponentFromSymbol(score->getSymbol(i), false);
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

void PageController::clearAllSubcomponents()
{
    getView()->clearAllSubcomponents();
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

