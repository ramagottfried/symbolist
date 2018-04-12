#ifndef PageController_hpp
#define PageController_hpp

#include <stdio.h>
#include "Controller.hpp"
#include "SymbolistModel.hpp"

class PageComponent;
class BaseComponent;
class StaffComponent;

/**
 * Represents the controller class for the PageComponent of the application.
 *
 * Whenever the PageComponent (the graphic view of the score) wants to
 * modify the model, in response to user actions, the PageController is solici-
 * tated.
 */
class PageController : public virtual Controller<SymbolistModel, PageComponent> {
    
public:
    
    /**
     * PageController's default constructor.
     */
    PageController();
    
    /**
     * PageController's cosntructor with model and view passed as arguments.
     */
    PageController(SymbolistModel* model, PageComponent* view);
    
    /**
     * Creates a new component from the symbol in parameter
     * and adds it to the view.
     *
     * Calls the makeComponentFromSymbol method of the parent controller,
     * which normally is a SymbolistHandler instance.
     *
     * @return A pointer to the newly created BaseComponent or <code>NULL</code>
     *         if the parent controller of this PageController instance is
     *         not of type SymbolistHandler.
     *
     * @see    SymbolistHandler#makeComponentFromSymbol(Symbol*, bool)
     */
    BaseComponent* makeComponentFromSymbol(Symbol* s, bool attach_the_symbol);
    
    /**
     * Gets the count of symbols in the score.
     *
     * This method is a wrapper around the
     * Score::getSize() method.
     *
     * @return the number of symbols currently in the score.
     */
    int getCountOfSymbols();
    
    /**
     * Gets the symbol at index n in the score.
     *
     * This method is a wrapper around the
     * Score::getSymbol(int n) method.
     *
     * @param n             the index of the symbol to retrieve.
     *
     * @throws length_error If n is out of bound according to the score size
     *
     * @return the pointer to the symbol at index n in the score.
     */
    Symbol* getSymbolAtIndex(int n);
    
    /**
     * Sets a new symbol in the score.
     *
     * @param bundle the serialized Odot bundle from which the new symbol
     *               will be created.
     *
     * @return       a pointer to the newly created symbol.
     */
    Symbol* setOneSymbol(const OdotBundle_s& bundle);
    
    /**
     * Resets the score and creates new symbols from
     * the serialized OSC bundle array in parameter.
     *
     * @param bundleArray the serialized OSC bundle array containing
     *                     one or many symbols as OSC bundles.
     *
     * @see               SymbolistModel#importScoreFromOSC(const OdotBundle_s&)
     *
     * @see               Score#importScoreFromOSC(const OdotBundle_s&)
     */
    void importScoreFromOSC(const OdotBundle_s& bundleArray);
    
    /**
     * Creates a graphic component for each symbol in the score
     * and adds it to the view.
     */
    void addComponentsFromScore();
    
    /**
     * Erases all graphic components from the view.
     */
    void clearAllSubcomponents();
    
    /**
     * Gets the staff component (if exists) placed at the specified time
     * in the score.
     *
     * @param time the time value where to look for a staff component.
     *
     * @return     a pointer to the StaffComponent object found
     *             at the specified time or <code>NULL</code> if
     *             nothing no staff has been found.
     */
    StaffComponent* getStaveAtTime(float time);
    
    /**
     * Gets the symbols registered at the specified time,
     * embedded in a serialized Odot bundle.
     *
     * @param time the time value for which to retrieve the symbols.
     *
     * @return     a serialized Odot bundle embedding all symbols found
     *             at the specified time.
     *
     */
    OdotBundle_s getSymbolsAtTime(float time);
    
    /**
     * Gets a serialized Odot bundle embedding the value of
     * the score's time duration.
     *
     * @return a serialized Odot bundle embedding the message
     *         /time/duration which value corresponds to the
     *         score's time duration.
     */
    OdotBundle_s getDurationBundle();
    
    /**
     * Gets a serialized Odot bundle embedding all score's symbols.
     *
     * @return a serialized Odot bundle embedding all the symbols
     *         of the score, also represented as Odot bundles.
     */
    OdotBundle_s getScoreBundle();
    
    /**
     * Removes all symbols from the score.
     *
     * The time points and staves lists associated with the score
     * are also deleted.
     */
    void removeAllSymbols();
    
    /* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
};

#endif /* PageController_hpp */
