#ifndef PageController_hpp
#define PageController_hpp

#include <stdio.h>
#include "Controller.hpp"
#include "SymbolistModel.hpp"
#include "symbolist-utils.hpp"

class PageComponent;
class BaseComponent;
class StaffComponent;
class SymbolistComponent;
class SymbolGroupComponent;

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
     * Wrapper method around the SymbolistHandler::makeComponentFromSymbol
     * method.
     *
     * @throws logic_error If this PageController has no parent controller,
     *                     or if it is not a SymbolistHandler instance.
     *
     * @see    SymbolistHandler#makeComponentFromSymbol(Symbol*, bool)
     */
    BaseComponent* makeComponentFromSymbol(Symbol* s, bool attachTheSymbol);
	
	/**
	 * Wrapper method around the SymbolistHandler::createIdFromName method.
	 *
	 * @throws logic_error If this PageController has no parent controller,
     *                     or if it is not a SymbolistHandler instance.
     *
	 * @see SymbolistHandler#createIdFromName(string&) SymbolistHandler::createIdFromName
	 *
	 */
	string createIdFromName(string& name);
	
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
     * Imports the symbols from bundle into score
     *
     * @param bundle a serialized OSC bundle containing
     *                     one or many symbols as OSC bundles.
     *
     * @see               SymbolistModel#importSymbols(const OdotBundle_s&)
     *
     * @see               Score#importSymbols(const OdotBundle_s&)
     */
    void importSymbols( const OdotBundle_s& bundle );
	
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
	 * Puts the symbols attached to the selected components
	 * in the score view into the clipboard.
	 *
	 * Works only when the selected components are not nested
	 * into a symbol.
	 */
	void copySelectedToClipBoard();

	/**
	 * Creates new components in the score view according
	 * to the clipboard's content.
	 */
	void newFromClipBoard();

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
	
	/**
	 * Wrapper method around the SymbolistHandler::removeSymbolFromScore()
	 * method.
	 * Removes the component's attached symbol from the score.
	 *
	 * @param component the graphic component referencing the
	 * 					symbol that will be removed from the
	 *					score.
	 *
	 * @throws logic_error If this PageController has no parent controller,
     *                     or if it is not a SymbolistHandler instance.
	 */
	void removeAttachedSymbolFromScore(BaseComponent* component);
	
	/**
	 * Creates a symbol group from the symbols attached
	 * to the selected components passed in parameter.
	 * The created symbol group is then added to the score.
	 *
	 * @param selectedComponents the array of SymbolistComponent from which
	 *                           the attached symbols will be retrieved to
	 *                           create a new group symbol in the score.
	 *
	 * @return                   a pointer to the newly created group symbol
	 *                           in the score.
	 *
	 * @throws logic_error       If this PageController has no parent controller,
     *                           or if it is not a SymbolistHandler instance.
	 */
	Symbol* createTopLevelSymbolGroup(Array<SymbolistComponent* > selectedComponents);
	
	/**
	 * Creates a symbol group from the symbols attached to the selected components.
	 * The symbol group is not added to the score because it is nested
	 * into a higher level symbol group.
	 *
	 * @param selectedComponents the array of SymbolistComponent from which
	 *                           the attached symbols will be retrieved to
	 *                           create a new group symbol.
	 *
	 * @param container          the SymbolGroupComponent containing the
	 *                           selected components.
	 *
	 * @return                   a copy of the symbol group created from
	 *                           the selected components.
	 */
	Symbol createNestedSymbolGroup(Array<SymbolistComponent* > selectedComponents, SymbolGroupComponent* container);
	
    /* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}

private:
	OwnedArray<Symbol> clipboard;

};

#endif /* PageController_hpp */
