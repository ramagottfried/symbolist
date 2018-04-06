#pragma once

#include <stdio.h>
#include "Controller.hpp"
#include "SymbolistModel.hpp"
#include "JuceHeader.h"

class PaletteComponent;
class BaseComponent;

/**
 * The controller class for all palette-related user interactions.
 * 
 */
class PaletteController : public virtual Controller<SymbolistModel, PaletteComponent> {
    
public:
    /**
     * PaletteController's default constructor.
     */
    PaletteController();
    
    /**
     * PaletteController's with model and view passed as arguments.
     */
    PaletteController(SymbolistModel* model, PaletteComponent* view);
    
    /**
     * PaletteController's default destructor.
     */
    virtual inline ~PaletteController() override
    {
    };
    
    /**
     * Calls the makeComponentFromSymbol method of the parent controller,
     * which normally is a SymbolistHandler instance.
     *
     * @return A pointer to the newly created BaseComponent or <code>NULL</code>
     *         if the parent controller of this PaletteController instance is
     *         not of type SymbolistHandler.
     */
    BaseComponent* makeComponentFromSymbol(Symbol* s, bool attach_the_symbol);
    
    /**
     * Sets the selected_item number of the model's palette
     * instance to indexOfSelectedItem.
     *
     * @param indexOfSelectedItem the index of the selected item in the
     *                            palette.
     */
    void setSelectedItem(int indexOfSelectedItem);
    
    /**
     * Gets the count of user-defined symbols in the palette.
     *
     * @return the count of user-defined symbols in the palette.
     */
    int getNumPaletteSymbols();
    
    /**
     * Gets the palette symbol at index n.
     *
     * @return a pointer to the symbol at index n
     *         in the palette.
     */
    Symbol* getPaletteSymbol(int n);
    
    /**
     * Adds a new symbol to the palette's user items.
     * The new symbol is a copy of the
     * serialized bundle passed as argument.
     *
     * @param bundle the odot bundle from which the
     *               new symbol will be created.
     */
    void setOnePaletteSymbol(const OdotBundle_s& bundle);
    
    /**
     * Adds one or more symbols to the palette's user items.
     * Added symbols are copies of the bundles in the
     * serialized bundle array.
     *
     * @param bundle_array a serialized bundle array containing
     *                     at least one bundle.
     */
    void setPaletteSymbols(const OdotBundle_s& bundle_array);
    
    /**
     * Returns the currently selected item in the palette.
     * It can be either user-defined or a default item.
     *
     * @return a pointer to the currently selected symbol
     *         in the palette.
     */
    Symbol* getSelectedSymbolInPalette();
    
    /**
     * Adds the symbol associated with the component in parameter
     * as a user-defined palette item.
     *
     * @param component the component owning the symbol
     *                  that will be added to user-defined
     *                  palette's items.
     */
    void addSymbolFromComponent(BaseComponent* component);
    
    /* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (PaletteController)
    
};

