#include "PaletteController.hpp"
#include "PaletteComponent.h"
#include "BaseComponent.h"

PaletteController::PaletteController()
{
    
}

PaletteController::PaletteController(SymbolistModel* model, PaletteComponent* view)
{
    setModel(model);
    setView(view);
}

BaseComponent* PaletteController::makeComponentFromSymbol(Symbol* s, bool attach_the_symbol)
{
    SymbolistHandler* parentController = dynamic_cast<SymbolistHandler*>(getParentController());
    if (parentController != NULL)
        return parentController->makeComponentFromSymbol(s, attach_the_symbol);
    else throw logic_error("PaletteController has no parent controller.");
}

void PaletteController::setSelectedItem(int indexOfSelectedItem)
{
    getModel()->getPalette()->setSelectedItem(indexOfSelectedItem);
}

int PaletteController::getNumPaletteSymbols()
{
    return static_cast<int>(getModel()->getPalette()->getPaletteNumUserItems());
}

Symbol* PaletteController::getPaletteSymbol(int n)
{
    return getModel()->getPalette()->getPaletteUserItem(n);;
}

void PaletteController::setOnePaletteSymbol(const OdotBundle_s& bundle)
{
    getModel()->addSymbolToPalette(Symbol(bundle));
}

void PaletteController::setPaletteSymbols(const OdotBundle_s& bundle_array)
{
    const OdotBundle bndl(bundle_array);
    
    for (auto msg : bndl.getMessageArray() )
        if( msg[0].getType() == OdotAtom::O_ATOM_BUNDLE && msg.getAddress().find("/symbol") == 0 )
        {
            Symbol s = Symbol(msg.getBundle().get_o_ptr());
            getModel()->addSymbolToPalette(s);
        }
    
    // If view is set (means the main window is visible).
    if (getView() != NULL)
        getView()->buildFromPalette();
    
}

Symbol* PaletteController::getSelectedSymbolInPalette()
{
    Palette* palette = getModel()->getPalette();
    int num_def_symbols = palette->getPaletteNumDefaultItems();
    int sel = palette->getSelectedItem();
    
    if (sel < num_def_symbols)
        return palette->getPaletteDefaultItem(sel);
    else
        return palette->getPaletteUserItem(sel - num_def_symbols);

}

void PaletteController::addSymbolFromComponent(BaseComponent* component)
{
    Symbol newPaletteSymbol = Symbol();
    component->addSymbolMessages(&newPaletteSymbol);
	
	/* Resets the symbol id and the ids of its nested symbols
	 * before adding it to the palette.
	 * Palette symbols have no id.
	 */
    newPaletteSymbol.resetAllIds();
	
    getModel()->addSymbolToPalette(newPaletteSymbol);
}
