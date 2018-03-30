#include "PaletteController.hpp"
#include "PaletteComponent.h"

PaletteController::PaletteController()
{
    
}

PaletteController::PaletteController(SymbolistModel* model, PaletteComponent* view)
{
    setModel(model);
    setView(view);
}
