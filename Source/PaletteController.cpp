#include "PaletteController.hpp"
#include "PaletteComponent.h"

PaletteController::PaletteController(shared_ptr<SymbolistModel> model, shared_ptr<PaletteComponent> view)
{
    setModel(model);
    setView(view);
}
