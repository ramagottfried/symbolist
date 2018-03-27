#ifndef PaletteController_hpp
#define PaletteController_hpp

#include <stdio.h>
#include "Controller.hpp"
#include "SymbolistModel.hpp"

class PaletteComponent;

class PaletteController : public virtual Controller<SymbolistModel, PaletteComponent> {
    
public:
    PaletteController(shared_ptr<SymbolistModel> model, shared_ptr<PaletteComponent> view);
    
    virtual inline ~PaletteController() {};
    
    /* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
    
};
#endif /* PaletteController_hpp */
