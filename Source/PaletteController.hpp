#pragma once

#include <stdio.h>
#include "Controller.hpp"
#include "SymbolistModel.hpp"

class PaletteComponent;

class PaletteController : public virtual Controller<SymbolistModel, PaletteComponent> {
    
public:
    PaletteController();
    PaletteController(SymbolistModel* model, PaletteComponent* view);
    
    virtual inline ~PaletteController() override
    {
        Controller::~Controller();
    };
    
    /* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
    
};

