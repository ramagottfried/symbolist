//
//  MouseModeController.hpp
//  symbolist
//
//  Created by Vincent Iampietro on 23/04/2018.
//

#ifndef MouseModeController_hpp
#define MouseModeController_hpp

#include <stdio.h>
#include "SymbolistModel.hpp"
#include "Controller.hpp"

class MouseModeComponent;

class MouseModeController : public virtual Controller<SymbolistModel, MouseModeComponent > {

public:

	/* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}

};

#endif /* MouseModeController_hpp */
