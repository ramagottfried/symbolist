//
//  TimeDisplayController.hpp
//  symbolist
//
//  Created by Vincent Iampietro on 24/04/2018.
//

#ifndef TimeDisplayController_hpp
#define TimeDisplayController_hpp

#include <stdio.h>
#include "SymbolistModel.hpp"
#include "Controller.hpp"

class TimeDisplayComponent;

class TimeDisplayController : public virtual Controller<SymbolistModel, TimeDisplayComponent > {
	
public:

	/* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
};

#endif /* TimeDisplayController_hpp */
