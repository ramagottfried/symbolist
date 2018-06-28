#ifndef CodeBoxController_hpp
#define CodeBoxController_hpp

#include <stdio.h>
#include "Controller.hpp"
#include "SymbolistModel.hpp"
#include "BaseComponent.h"

class CodeBoxComponent;

class CodeBoxController : public virtual Controller<SymbolistModel, CodeBoxComponent>
{

public:
	void updateExprInSymbol(Symbol* symbol, string newExpression);
	
	/* Overrides the update method inherited from the Observer class. */
    inline void update() override { }

};

#endif /* CodeBoxController_hpp */
