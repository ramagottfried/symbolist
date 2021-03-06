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

	/**
	 * Wrapper method around the SymbolistModel::updateExprInSymbol method.
	 *
	 * @see SymbolistModel#updateExprInSymbol(Symbol*, string)
	 *		SymbolistModel::updateExprInSymbol(Symbol*, string)
	 */
	void updateExprInSymbol(Symbol* symbol, string newExpression);
	
	/* Overrides the update method inherited from the Observer class. */
    inline void update() override { }

};

#endif /* CodeBoxController_hpp */
