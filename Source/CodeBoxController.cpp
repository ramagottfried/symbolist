#include "CodeBoxController.hpp"
#include "CodeBoxComponent.hpp"

void CodeBoxController::updateExprInSymbol(Symbol* symbol, string newExpression)
{
	if (getModel() != NULL)
		getModel()->updateExprInSymbol(symbol, newExpression);
	
}
