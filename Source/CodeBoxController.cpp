#include "CodeBoxController.hpp"

void CodeBoxController::updateExprInSymbol(Symbol* symbol, string newExpression)
{
		symbol->addMessage("/expr", newExpression);
}
