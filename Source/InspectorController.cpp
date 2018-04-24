#include "InspectorController.hpp"
#include "BaseComponent.h"
#include "InspectorComponent.h"

void InspectorController::addToInspector( BaseComponent *c )
{
    // Only selected and called if the main component is there...
    getView()->setInspectorObject(c);
}

void InspectorController::clearInspector()
{
    getView()->clearInspector();
}

void InspectorController::updateSymbolFromComponent(BaseComponent* component)
{
	SymbolistHandler* parentController = dynamic_cast<SymbolistHandler* >(getParentController());
	if (parentController != NULL)
		parentController->updateSymbolFromComponent(component);
	else throw logic_error("InspectorController has no parent controller");
}
