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

void InspectorController::addMessageToInspectedSymbol(Symbol* theInspectedSymbol, String messageAddress, String messageType)
{
	if (theInspectedSymbol == NULL)
		return;
	
	if (!messageAddress.startsWith("/"))
		return;
	
	// Adds arbitrary values depending on the chosen type of message.
  	if (messageType == "int")
    	theInspectedSymbol->addMessage(messageAddress.toStdString(), 0);
	else if (messageType == "float")
		theInspectedSymbol->addMessage(messageAddress.toStdString(), 0.0f);
	else theInspectedSymbol->addMessage(messageAddress.toStdString(), "text");
}
