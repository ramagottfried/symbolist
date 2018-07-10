#include "CreateSymbolCommand.hpp"

CreateSymbolCommand::CreateSymbolCommand(ScoreComponent* client, SymbolistHandler* commandReceiver, MouseEvent* triggeringEvent, bool symbolIsTopLevel)
{
	this->client = client;
	this->command_receiver = commandReceiver;
	this->triggering_event = triggeringEvent;
	this->symbol_is_top_level = symbolIsTopLevel;
	
}

void CreateSymbolCommand::execute()
{
	/* Creates a new symbol with the same settings as the selected
	 * symbol template in the palette.
	 * Template symbols all have a default type of "path" and bounds of 0,0,30,30
     * the generic symbol has the same OSC data as the BaseComponent.
	 */
	Symbol* newSymbol = command_receiver->createSymbolFromTemplate();
	
	// Sets default position before creating the graphic component.
	newSymbol->setPosition(triggering_event->position);
	
	// Creates a new component of the current selected symbol type.
	BaseComponent* newComponent = command_receiver->makeComponentFromSymbol(newSymbol, symbol_is_top_level);
	
	// Adds component in the view.
	client->addSubcomponent(newComponent);
}
