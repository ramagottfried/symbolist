#ifndef CreateSymbolCommand_hpp
#define CreateSymbolCommand_hpp

#include <stdio.h>
#include "Command.hpp"
#include "Symbol.h"
#include "BaseComponent.h"

/**
 * A command to create a new symbol and its representing graphic component.
 *
 * The implementation of the execute method encapsulates calls to
 * SymbolistHandler::createSymbolFromTemplate and SymbolistHandler::makeComponentFromSymbol methods.
 */
class CreateSymbolCommand : public Command
{
	
public:
	/**
	 * CreateSymbolCommand constructor.
	 *
	 * Parameters needed by the command receiver to perform its
	 * operations are stored as instance variables (triggeringEvent and symbolIsTopLevel).
	 *
	 * @param client           a reference to the client object, creator of this CreateSymbolCommand instance
	 *						   and owner of the Invoker object.
	 *
	 * @param commandReceiver  a reference to the object which will be performing the concrete operations
	 *						   to execute the command.
	 *
	 * @param triggeringEvent  the mouse event associated with the command.
	 *
	 * @param symbolIsTopLevel a boolean to indicate if the command receiver must create a top-level component.
	 *
	 */
	CreateSymbolCommand(ScoreComponent* client, SymbolistHandler* commandReceiver, MouseEvent* triggeringEvent, bool symbolIsTopLevel);
	
	/**
	 * Performs the command by invoking the command receiver.
	 *
	 * Encapsulates calls to the SymbolistHandler::createSymbolFromTemplate
	 * and SymbolistHandler::makeComponentFromSymbol methods.
	 *
	 * @see SymbolistHandler#createSymbolFromTemplate()
	 *		SymbolistHandler::createSymbolFromTemplate()
	 *
	 * @see SymbolistHandler#makeComponentFromSymbol(Symbol*, boolean)
	 *		SymbolistHandler::(Symbol*, boolean)
	 *
	 */
	void execute() override;
	
private:
	/**
	 * The SymbolistHandler object in charge of performing
	 * the concrete operations to execute the command.
	 */
	SymbolistHandler* command_receiver;
	
	/**
	 * The ScoreComponent object, creator of this CreateSymbolCommand instance
	 *  and owner of the Invoker object (which invokes the execute method).
	 */
	ScoreComponent*   client;
	
	/** The MouseEvent object triggering at the origin of the command invocation. */
	MouseEvent*       triggering_event;
	
	/** A boolean telling the command receiver to create a top-level component, or not. */
	bool              symbol_is_top_level;
	
	/******************************
	 *            STATE           *
	 ******************************/
	
	/** The symbol created by SymbolistHandler::createSymbolFromTemplate method. */
	Symbol*           created_symbol = NULL;
	
	/** The graphic component created by SymbolistHandler::makeComponentFromSymbol method. */
	BaseComponent*    created_component = NULL;
	
};

#endif /* CreateSymbolCommand_hpp */
