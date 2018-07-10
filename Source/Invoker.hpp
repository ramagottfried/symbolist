#ifndef Invoker_hpp
#define Invoker_hpp

#include <stdio.h>
#include <vector>
#include <queue>
#include "Command.hpp"
#include "SymbolistHandler.h"

using namespace std;

/**
 * Executes a command, and stores it into a command history.
 *
 * Commands from the history can be done and undone, in order to implement
 * the undo and redo actions.
 */
class Invoker
{

public:
	void execute(Command* command);
	
private:
	queue<unique_ptr<Command> > command_history;
	SymbolistHandler* 			command_receiver;
	
};

#endif /* Invoker_hpp */
