#ifndef Command_hpp
#define Command_hpp

#include <stdio.h>

/**
 * Describes an abstract class to execute an operation.
 */
class Command
{

public:

	inline virtual ~Command() {}
	virtual void execute() = 0;
	
};


#endif /* Command_hpp */
