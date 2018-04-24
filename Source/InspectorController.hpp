//
//  InspectorController.hpp
//  symbolist
//
//  Created by Vincent Iampietro on 24/04/2018.
//

#ifndef InspectorController_hpp
#define InspectorController_hpp

#include <stdio.h>
#include "SymbolistModel.hpp"
#include "Controller.hpp"

class InspectorComponent;
class BaseComponent;

class InspectorController : public virtual Controller<SymbolistModel, InspectorComponent > {
	
public:
	void addToInspector(BaseComponent* c);
	void clearInspector();
	
	/**
	 * Wrapper method around SymbolistHandler::updateSymbolFromComponent
	 * method.
	 *
	 * @throws logic_error If this InspectorController instance has no
	 *					   parent controller, or it's parent controller
	 *					   is not a SymbolistHandler instance.
	 *
	 * @see SymbolistHandler#updateSymbolFromComponent(BaseComponent*)
	 *		SymbolistHandler::updateSymbolFromComponent
	 */
	void updateSymbolFromComponent(BaseComponent* component);
	
	/* Overrides the update method inherited from the Observer class. */
    virtual inline void update() override {}
	
};

#endif /* InspectorController_hpp */
