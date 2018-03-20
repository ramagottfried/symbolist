//
//  View.hpp
//  symbolist
//
//  Created by Vincent Iampietro on 20/03/2018.
//

#ifndef View_hpp
#define View_hpp

#include <stdio.h>
#include "Observer.hpp"
#include "Observable.hpp"
#include "Controller.hpp"

template <class ObservableClass, class ControllerClass>
class View : public virtual Observer {

private:
    ObservableClass model;
    ControllerClass controller;
    
public:
    /**
     * View's empty constructor.
     */
    View() {}
    
    /**
     * View's constructor passing its model and controller
     * as arguments.
     *
     * @param model      the model instance which is presented by this
     *                   View instance.
     *
     * @param controller the controller instance which is linked with this
     *                   View instance.
     *
     */
    View(ObservableClass model, ControllerClass controller) {
        
        // Asserting that model and controller parameters derive from the appropriated types
        static_assert(std::is_base_of<Observable, ObservableClass>::value, "type parameter of this class must derive from Observable");
        static_assert(std::is_base_of<Observer, ControllerClass>::value, "type parameter of this class must derive from Observer");
        
        this->model = model;
        this->controller = controller;
    }
    
    /**
     * View's destructor method.
     */
    virtual ~View() {};
    
};

#endif /* View_hpp */
