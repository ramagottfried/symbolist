#ifndef View_hpp
#define View_hpp

#include <stdio.h>
#include "Observer.hpp"
#include "Observable.hpp"
#include "Controller.hpp"

template <class ObservableClass, class ControllerClass>
class View : public virtual Observer {

    ScopedPointer<ObservableClass> model;
    ScopedPointer<ControllerClass> controller;
    
public:
    /************************************************
     *                 CONSTRUCTORS                 *
     ************************************************/
    
    /**
     * View's empty constructor.
     */
    inline View() {
        // Asserting that model and controller parameters derive from the appropriated types
        static_assert(std::is_base_of<Observable, ObservableClass>::value, "type parameter of this class must derive from Observable");
        static_assert(std::is_base_of<Observer, ControllerClass>::value, "type parameter of this class must derive from Observer");
    }
    
    /**
     * View's destructor method.
     */
    virtual ~View() {};
    
    /*******************************************************
     *                 GETTERS AND SETTERS                 *
     *******************************************************/
    inline virtual ScopedPointer<ObservableClass> getModel() { return model; }
    inline virtual void setModel(ScopedPointer<ObservableClass> model) { this->model = model; }
    
    inline virtual ScopedPointer<ControllerClass> getController() { return controller; }
    inline virtual void setController(ScopedPointer<ControllerClass> controller) { this->controller = controller; }
    
};

#endif /* View_hpp */
