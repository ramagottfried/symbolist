#ifndef Controller_hpp
#define Controller_hpp

#include <stdio.h>
#include "Observable.hpp"
#include "View.hpp"
#include "Observer.hpp"

/**
 * Describes an abstract templated controller class.
 * This Controller class's behavior must be redefined
 * through the definition of concrete controllers.
 * Especially the concrete controllers must redefine
 * the update method inherited by the Observer class.
 *
 */
template <class ObservableClass, class ViewClass>
class Controller : public virtual Observer {
    
private:
    ObservableClass model;
    ViewClass view;
    
public:
    /**
     * Controller's empty constructor.
     */
    Controller();
    
    /**
     * Controller's constructor specifying its model, and
     * view as parameters.
     */
    Controller(ObservableClass model, ViewClass view) {
        
        // Asserting that model and view parameters derive from the appropriated types
        static_assert(std::is_base_of<Observable, ObservableClass>::value, "type parameter of this class must derive from Observable");
        static_assert(std::is_base_of<Observer, ViewClass>::value, "type parameter of this class must derive from Observer");
        
        this->model = model;
        this->view = view;
        
    };
    
    /**
     * Controller's destructor.
     */
    virtual ~Controller() {};
};

#endif /* Controller_hpp */
