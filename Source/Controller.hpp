#ifndef Controller_hpp
#define Controller_hpp

#include <stdio.h>
#include "Observable.hpp"
#include "Observer.hpp"

using namespace std;

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
    
    shared_ptr<ObservableClass> model;
    shared_ptr<ViewClass> view;
    
public:
    /************************************************
     *                 CONSTRUCTORS                 *
     ************************************************/
    
    /**
     * Controller's empty constructor.
     */
    inline Controller() {
        // Asserting that model and view parameters derive from the appropriated types
        static_assert(std::is_base_of<Observable, ObservableClass>::value, "type parameter of this class must derive from Observable");
        static_assert(std::is_base_of<Observer, ViewClass>::value, "type parameter of this class must derive from Observer");
    };
    
    /**
     * Controller's destructor.
     */
    virtual ~Controller() {};
    
    /*******************************************************
     *                 GETTERS AND SETTERS                 *
     *******************************************************/
    inline virtual shared_ptr<ObservableClass> getModel() { return model; }
    inline virtual void setModel(shared_ptr<ObservableClass> model) { this->model = model; }
    
    inline virtual shared_ptr<ViewClass> getView() { return view; }
    inline virtual void setView(shared_ptr<ViewClass> view) { this->view = view; }

    
};

#endif /* Controller_hpp */
