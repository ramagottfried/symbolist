#ifndef Controller_hpp
#define Controller_hpp

#include "Observable.hpp"
#include "JuceHeader.h"

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
    
    ObservableClass* model;
    ViewClass* view;
    
    Observer* parentController;
    
public:
    /************************************************
     *                 CONSTRUCTORS                 *
     ************************************************/
    
    /**
     * Controller's empty constructor.
     */
    inline Controller()
    {
        // Asserting that model and view parameters derive from the appropriated types
        static_assert(std::is_base_of<Observable, ObservableClass>::value, "type parameter of this class must derive from Observable");
        static_assert(std::is_base_of<Observer, ViewClass>::value, "type parameter of this class must derive from Observer");
    };
    
    /**
     * Controller's destructor.
     */
    inline virtual ~Controller() {};
    
    /*******************************************************
     *                 GETTERS AND SETTERS                 *
     *******************************************************/
    inline virtual ObservableClass* getModel() { return model; }
    inline virtual void setModel(ObservableClass* model) { this->model = model; }
    
    inline virtual ViewClass* getView() { return view; }
    inline virtual void setView(ViewClass* view) { this->view = view; }
    
    inline virtual Observer* getParentController() { return parentController; }
    inline virtual void setParentController(Observer* parentController)
    {
        this->parentController = parentController;
    }
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (Controller)
};

#endif /* Controller_hpp */
