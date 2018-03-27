#ifndef Observable_hpp
#define Observable_hpp

#include <stdio.h>
#include "Observer.hpp"
#include "../JuceLibraryCode/JuceHeader.h"

using namespace std;

/**
 * Describes the Observable class from the observer
 * design pattern.
 * The observer design pattern is necessary to implement
 * the model-view-controller architecture.
 *
 */
class Observable {

    /**
     * An array of observers which are listening
     * to this Observable object.
     */
    vector<shared_ptr<Observer> > observers;
    
public:
    
    /**
     * Observable's sole constructor.
     * Initializes the observers array.
     */
    Observable();
    
    /**
     * Observable's destructor method.
     */
    virtual ~Observable() { };
    
    /**
     * Adds a new observer to the list of observers if
     * it is not already in the list.
     *
     * @param observer the observer to be added to the
     *                 list of observers.
     */
    void attach(shared_ptr<Observer> observer);
    
    /**
     * Removes the specified observer from the list if
     * it exists.
     *
     * @param observer the observer to be removed from the
     *                 list of observers.
     */
    void detach(shared_ptr<Observer> observer);
    
    /**
     * Notifies all the observers that an event
     * occured by calling the <code>Observer::update()</code>
     * them.
     */
    void notify();
    
};

#endif /* Observable_hpp */
