#ifndef Observable_hpp
#define Observable_hpp

#include "Observer.hpp"
#include <memory>
#include <iostream>
#include <vector>
#include "JuceHeader.h"

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
    std::vector<Observer* > observers;
    
public:
    
    /**
     * Observable's sole constructor.
     * Initializes the observers array.
     */
    Observable();
    
    /**
     * Observable's destructor method.
     */
    virtual ~Observable()
    {
        for (Observer* observer : observers)
            delete observer;
    };
    
    /**
     * Adds a new observer to the list of observers if
     * it is not already in the list.
     *
     * @param observer the observer to be added to the
     *                 list of observers.
     */
    void attach(Observer* observer);
    
    /**
     * Removes the specified observer from the list if
     * it exists.
     *
     * @param observer the observer to be removed from the
     *                 list of observers.
     */
    void detach(Observer* observer);
    
    /**
     * Notifies all the observers that an event
     * occured by calling the <code>Observer::update()</code>
     * them.
     */
    void notify();
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (Observable)
};

#endif /* Observable_hpp */
