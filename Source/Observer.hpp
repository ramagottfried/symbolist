#ifndef Observer_hpp
#define Observer_hpp

#include <stdio.h>


/**
 * Describes the observer class of the observer design pattern.
 * Observer is an abstract class.
 */
class Observer {
    
public:
    virtual ~Observer() {};
    
    /**
     * Updates the state of the observer.
     * The update method is a callback method
     * invoked by the observable object in the notify loop.
     *
     * @see Observable::notify
     */
    virtual void update() = 0;
    
};

#endif /* Observer_hpp */
