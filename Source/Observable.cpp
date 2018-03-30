#include "Observable.hpp"

Observable::Observable()
{
    observers = std::vector<Observer* >();
}

void Observable::attach(Observer* observer)
{
    /*
     * find_if calls a lambda to compare the pointers,
     * since weak_ptr doesn't implement the == operator.
     */
    auto iteratorOnObserver = find(observers.begin(),
                                   observers.end(),
                                   observer);
    
    /* find() returns an iterator on the last element
     * if observer is not in observers.
     */
    if (iteratorOnObserver != observers.end())
        observers.push_back(observer);
        
}

void Observable::detach(Observer* observer)
{
    /*
     * find_if calls a lambda to compare the pointers,
     * since weak_ptr doesn't implement the == operator.
     */
    auto iteratorOnObserver = find(observers.begin(),
                                   observers.end(),
                                   observer);
    
    /* find() returns an iterator on the last element
     * if observer is not in observers.
     */
    if (iteratorOnObserver != observers.end())
        observers.erase(iteratorOnObserver);

}

void Observable::notify() {
    
    for (Observer* observer : observers)
    {
        if (observer != NULL)
            observer->update();
    }
    
}
