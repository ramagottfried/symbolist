#include "Observable.hpp"

Observable::Observable()
{
    observers = vector<shared_ptr<Observer> >();
}

void Observable::attach(shared_ptr<Observer> observer)
{
    auto iteratorOnObserver = find(observers.begin(), observers.end(), observer);
    
    /* find() returns an iterator on the last element
     * if observer is not in observers.
     */
    if (iteratorOnObserver != observers.end())
        observers.push_back(observer);
        
}

void Observable::detach(shared_ptr<Observer> observer)
{
    auto iteratorOnObserver = find(observers.begin(), observers.end(), observer);
    
    /* find() returns an iterator on the last element
     * if observer is not in observers.
     */
    if (iteratorOnObserver != observers.end())
        observers.erase(iteratorOnObserver);

}

void Observable::notify() {
    
    for (shared_ptr<Observer> observer : observers)
        observer->update();
    
}
