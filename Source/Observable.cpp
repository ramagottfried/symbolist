//
//  Observable.cpp
//  symbolist
//
//  Created by Vincent Iampietro on 20/03/2018.
//

#include "Observable.hpp"

Observable::Observable()
{
    observers = OwnedArray<Observer>();
}

void Observable::attach(Observer *observer)
{
    if (!observers.contains(observer))
    {
        observers.add(observer);
    }
}

void Observable::detach(Observer *observer)
{
    if (observers.contains(observer))
    {
        observers.removeObject(observer);
    }
}

void Observable::notify() {
    
    for (Observer* observer : observers) {
        observer->update();
    }
    
}
