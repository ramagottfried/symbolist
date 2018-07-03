#ifndef SymbolistModelTests_hpp
#define SymbolistModelTests_hpp

#include <stdio.h>
#include "SymbolistModel.hpp"

class SymbolistModelTestsFixture {
    
public:
    
    SymbolistModel* model;
    
    inline SymbolistModelTestsFixture() { model = new SymbolistModel(); }
    inline ~SymbolistModelTestsFixture() { delete model; }
    inline SymbolistModel* getModel() { return model; }
};

#endif /* SymbolistModelTests_hpp */
