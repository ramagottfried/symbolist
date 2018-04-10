//
//  ScoreTestsFixture.hpp
//  SymbolistTests
//
//  Created by Vincent Iampietro on 10/04/2018.
//

#ifndef ScoreTestsFixture_hpp
#define ScoreTestsFixture_hpp

#include <stdio.h>
#include "Score.h"

class ScoreTestsFixture {

public:
    
    Score* score;
    
    inline ScoreTestsFixture() { score = new Score(); };
    inline ~ScoreTestsFixture() { delete score; }

};

#endif /* ScoreTestsFixture_hpp */
