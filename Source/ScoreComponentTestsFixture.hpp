#ifndef ScoreComponentTestsFixture_hpp
#define ScoreComponentTestsFixture_hpp

#include <stdio.h>
#include "ScoreComponent.h"

class ScoreComponentTestsFixture {

public:
	
	ScoreComponent* scoreComponent;
	
	ScoreComponentTestsFixture() { scoreComponent = new ScoreComponent(); }
  	~ScoreComponentTestsFixture() { delete scoreComponent; }

};


#endif /* ScoreComponentTestsFixture_hpp */
