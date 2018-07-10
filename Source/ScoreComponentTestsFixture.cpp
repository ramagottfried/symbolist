#include "ScoreComponentTestsFixture.hpp"
#include "catch.hpp"
#include "EditSelectionBox.h"

TEST_CASE_METHOD(ScoreComponentTestsFixture, "The copy constructor works well for ScoreComponent", "[unit][view][score-components]")
{
	ScoreComponent* firstSubComponent = new ScoreComponent();
	ScoreComponent* secondSubComponent = new ScoreComponent();
	
	scoreComponent->addSubcomponent(firstSubComponent);
	scoreComponent->addSubcomponent(secondSubComponent);
	scoreComponent->addToSelection(firstSubComponent);
	
	ScoreComponent* copyScoreComponent = new ScoreComponent(scoreComponent);
	
	// Checks subcomponents are well-copied.
	CHECK(copyScoreComponent->getSubcomponents()->getFirst() != NULL);
	CHECK(copyScoreComponent->getSubcomponents()->getLast() != NULL);
	
	// Checks subcomponents are different from original to copy.
	CHECK(firstSubComponent != copyScoreComponent->getSubcomponents()->getFirst());
	CHECK(secondSubComponent != copyScoreComponent->getSubcomponents()->getLast());
	
	// Checks selected components are added to selection box.
	CHECK(scoreComponent->getEditSelectionBox()->getComponentSet() != NULL);
	CHECK(copyScoreComponent->getEditSelectionBox()->getComponentSet() != NULL);
	
	delete copyScoreComponent;
	
	// Checks original sub components still exist after copy deletion.
	CHECK(firstSubComponent != NULL);
	CHECK(secondSubComponent != NULL);
	
	
}



