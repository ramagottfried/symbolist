#include "SymbolistModelTestsFixture.hpp"
#include "catch.hpp"

TEST_CASE_METHOD(SymbolistModelTestsFixture, "Score and palette are not null after model creation", "[unit][model]")
{
    REQUIRE(getModel()->getScore() != NULL);
    REQUIRE(getModel()->getPalette() != NULL);
    
}
