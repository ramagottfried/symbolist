#include "ScoreTestsFixture.hpp"
#include "catch.hpp"

TEST_CASE_METHOD(ScoreTestsFixture, "After creation, score_symbols, time_points and staves are of size zero.", "[unit][model][score]")
{
    REQUIRE(score->getSize() == 0);
    REQUIRE(score->getStaves().size() == 0);
    REQUIRE(score->getTimePoints()->getSymbolTimePoints().size() == 0);
}

TEST_CASE_METHOD(ScoreTestsFixture, "Symbols are well sorted in the score.", "[unit][model][score]")
{
    Symbol* firstSymbol = score->createSymbol();
    Symbol* secondSymbol = score->createSymbol();
    
    SECTION("Symbols with no time start are placed in the creation order.")
    {
        REQUIRE(score->getSymbolPosition(firstSymbol) == 0);
        REQUIRE(score->getSymbolPosition(secondSymbol) == 1);
    }
    
    SECTION("Symbols are sorted according to time start value.")
    {
        firstSymbol->setTimeAndDuration(10.0, 2.0);
        secondSymbol->setTimeAndDuration(15.0, 3.0);
        
        // Creates symbol and sets its time start value.
        Symbol* symbol = new Symbol();
        symbol->setTimeAndDuration(12.0, 1.0);
        
        Symbol* thirdSymbol = score->addSymbol(symbol);
        
        REQUIRE(score->getSymbolPosition(firstSymbol) == 0);
        REQUIRE(score->getSymbolPosition(thirdSymbol) == 1);
        REQUIRE(score->getSymbolPosition(secondSymbol) == 2);
    }
    
    SECTION("Symbols with same start value are sorted in order of creation.")
    {
        // Creates symbol and sets its time start value.
        Symbol* symbol = new Symbol();
        
        // Same start time and duration as secondSymbol.
        symbol->setTimeAndDuration(12.0, 1.0);
        
        Symbol* fourthSymbol = score->addSymbol(symbol);
        REQUIRE(score->getSymbolPosition(fourthSymbol) == 2);
    }
    
}

TEST_CASE_METHOD(ScoreTestsFixture, "Symbols are well copied to the score.", "[unit][model][score]")
{
    // Creates a new symbol and adds messages to it.
    Symbol* symbol = new Symbol();
    symbol->setTypeXYWH("circle", 30.0, 30.0, 200, 200);
    symbol->setTimeAndDuration(15.0, 15.0);
    
    /* Inserts a new symbol in the score by copying
     * the one passed in argument.
     */
    Symbol* insertedSymbol = score->addSymbol(symbol);
    
    // Statements to make sure the symbol is well copied.
    REQUIRE(symbol->getType() == insertedSymbol->getType());
    REQUIRE(symbol->getMessage("/x").getFloat() == insertedSymbol->getMessage("/x").getFloat());
    REQUIRE(symbol->getMessage("/y").getFloat() == insertedSymbol->getMessage("/y").getFloat());
    REQUIRE(symbol->getMessage("/w").getFloat() == insertedSymbol->getMessage("/w").getFloat());
    REQUIRE(symbol->getMessage("/h").getFloat() == insertedSymbol->getMessage("/h").getFloat());
    REQUIRE(symbol->getTime() == insertedSymbol->getTime());
    REQUIRE(symbol->getDuration() == insertedSymbol->getDuration());
    
}

TEST_CASE_METHOD(ScoreTestsFixture, "Timepoints are well created when staff-linked symbol is added.", "[unit][model][score]")
{
    Symbol* staffSymbol = new Symbol();
    staffSymbol->setTypeXYWH("staff", 10.0, 10.0, 200.0, 200.0);
    staffSymbol->setTimeAndDuration(10.0, 20.0);
    staffSymbol->addMessage("/id", "/staff/0");
    staffSymbol->addMessage("/name", "myStaff");
    
    score->addSymbol(staffSymbol);
    
    SECTION("Timepoints size is zero when no symbol in score.")
    {
        REQUIRE(score->getTimePoints()->getSymbolTimePoints().size() == 0);
    }
    
    SECTION("Timepoints size increase when staff-linked symbol is added to score.")
    {
        Symbol* linkedSymbol = new Symbol();
        linkedSymbol->setTypeXYWH("circle", 10.0, 10.0, 200.0, 200.0);
        linkedSymbol->setTimeAndDuration(10.0, 20.0);
        
        // Links linkedSymbol to staffSymbol.
        linkedSymbol->addMessage("/staff", staffSymbol->getID());
        
        score->addSymbol(linkedSymbol);
        
        /* TimePoints size is 2 because timepoints have been added
         * for the symbol's start time and end time.
         */
        REQUIRE(score->getTimePoints()->getSymbolTimePoints().size() == 2);
    }
    
}

TEST_CASE_METHOD(ScoreTestsFixture, "Symbols are well removed from the score.", "[unit][model][score]")
{
    SECTION("Score size decreases after symbol removal.")
    {
        Symbol* symbolToRemove = score->createSymbol();
        REQUIRE(score->getSize() == 1); // Ensures that the symbol is created.
        
        score->removeSymbol(symbolToRemove);
        REQUIRE(score->getSize() == 0);
    }
    
    SECTION("Attempting to remove symbol from empty score throws an exception.")
    {
        REQUIRE(score->getSize() == 0); // Ensures that score is empty.
        REQUIRE_THROWS_AS(score->removeSymbol(new Symbol()), logic_error);
    }
    
    SECTION("Attempting to remove a symbol which is not among the score throws an exception.")
    {
        score->createSymbol(); // Symbol creation.
        REQUIRE(score->getSize() == 1); // Ensures that score is not empty.
        
        REQUIRE_THROWS_AS(score->removeSymbol(new Symbol()), invalid_argument);
    }
    
    SECTION("Passing NULL pointer as argument throws a exception.")
    {
        REQUIRE_THROWS_AS(score->removeSymbol(NULL), invalid_argument);
    }
    
}
