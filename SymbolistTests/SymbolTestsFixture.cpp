//
//  SymbolTestsFixture.cpp
//  symbolist - Dynamic Library
//
//  Created by Vincent Iampietro on 20/04/2018.
//

#include "SymbolTestsFixture.hpp"
#include "catch.hpp"

TEST_CASE_METHOD(SymbolTestsFixture, "idExists looks for the searched id at all nested levels in a symbol.", "[model][symbol]")
{
	symbol->addMessage("/id", "group/1");
	symbol->addMessage("/type", "group");
	symbol->addMessage("/name", "myTopLevelSymbolGroup");
	
	SECTION("Looks for an id at one level of nesting.")
	{
		Symbol innerSymbol = Symbol();
		innerSymbol.addMessage("/id", "circle/1");
		
		symbol->addMessage("/subsymbol/1", innerSymbol);
		
		string groupId = "group/1";
		string innerSymbolId = "circle/1";
		
		CHECK(symbol->idExists(groupId));
		CHECK(symbol->idExists(innerSymbolId));
	
	}
	
	SECTION("Looks for an id at multiple levels of nesting.")
	{
		Symbol innerGroup = Symbol();
		innerGroup.addMessage("/id", "group/2");
		innerGroup.addMessage("/type", "group");
		
		Symbol innerSymbol = Symbol();
		innerSymbol.addMessage("/id", "circle/1");
		
		
		innerGroup.addMessage("/subsymbol/1", innerSymbol);
		symbol->addMessage("/subsymbol/1", innerGroup);
		
		string groupId = "group/1";
		string innerGroupId = "group/2";
		string innerSymbolId = "circle/1";
		
		CHECK(symbol->idExists(groupId));
		CHECK(symbol->idExists(innerGroupId));
		CHECK(symbol->idExists(innerSymbolId));
	}
	
}

TEST_CASE_METHOD(SymbolTestsFixture, "Odot expressions are well evaluated in a symbol.", "[model][symbol]")
{
	
	symbol->addMessage("/x", 12);
	symbol->addMessage("/y", 12);
	symbol->addMessage("/expr", "/pitch = /x + /y");

	symbol->applyExpr(symbol->getMessage("/expr").getString());
	
	CHECK(symbol->getMessage("/pitch").getInt() == 24);
}
