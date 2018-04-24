//
//  SymbolTestsFixture.hpp
//  symbolist - Dynamic Library
//
//  Created by Vincent Iampietro on 20/04/2018.
//

#ifndef SymbolTestsFixture_hpp
#define SymbolTestsFixture_hpp

#include <stdio.h>
#include <string>
#include "Symbol.h"

class SymbolTestsFixture {

public:
	Symbol* symbol;

	inline SymbolTestsFixture() { symbol = new Symbol(); }
	inline ~SymbolTestsFixture() { delete symbol; }
	
};

#endif /* SymbolTestsFixture_hpp */
