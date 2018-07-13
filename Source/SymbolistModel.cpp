#include "SymbolistModel.hpp"

SymbolistModel::SymbolistModel()
{    
    this->score = unique_ptr<Score>( new Score() );
    this->palette = unique_ptr<Palette>(new Palette());
}

Symbol* SymbolistModel::addSymbolToScore( Symbol* symbol )
{
    Symbol* newSymbol = score->addSymbol(symbol);
    notify();
    
    return newSymbol;
}

void SymbolistModel::importSymbols( const OdotBundle_s& bundle )
{
    score->importSymbols( bundle );
    notify();
}

void SymbolistModel::removeAllSymbolsFromScore()
{
    getScore()->removeAllSymbols();
    notify();
}

void SymbolistModel::addSymbolToPalette(Symbol symbol)
{
    getPalette()->addUserItem(symbol);
    notify();
}

void SymbolistModel::updateExprInSymbol(Symbol* symbol, string newExpression)
{
	if (symbol != NULL && symbol->get_o_ptr() != NULL)
	{
		symbol->addMessage("/expr", newExpression);
		notify();
	}
}

void SymbolistModel::addStaff(Symbol* newStaff)
{
	getScore()->addStaff(newStaff);
	notify();
}

void SymbolistModel::removeSymbolFromScore( Symbol* s )
{
	getScore()->removeSymbolTimePoints(s);
	getScore()->removeSymbol(s);
	notify();
}


