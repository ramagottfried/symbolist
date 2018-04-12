#include "SymbolistModel.hpp"

SymbolistModel::SymbolistModel()
{
    Observable::Observable();
    
    this->score = unique_ptr<Score>( new Score() );
    this->palette = unique_ptr<Palette>(new Palette());
}

Symbol* SymbolistModel::addSymbolToScore(Symbol* symbol)
{
    Symbol* newSymbol = score->addSymbol(symbol);
    notify();
    
    return newSymbol;
}

void SymbolistModel::importScoreFromOSC(const OdotBundle_s& bundleArray)
{
    score->importScoreFromOSC(bundleArray);
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


