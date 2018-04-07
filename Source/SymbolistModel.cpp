#include "SymbolistModel.hpp"

SymbolistModel::SymbolistModel()
{
    Observable::Observable();
    
    this->score = unique_ptr<Score>( new Score() );
    this->score->getTimePoints()->setScore(this->score.get());
    
    this->palette = unique_ptr<Palette>(new Palette());
}

