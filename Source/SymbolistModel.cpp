#include "SymbolistModel.hpp"

SymbolistModel::SymbolistModel()
{
    Observable::Observable();
    
    this->score = new Score();
    this->score->getTimePoints()->setScore(this->score);
    
    this->palette = new Palette();
}

SymbolistModel::SymbolistModel(Score* score, Palette* palette)
{
    Observable::Observable();
    
    this->score = score;
    this->score->getTimePoints()->setScore(this->score);
    
    this->palette = palette;
}
