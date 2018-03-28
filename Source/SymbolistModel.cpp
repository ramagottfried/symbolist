#include "SymbolistModel.hpp"

SymbolistModel::SymbolistModel()
{
    Observable::Observable();
    
    this->score = make_shared<Score>();
    this->score->getTimePoints()->setScore(this->score);
    
    this->palette = make_shared<Palette>();
}

SymbolistModel::SymbolistModel(shared_ptr<Score> score, shared_ptr<Palette> palette)
{
    Observable::Observable();
    
    this->score = score;
    this->score->getTimePoints()->setScore(this->score);
    
    this->palette = palette;
}
