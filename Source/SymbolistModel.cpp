#include "SymbolistModel.hpp"

SymbolistModel::SymbolistModel()
{
    Observable::Observable();
    
    this->score = make_shared<Score>();
    this->palette = make_shared<Palette>();
}
