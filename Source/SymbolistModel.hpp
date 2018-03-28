#ifndef SymbolistModel_hpp
#define SymbolistModel_hpp

#include <stdio.h>
#include "Observable.hpp"
#include "Score.h"
#include "Palette.hpp"

using namespace std;

/**
 * Represents the business logic of the symbolist application.
 * The SymbolistModel class references a Score object and a Palette
 * object as instance variables. The score and palette
 * characterize the data layer of the symbolist application.
 */
class SymbolistModel : public virtual Observable {
    
    shared_ptr<Score> score;
    shared_ptr<Palette> palette;
    
public:
    
    /************************************************
     *                 CONSTRUCTORS                 *
     ************************************************/
    
    /**
     * SymbolistModel's empty constructor.
     */
    SymbolistModel();
    
    /**
     * SymbolistModel's constructor taking a Score and a Palette
     * object as arguments.
     */
    SymbolistModel(shared_ptr<Score> score, shared_ptr<Palette> palette);
    
    /**
     * SymbolistModel's default destructor.
     */
    virtual ~SymbolistModel() {};
    
    /*******************************************************
     *                 GETTERS AND SETTERS                 *
     *******************************************************/
    inline shared_ptr<Score> getScore() { return score; }
    inline void setScore(shared_ptr<Score> score) { this->score = score; }
    
    inline shared_ptr<Palette> getPalette() { return palette; }
    inline void setPalette(shared_ptr<Palette> palette) { this->palette = palette; }
    
};


#endif /* SymbolistModel_hpp */
