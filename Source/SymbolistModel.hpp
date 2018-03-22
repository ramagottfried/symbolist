#ifndef SymbolistModel_hpp
#define SymbolistModel_hpp

#include <stdio.h>
#include "Observable.hpp"
#include "Score.h"
#include "Palette.hpp"

/**
 * Represents the business logic of the symbolist application.
 * The SymbolistModel class references a Score object and a Palette
 * object as instance variables. The score and palette
 * characterize the data layer of the symbolist application.
 */
class SymbolistModel : public virtual Observable {
    
    ScopedPointer<Score> score;
    ScopedPointer<Palette> palette;
    
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
    SymbolistModel(Score* score, Palette* palette);
    
    /**
     * SymbolistModel's default destructor.
     */
    virtual ~SymbolistModel();
    
    /*******************************************************
     *                 GETTERS AND SETTERS                 *
     *******************************************************/
    inline ScopedPointer<Score> getScore() { return score; }
    inline void setScore(ScopedPointer<Score> score) { this->score = score; }
    
    inline ScopedPointer<Palette> getPalette() { return palette; }
    inline void setPalette(ScopedPointer<Palette> palette) { this->palette = palette; }
    
};


#endif /* SymbolistModel_hpp */
