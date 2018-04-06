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
    
    unique_ptr<Score>   score;
    unique_ptr<Palette> palette;
    
public:
    
    /************************************************
     *                 CONSTRUCTORS                 *
     ************************************************/
    
    /**
     * SymbolistModel's empty constructor.
     */
    SymbolistModel();
    
    /**
     * SymbolistModel's default destructor.
     */
    inline virtual ~SymbolistModel() {}
    
    /*******************************************************
     *                 GETTERS AND SETTERS                 *
     *******************************************************/
    inline Score* getScore() { return score.get(); }
    inline void setScore(Score* score) { this->score = unique_ptr<Score>(score); }
    inline Palette* getPalette() { return palette.get(); }
    inline void setPalette(Palette* palette) { this->palette = unique_ptr<Palette>(palette); }
    
    //==============================================================================
    JUCE_LEAK_DETECTOR (SymbolistModel)
};


#endif /* SymbolistModel_hpp */
