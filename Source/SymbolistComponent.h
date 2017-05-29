//
//  SymbolistComponent.hpp
//  symbolist
//
//  Created by Jean Bresson on 28/05/2017.
//
//

#ifndef SymbolistComponent_hpp
#define SymbolistComponent_hpp


class SymbolistComponent : public Component
{
public:
    virtual SymbolistComponent* getTPLScoreComponent() {
        auto p = static_cast<SymbolistComponent*>( getParentComponent() );
        if (p == NULL) {
            return p;
        }
        else return p->getTPLScoreComponent();
        
    }

    enum UI_MouseMode
    {
        edit,
        path,
        circle
    };
    
};

#endif /* SymbolistComponent_hpp */
