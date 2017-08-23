#pragma once

#include "SymbolGroupComponent.h"

/*
 *  staff component is a type of group, but one of the elements of the group is a "staff" which determines the time position of the other subcomponents of the group.
    .. unlike the group component, the staff does not remove the symbol from the score, which allows each symbol to be individually referenced from the TimePoint array.
 
    subcomponents require that the toplevel component contains their bounds...
 
    the BaseComponent drawn is the staff
    then, the attached components are updated if the staff is moved
 
    if the staff is deleted then what? keep the reference in the score symbol and just fail if it's not found I think might be the best
        in that case after creation we need to search the score for attachments
 
    converting an object into a staff changes it's type:
    but it doesn't have subcomponents...or it could but not subsymbols... 
 
 
    ...
 
    at least one subcomponent: the staff 
 

 
 *
 */


class StaffComponent : public SymbolGroupComponent
{
public:
    StaffComponent() = default;
    ~StaffComponent() = default;
    
    virtual String getSymbolTypeStr() const override { return "staff"; }
    
    void importFromSymbol( const Symbol &s ) override;
    int addSymbolMessages( Symbol* s, const String &base_address ) override;

private:
    
    vector<BaseComponent*>  symbols_on_staff;
    
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (StaffComponent)

};

