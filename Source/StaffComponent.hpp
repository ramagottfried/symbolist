#pragma once

#include "SymbolGroupComponent.h"

/**
 *  A staff component is a type of group, but one of the elements of the group is a "staff" which determines the time position of the other subcomponents of the group.
 *   .. unlike the group component, the staff does not remove the symbol from the score, which allows each symbol to be individually referenced from the TimePoint array.
 *
 *   Subcomponents require that the toplevel component contains their bounds...
 *
 *   The BaseComponent drawn is the staff
 *   then, the attached components are updated if the staff is moved
 *
 *   If the staff is deleted then what? keep the reference in the score symbol and just fail if it's not found I think might be the best
 *       in that case after creation we need to search the score for attachments
 *
 *   Converting an object into a staff changes it's type:
 *   but it doesn't have subcomponents...or it could but not subsymbols...
 *
 *   There is at least one subcomponent in the group: the staff
 *
 */
class StaffComponent : public SymbolGroupComponent
{
public:
    StaffComponent() = default;
    ~StaffComponent() = default;
    
    virtual string getSymbolTypeStr() const override { return "staff"; }
    
    void importFromSymbol( const Symbol &s ) override;
    void addSymbolMessages(shared_ptr<Symbol> s ) override;
    
    void parentHierarchyChanged() override;

    void paint ( Graphics& g ) override;

    void selectComponent() override;
    void deselectComponent() override;
    
    inline void addOjbectToStave( BaseComponent *c)
    {
        symbols_on_staff.emplace_back(c);
    }
    
    inline void removeStaffOjbect( BaseComponent *c)
    {
        symbols_on_staff.erase( remove( symbols_on_staff.begin(), symbols_on_staff.end(), c ), symbols_on_staff.end() );
    }
    
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    
    bool hitTest (int x, int y) override;
    
    
private:
    
    vector<BaseComponent*>  symbols_on_staff;
    
    bool    draw_timepoints = false;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (StaffComponent)

};

