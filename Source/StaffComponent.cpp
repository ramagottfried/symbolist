
#include "StaffComponent.hpp"

#include "PageComponent.h"
#include "ScoreComponent.h"


void StaffComponent::importFromSymbol( const Symbol &s )
{
    clearAllSubcomponents();
    
    BaseComponent::importFromSymbol(s);
    
    auto subsym = Symbol( s.getMessage( "/subsymbol" ).getBundle().get_o_ptr() ); // there can be only one staff subsymbol, must be grouped if multiple
    
    if( subsym.size() == 0 )
    {
        cout << "no staff subsymbol found" << endl;
        return;
    }
    
    BaseComponent* c = getSymbolistHandler()->makeComponentFromSymbol( &subsym , false );
    
    if ( c != NULL)
        addSubcomponent( c );
    else
        cout << "Error importing staffSymbol " << endl;
        
}

void StaffComponent::addSymbolMessages( Symbol* s )
{
    BaseComponent::addSymbolMessages( s );

    if( getNumSubcomponents() )
    {
        auto sub_c = getSubcomponent(0);
        if( sub_c )
        {
            Symbol sub_sym;
            ((BaseComponent*)sub_c)->addSymbolMessages( &sub_sym );
            s->addMessage( "/subsymbol", sub_sym );
        }
        else
        {
            cout << "no subcomponent found" << endl;
        }
    }
    
}

void StaffComponent::parentHierarchyChanged()
{
    BaseComponent::parentHierarchyChanged();
    
    PageComponent *pc = getPageComponent();
    if( pc )
    {
        if( Symbol *s = getScoreSymbolPointer() )
        {
            String id = s->getID();
        
            auto objects = pc->getSubcomponentsByStaff( id );
            for( auto o : objects )
            {
                addOjbectToStave( o );
            }
            
            objects.clear();
        }
    }
    // repaint();
}

void StaffComponent::selectComponent()
{
    SymbolGroupComponent::selectComponent();
    /*
    auto page = getPageComponent();
    for( BaseComponent *c : symbols_on_staff )
    {
        page->addToSelection( c );
    }
    */
}

void StaffComponent::deselectComponent()
{
    SymbolGroupComponent::deselectComponent();
}


bool StaffComponent::hitTest (int x, int y)
{
    if( in_staff_selection_mode )
    {
        return true;
    }
    
    return SymbolGroupComponent::hitTest ( x, y );
    
}


void StaffComponent::mouseDown( const MouseEvent& event )
{
    
    if( in_staff_selection_mode )
    {
        for( auto s : getPageComponent()->getSelectedItems() )
        {
            BaseComponent *c = dynamic_cast<BaseComponent*>(s);
            if( c )
            {
                if( c->getSymbolTypeStr() != "staff" )
                {
                    addOjbectToStave(c);
                    c->setStaff(this);
                    getSymbolistHandler()->modifySymbolInScore(c);
                }
            }
        }
    }
    else
    {
       BaseComponent::mouseDown(event);
    }
}

void StaffComponent::mouseDrag( const MouseEvent& event )
{
    auto page = getPageComponent();

    if( is_selected )
    {
        
//        cout << "StaffComponent::mouseDrag teste " << event.getDistanceFromDragStart() << endl;

        for( BaseComponent *c : symbols_on_staff )
        {
            page->addToSelection( c );
        }
        
    }
    
    BaseComponent::mouseDrag( event );
    
    page->updateTimeCursor();
}



void StaffComponent::paint ( Graphics& g )
{
    BaseComponent::paint( g );
 
    if( draw_timepoints )
    {
        auto timepoints = getSymbolistHandler()->getTimePointArray();
        
        float start_t = getScoreSymbolPointer()->getTime();
        float end_t = start_t + getScoreSymbolPointer()->getDuration();
        
        for( auto t : (*timepoints) )
        {
            if( t->time >= start_t && t->time <= end_t )
                g.fillEllipse( (t->time - start_t) * 100.0f, getHeight() / 2, 2, 2);
        }
    }
    
    if( in_staff_selection_mode )
    {
        g.setColour( Colours::lightblue );
        g.fillRect( getLocalBounds() );
        
        auto f = g.getCurrentFont();
        f.setItalic(true);
        f.setHeight( 10 );
        g.setFont( f );
        
        g.setColour( Colours::black );
        g.drawText( getScoreSymbolPointer()->getID(), getLocalBounds().reduced(10), Justification::centredLeft );
    }
}
