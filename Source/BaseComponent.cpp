
#include "BaseComponent.h"
#include "PageComponent.h"
#include "ScoreComponent.h"
#include "SymbolistMainComponent.h"
#include "StaffComponent.hpp"

// do we need this ?
template <typename T> void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}


BaseComponent::~BaseComponent()
{
    if( staff )
    {
        ((StaffComponent*)staff)->removeStaffOjbect(this);
    }
}


bool BaseComponent::isTopLevelComponent()
{
    if ( getParentComponent() != NULL && getParentComponent() == getPageComponent() )
    {
        if ( score_symbol != NULL )
        {
            return true;
        }
        else
        {
            std::cout << "Warning: BaseComponent is TopLevel but has no attached score symbol!" << std::endl ;
            return false;
        }
    }
    else
    {
        return false;
    }
    
}


// This is the function to call when we want to update the score after a modification
void BaseComponent::reportModification()
{
    if ( getParentComponent() != NULL ) // we're in the score..
    {
        if ( isTopLevelComponent() )
        {
            getSymbolistHandler()->modifySymbolInScore( this );
            // updateRelativeAttributes();
        }
        else
        {
            ((BaseComponent*) getParentComponent())->reportModification() ;
        }
    }
    
    /*
    if (inPlaceForRelativeUpdates())
    {
        updateRelativePos();
        updateRelativeSize();
    }
    */
}


/******************
 * Creates OSC Messages in the Symbol
 * Can be overriden / completed by class-specific messages
 *****************/

void BaseComponent::createAndAttachSymbol()
{
    Symbol *s = new Symbol();
    addSymbolMessages( s );
    setScoreSymbolPointer( s );
}

// addSymbolMessages outputs the component's values into the symbol
int BaseComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = 0;
    
    auto b = symbol_export_bounds();
    
    String addr;

    addr = base_address + "/name";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         name );
        messages_added++;
    }
    
    addr = base_address + "/type";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         getSymbolTypeStr());
        messages_added++;
    }


    addr = base_address + "/id";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,          getComponentID() );
        
    // no default name for now
      //  if( name.isEmpty() )
      //      name = getComponentID();
        
        messages_added++;
    }
    
    
   
    if( getSymbolTypeStr() != "staff ")
    {
        addr = base_address + "/staff";
        if( s->getOSCMessagePos(addr) == -1 )
        {
            s->addOSCMessage( addr,         staff_name );
            messages_added++;
        }
    }
    
    addr = base_address + "/x";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         b.getX() );
        messages_added++;
    }
    
    addr = base_address + "/y";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr ,        b.getY() );
        messages_added++;
    }
    
    addr = base_address + "/w";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr ,        b.getWidth() );
        messages_added++;
    }
    
    addr = base_address + "/h";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr ,        b.getHeight() );
        messages_added++;
    }
   
    /*
    addr = base_address + "/time/start";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         s->pixelsToTime( b.getX() ) );
        messages_added++;
    }

    addr = base_address + "/time/duration";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,   s->pixelsToTime( b.getWidth() ) );
        messages_added++;
    }
    */
    
    addr = base_address + "/color";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage(addr, sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha() );
        
        // s->addOSCMessage( OSCMessage( addr,   sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha()  ) );
        
        messages_added++;
    }
    
    /* // not sure how to best do this yet
    addr = base_address + "/lambda";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         lambda );
        messages_added++;
    }
     */
    
//    cout << "*********** START BASE ADD DATA ************ " << endl;
//    s->printBundle();
    
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle (can be overriden by sub-class)
 *****************/

void BaseComponent::importFromSymbol( const Symbol &s )
{
    int typeMessagePos = s.getOSCMessagePos("/type");
    
    if ( typeMessagePos == -1 ) {
        
        cout << "BaseComponent import: Could not find '/type' message in OSC Bundle.. (size=" << s.getOSCBundle()->size() << ")" << endl;
        
    } else {
        
        String typeStr = s.getOSCMessageValue(typeMessagePos).getString();
//        cout << "Importing BaseComponent from Symbol: " << typeStr << endl;
        
        
        int x_pos = s.getOSCMessagePos("/x");
        int y_pos = s.getOSCMessagePos("/y");
        int w_pos = s.getOSCMessagePos("/w");
        int h_pos = s.getOSCMessagePos("/h");
        
        if( x_pos != -1 && y_pos != -1 && w_pos != -1 && h_pos != -1  )
        {
            float x = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(x_pos) );
            float y = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(y_pos) );
            float w = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(w_pos) );
            float h = Symbol::getOSCValueAsFloat( s.getOSCMessageValue(h_pos) );
            
//            cout << "x " << x  << " y " << y << " w " << w << endl;
            
            setBoundsFromSymbol( x , y , w , h);
        }
        else
        {
            // Allow for the case where the score is specified in terms of time and staff type name...
            cout << "***** couldn't find x y w or h values " << endl;
        }
        
        
        int color_pos = s.getOSCMessagePos("/color");
        if( color_pos != -1  )
        {
            auto bndl = *(s.getOSCBundle());
            if( bndl[color_pos].getMessage().size() == 1 && bndl[color_pos].getMessage()[0].isString() )
            {
                sym_color = Colour::fromString( bndl[color_pos].getMessage()[0].getString() );
            }
            else if( bndl[color_pos].getMessage().size() == 4 )
            {
                float r = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[0] );
                float g = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[1] );
                float b = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[2] );
                float a = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[3] );
                sym_color = Colour::fromFloatRGBA( r, g, b, a );
            }
        }
        

        int name_pos = s.getOSCMessagePos("/name");
        if( name_pos != -1 )
            name = s.getOSCMessageValue(name_pos).getString();
        else
            name = typeStr;

        
        // set /id via component ID
        if( isVisible() )
            setSymbolID();
        else
            setComponentID( name + "/palette");
        
        
        int staffname_pos = s.getOSCMessagePos("/staff");
        if( staffname_pos != -1 )
        {
            staff_name = s.getOSCMessageValue(staffname_pos).getString();
            if( staff_name == "<none>" )
                staff_name = "";
            
            attachToStaff();
        }
        
        /* // not sure how to best do this yet
        int lambda_pos = s.getOSCMessagePos("/lambda");
        if( lambda_pos != -1 )
        {
            lambda = s.getOSCMessageValue(lambda_pos).getString();
        }
        */
    }
}

void BaseComponent::setSymbolID()
{
    PageComponent *pc = getPageComponent();
    if( pc )
    {
        Symbol *s = getScoreSymbolPointer();

        if( s )
        {
            String typeStr = s->getType();
            
            String id;
            int id_pos = s->getOSCMessagePos("/id");
            if( id_pos != -1 )
            {
                id = s->getOSCMessageValue(id_pos).getString();
            }
            

            if( id.isEmpty() || getComponentID() != id || id == (typeStr + "/palette")  || id == (name + "/palette") || !id.contains(name))
            {
                // if there is a name use this for the id
                // for the id, check to see if there are others with this name and then increment 1
                
                auto sh = getSymbolistHandler();
                int count = sh->symbolNameCount( name );
                
                id = name + "/" + (String)count;
                
                while( !sh->uniqueIDCheck( id ) )
                {
                    id = name + "/" + (String)(count++);
                }
                
                cout << "id check " << id << endl;
                
            }
            
            setComponentID( id );
            s->setID(id);
            //cout << "setting ID "<< id << endl;
        }
    }
}

void BaseComponent::attachToStaff()
{
    PageComponent *pc = getPageComponent();
    if( pc )
    {
        if( staff_name.isNotEmpty() )
        {
            auto c = pc->getSubcomponentByID( staff_name );
            StaffComponent* staff_component = dynamic_cast<StaffComponent*>(c);
            if( staff_component )
            {
                staff_component->addOjbectToStave( this );
                staff = staff_component;
            }
        }
        
    }
}

// a component can't access the score until it's been added and made visible, so we wait to process the the naming
void BaseComponent::parentHierarchyChanged()
{
//    cout << "BaseComponent::parentHierarchyChanged" << endl;
    setSymbolID();
    attachToStaff();
}


void BaseComponent::setBoundsFromSymbol( float x, float y , float w , float h)
{
    setBounds( x , y , w , h);
}


/******************
 * Called by selection mechanism
 *****************/

void BaseComponent::selectComponent()
{
    SymbolistComponent::selectComponent();
    
    if( isTopLevelComponent() )
        getSymbolistHandler()->addToInspector( this );
}

void BaseComponent::deselectComponent()
{
    SymbolistComponent::deselectComponent();
}


void BaseComponent::setEditMode(bool val)
{
    in_edit_mode = val;
    repaint();
}


bool BaseComponent::isInEditMode()
{
    return in_edit_mode;
}


const Colour BaseComponent::getCurrentColor()
{
    if ( is_selected ) return sel_color;
    else return sym_color;
}

/************************/
/* Expanding/srinking   */
/************************/

Rectangle<int> BaseComponent::getMinimalBounds()
{
    int minx = getWidth(), maxx = 0, miny = getHeight(), maxy = 0;
    for( int i = 0; i < getNumSubcomponents(); i++)
    {
        Rectangle<int> compBounds = getSubcomponent(i)->getBounds();
        minx =  min( minx, compBounds.getX() );
        miny =  min( miny, compBounds.getY() );
        maxx =  max( maxx, compBounds.getRight() );
        maxy =  max( maxy, compBounds.getBottom() );
    }
    return Rectangle<int>(minx, miny, maxx-minx, maxy-miny);
}

void BaseComponent::setMinimalBounds () {
    int minx = getWidth(), maxx = 0, miny = getHeight(), maxy = 0;
    for( int i = 0; i < getNumSubcomponents(); i++)
    {
        Rectangle<int> compBounds = getSubcomponent(i)->getBounds();
        minx =  min( minx, compBounds.getX() );
        miny =  min( miny, compBounds.getY() );
        maxx =  max( maxx, compBounds.getRight() );
        maxy =  max( maxy, compBounds.getBottom() );
    }
    setBounds(minx, miny, maxx-minx, maxy-miny);
    for( int i = 0; i < getNumSubcomponents(); i++)
    {
        SymbolistComponent* subcomp = getSubcomponent(i);
        subcomp->setTopLeftPosition( subcomp->getX()-getX(), subcomp->getY()-getY() );
    }
}

// Maximize I think is the same for everyone...
void BaseComponent::setMaximalBounds ()
{
    for ( int i  = 0; i < getNumSubcomponents(); i++ )
    {
        SymbolistComponent* subcomp = getSubcomponent(i);
        subcomp->setTopLeftPosition( getX()+subcomp->getX(), getY()+subcomp->getY() );
    }
    setBounds( 0, 0, getParentComponent()->getWidth(), getParentComponent()->getHeight());
}

void BaseComponent::recursiveMaximizeBounds()
{
    if ( ! isTopLevelComponent() )
        ((BaseComponent*) getParentComponent())->recursiveMaximizeBounds();
    
    setMaximalBounds();
}

void BaseComponent::recursiveShrinkBounds()
{
    setMinimalBounds();
    
    if ( ! isTopLevelComponent() )
        ((BaseComponent*) getParentComponent())->recursiveShrinkBounds();
}

/************************
 * Component modification callbacks
 ************************/


void BaseComponent::resizeToFit(int x, int y, int w, int h)
{
    
    cout << "resizeToFit " <<  getSymbolTypeStr() << " w " << w << " h " << h << endl;
    float scale_w = (float)w / (float)getWidth();
    float scale_h = (float)h / (float)getHeight();
    
    setTopLeftPosition(x, y);
    scaleScoreComponent(scale_w, scale_h);
}

/***
 *      repositions and sends scaling info to sub components
 *      each sub class needs to have it's own handling of how to scale, and adjust its size
 ***/
void BaseComponent::scaleScoreComponent(float scale_w, float scale_h)
{
    cout << "BaseComponent::scaleScoreComponent " << getSymbolTypeStr() << " " << this << " " << scale_w << " " << scale_h << endl;
    for ( int i = 0; i < getNumSubcomponents(); i++ )
    {
        BaseComponent* c = (BaseComponent*)getSubcomponent(i);
        c->setTopLeftPosition(c->getX() * scale_w, c->getY() * scale_h);
        c->scaleScoreComponent(scale_w, scale_h);
    }

}

/***
 *      sets size without scaling
 ***/
void BaseComponent::setScoreComponentSize(int w, int h)
{
    cout << "BaseComponent::setScoreComponentSize " << getSymbolTypeStr() << " " << this << " " << w << " " << h << endl;
    float this_w = (float)getWidth();
    float this_h = (float)getHeight();
    
    for ( int i = 0; i < getNumSubcomponents(); i++ )
    {
        BaseComponent* c = (BaseComponent*)getSubcomponent(i);
        float scale_w = (float)c->getWidth() / this_w;
        float scale_h = (float)c->getHeight() / this_h;
        c->setSize(w * scale_w, h * scale_h);
    }
    
    setSize(w, h);
    
    cout << "<<< end BaseComponent::setScoreComponentSize \n" << endl;
}


void BaseComponent::resized ()
{
    if( is_selected )
    {
        ((ScoreComponent*)getParentComponent())->reportModificationForSelectedSymbols();
    }
    
    if( staff )
    {
        staff->repaint();
    }
}

void BaseComponent::moved () {}

/************************
 * MOUSE INTERACTIONS
 ************************/

bool BaseComponent::respondsToMouseEvents()
{
    return ( isTopLevelComponent() || ((BaseComponent*)getParentComponent())->isInEditMode() );
}

void BaseComponent::mouseMove( const MouseEvent& event )
{
    //std::cout << "BaseComponent::mouseMove" << std::endl;
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;

    if ( in_edit_mode || ( isTopLevelComponent() && getMainMouseMode() == UI_EditType::draw) )
    {
        ScoreComponent::mouseDown( event );
    }
    else
    {
        
        ScoreComponent* parent = ((ScoreComponent*)getParentComponent());
        
        if ( respondsToMouseEvents() )
        {
            mouseDownSelection(event);
        }
        else
        {
            parent->mouseDown(event.getEventRelativeTo(parent));
        }
    }
}

void BaseComponent::altDragCopy( const MouseEvent& event  )
{
    is_alt_copying = true;
    ScoreComponent* parent = (ScoreComponent*)getParentComponent();
    
    if ( ! componentSelected() )
        parent->addToSelection(this);
    
    getSymbolistHandler()->copySelectedToClipBoard();
    
    parent->unselectAllComponents();
    getSymbolistHandler()->newFromClipBoard();
    
}

void BaseComponent::mouseDrag( const MouseEvent& event )
{
    
    if ( in_edit_mode )
        ScoreComponent::mouseDrag(event);
    else
    {
        
        ScoreComponent* parent = ( (ScoreComponent*) getParentComponent() );
        
        if ( respondsToMouseEvents() )
        {
            if( is_selected && (getMainMouseMode() == selection ) )
            {
                
                if( event.mods.isAltDown() && !is_alt_copying)
                {
                    altDragCopy( event );
                    m_prev_event = event.position - m_down;
                }
                else
                {
                    parent->translateSelectedComponents( (event.position - m_down).roundToInt() );
                }
                
            }
            else if( is_alt_copying )
            {
                auto delta_pt = (event.position - m_down - m_prev_event).roundToInt();
                parent->translateSelectedComponents( delta_pt );
                m_prev_event = event.position - m_down;
            }
            
        }
        else
        {
            parent->mouseDrag(event.getEventRelativeTo(parent));
        }
        
    }
}

void BaseComponent::mouseUp( const MouseEvent& event )
{
    
    if ( in_edit_mode || ( isTopLevelComponent() && getMainMouseMode() == UI_EditType::draw ) )
        ScoreComponent::mouseUp(event);
    else
    {
        
        if ( respondsToMouseEvents() )
        {
            if( is_selected && getMainMouseMode() == selection )
            {
                repaint();
            }
            ((ScoreComponent*) getParentComponent())->reportModificationForSelectedSymbols();
        }
        else
        {
            Component* parent = getParentComponent();
            parent->mouseUp(event.getEventRelativeTo(parent));
        }
    }
    
    if( is_alt_copying )
        is_alt_copying = false;
    
    if( staff )
    {
        staff->repaint();
    }
    
}


void BaseComponent::mouseDoubleClick(const MouseEvent& event)
{
    if ( (!in_edit_mode) && respondsToMouseEvents() )
    {
        getPageComponent()->enterEditMode( this );
    }
    else
    {
        Component* p = getParentComponent();
        p->mouseDoubleClick(event.getEventRelativeTo(p));
    }
}


/************************
 * DRAW
 ************************/



void BaseComponent::paint ( Graphics& g )
{
    if( in_edit_mode )
    {
        g.setColour( Colour::fromFloatRGBA(1.0f,1.0f,1.0f,0.9f)  );
        g.fillRect( getLocalBounds() );
    }
    
    //g.drawRect( getLocalBounds() );
}

