#include "BaseComponent.h"
#include "PageComponent.h"
#include "ScoreComponent.h"
#include "SymbolistMainComponent.h"
#include "StaffComponent.hpp"
#include "OdotAtom.hpp"

// do we need this ?
template <typename T> void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
}

BaseComponent::~BaseComponent()
{
    if ( staff )
        ((StaffComponent*)staff)->removeStaffOjbect(this);
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
    if (getParentComponent() != NULL) // we're in the score..
    {
        if (isTopLevelComponent())
        {
            getSymbolistHandler()->modifySymbolInScore(this);
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
    Symbol* s = new Symbol();
    addSymbolMessages(s);
    setScoreSymbolPointer(s);
}

// addSymbolMessages outputs the component's values into the symbol
void BaseComponent::addSymbolMessages(Symbol* s)
{
    s->addMessage("/name", name);
    s->addMessage("/type", getSymbolTypeStr());
    s->addMessage("/id", getComponentID().getCharPointer());
    s->addMessage("/staff", staff_name);
   
    auto b = symbol_export_bounds();
    
    s->addMessage("/x", b.getX());
    s->addMessage("/y", b.getY());
    s->addMessage("/w", b.getWidth());
    s->addMessage("/h", b.getHeight());
    s->addMessage("/color", sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha());

    /*
    addr = "/time/start";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         s->pixelsToTime( b.getX() ) );
        messages_added++;
    }

    addr = "/time/duration";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,   s->pixelsToTime( b.getWidth() ) );
        messages_added++;
    }
    */
    
   
    
    /* // not sure how to best do this yet
    addr = "/lambda";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         lambda );
        messages_added++;
    }
     */
    
//    cout << "*********** START BASE ADD DATA ************ " << endl;
//    s->printBundle();
    
}

Symbol BaseComponent::exportSymbol()
{
    Symbol s;
    s.addMessage("/name", name );
    s.addMessage("/type", getSymbolTypeStr() );
    s.addMessage("/id", getComponentID().getCharPointer() );
    s.addMessage("/staff", staff_name );
    
    auto b = symbol_export_bounds();
    
    s.addMessage("/x", b.getX() );
    s.addMessage("/y", b.getY() );
    s.addMessage("/w", b.getWidth() );
    s.addMessage("/h", b.getHeight() );
    s.addMessage("/color", sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha() );
    
    return s;
    
    /*
     addr = "/time/start";
     if( s->getOSCMessagePos(addr) == -1 )
     {
     s->addOSCMessage( addr,         s->pixelsToTime( b.getX() ) );
     messages_added++;
     }
     
     addr = "/time/duration";
     if( s->getOSCMessagePos(addr) == -1 )
     {
     s->addOSCMessage( addr,   s->pixelsToTime( b.getWidth() ) );
     messages_added++;
     }
     */
    
    
    
    /* // not sure how to best do this yet
     addr = "/lambda";
     if( s->getOSCMessagePos(addr) == -1 )
     {
     s->addOSCMessage( addr,         lambda );
     messages_added++;
     }
     */
    
    //    cout << "*********** START BASE ADD DATA ************ " << endl;
    //    s->printBundle();
    
}
/******************
 * Imports components' data from the symbol's OSC bundle (can be overriden by sub-class)
 *****************/

void BaseComponent::importFromSymbol(const Symbol &s)
{
    string typeOfSymbol = s.getMessage( "/type" ).getString();
    if (typeOfSymbol.size() == 0)
    {
        cout << "BaseComponent import: Could not find '/type' message in OSC Bundle.. (size=" << "insert bundle size here" << ")" << endl;
        return;
    }
    
    float x = s.getMessage("/x").getFloat();
    float y = s.getMessage("/y").getFloat();
    float w = s.getMessage("/w").getFloat();
    float h = s.getMessage("/h").getFloat();
    setBoundsFromSymbol(x, y, w, h);
    
    if (w == 0 || h == 0)
    {
        // Allow for the case where the score is specified in terms of time and staff type name...
        cout << "***** couldn't find x y w or h values " << endl;
    }
    
    auto color_atoms = s.getMessage("/color").getAtoms();
    if (color_atoms.size())
    {
        if (color_atoms[0].getType() == OdotAtom::OdotAtomType::O_ATOM_STRING)
        {
            sym_color = Colour::fromString(color_atoms[0].getString().c_str());
        }
        else if (color_atoms.size() == 4)
        {
            float r = color_atoms[0].getFloat();
            float g = color_atoms[1].getFloat();
            float b = color_atoms[2].getFloat();
            float a = color_atoms[3].getFloat();
            sym_color = Colour::fromFloatRGBA(r, g, b, a);
        }
        else if (color_atoms.size() == 3)
        {
            float r = color_atoms[0].getFloat();
            float g = color_atoms[1].getFloat();
            float b = color_atoms[2].getFloat();
            sym_color = Colour::fromFloatRGBA(r, g, b, 1.0);
        }
    }
    
    name = s.getMessage("/name").getString();
    if (name.size() == 0)
        name = typeOfSymbol;
        
    // set /id via component ID
    if(isVisible())
        setSymbolID();
    else
        setComponentID(name + "/palette");
    
    staff_name = s.getMessage("/staff").getString();
    if(staff_name == "<none>")
        staff_name = "";
        
    attachToStaff();
    
    /* // not sure how to best do this yet
    int lambda_pos = s.getOSCMessagePos("/lambda");
    if( lambda_pos != -1 )
    {
        lambda = s.getOSCMessageValue(lambda_pos).getString();
    }
    */
}

void BaseComponent::setSymbolID()
{
    PageComponent *pc = getPageComponent();
    if (pc)
    {
        Symbol* s = getScoreSymbolPointer();

        if (s)
        {
            String typeOfSymbol = s->getType();
            String id = s->getMessage("/id").getString();
            
            if( id.isEmpty() || id == (typeOfSymbol + "/palette")  || id == (name + "/palette") || !id.contains(String(name)))
            {
                // if there is a name use this for the id
                // for the id, check to see if there are others with this name and then increment 1
                
                auto sh = getSymbolistHandler();
                int count = sh->symbolNameCount( name );
                
                id = name + "/" + to_string(count);
				
				// Cannot pass directly id.toStdString() to uniqueIDCheck
				string stdStringId = id.toStdString();
				
                while(!sh->uniqueIDCheck(stdStringId))
                {
					id = name + "/" + to_string(count++);
					stdStringId = id.toStdString();
				}
				
            }
            
            setComponentID( id );
            s->addMessage( "/id", id.toStdString() );
        }
    }
}

void BaseComponent::attachToStaff()
{
    PageComponent *pc = getPageComponent();
    if (pc)
    {
        if (staff_name.size() != 0)
        {
            auto c = pc->getSubcomponentByID(staff_name);
            StaffComponent* staff_component = dynamic_cast<StaffComponent*>(c);
            
            // Checks the downcast result.
            if (staff_component != NULL)
            {
                staff_component->addOjbectToStave(this);
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
    setBounds(x, y, w, h);
}

Point<float> BaseComponent::computeSymbolPosition(float x, float y, float w, float h)
{
    return Point<float>(x,y);
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
        BaseComponent* c = (BaseComponent*) getSubcomponent(i);
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
        BaseComponent* c = (BaseComponent*) getSubcomponent(i);
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
        ((ScoreComponent*) getParentComponent())->reportModificationForSelectedSymbols();
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
    BaseComponent* parent = (BaseComponent*) getParentComponent();
    return ( isTopLevelComponent() || parent->isInEditMode() );
}

void BaseComponent::mouseMove( const MouseEvent& event )
{
    //std::cout << "BaseComponent::mouseMove" << std::endl;
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;

    if ( in_edit_mode || ( isTopLevelComponent() && getMainMouseMode() == UI_EditType::DRAW) )
    {
        ScoreComponent::mouseDown( event );
    }
    else
    {
        ScoreComponent* parent = (ScoreComponent*) getParentComponent();
        
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
    ScoreComponent* parent = (ScoreComponent*) getParentComponent();
    
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
        
        ScoreComponent* parent = (ScoreComponent*)getParentComponent();
        
        if ( respondsToMouseEvents() )
        {
            if( is_selected && (getMainMouseMode() == SELECTION ) )
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
    
    if ( in_edit_mode || ( isTopLevelComponent() && getMainMouseMode() == UI_EditType::DRAW ) )
        ScoreComponent::mouseUp(event);
    else
    {
        
        if ( respondsToMouseEvents() )
        {
            if( is_selected && getMainMouseMode() == SELECTION )
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
        g.setColour( Colour::fromFloatRGBA(1.0f, 1.0f, 1.0f, 0.9f)  );
        g.fillRect( getLocalBounds() );
    }
}

