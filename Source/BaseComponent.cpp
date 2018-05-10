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
    if ( staff != NULL)
        staff->removeStaffObject(this);
}

bool BaseComponent::isTopLevelComponent()
{
    if ( getParentComponent() != NULL && getParentComponent() == getPageComponent() )
    {
        if ( score_symbol != NULL )
            return true;
        else
        {
            DEBUG_FULL("Warning: BaseComponent is TopLevel but has no attached score symbol!" << endl)
            return false;
        }
    }
    else
        return false;
    
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
	if (getSymbolistHandler() != NULL)
	{
		Symbol* s = getSymbolistHandler()->createSymbol();
    	addSymbolMessages(s);
    	setScoreSymbol(s);
	}
	else throw logic_error("BaseComponent has no access to the SymbolistHandler instance.");
	
}

// addSymbolMessages outputs the component's values into the symbol
void BaseComponent::addSymbolMessages(Symbol* s)
{
    s->addMessage("/name", name);
    s->addMessage("/type", getSymbolTypeStr());
    s->addMessage("/id", getComponentID().getCharPointer());
	s->addMessage("/staff", staff_id);
   
    auto b = symbolExportBounds();
    
    s->addMessage("/x", b.getX());
    s->addMessage("/y", b.getY());
    s->addMessage("/w", b.getWidth());
    s->addMessage("/h", b.getHeight());
    s->addMessage("/color", sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha());

}

Symbol BaseComponent::exportSymbol()
{
    Symbol s;
    s.addMessage("/name", name );
    s.addMessage("/type", getSymbolTypeStr() );
    s.addMessage("/id", getComponentID().getCharPointer() );
	s.addMessage("/staff", staff_id );
    
    auto b = symbolExportBounds();
    
    s.addMessage("/x", b.getX() );
    s.addMessage("/y", b.getY() );
    s.addMessage("/w", b.getWidth() );
    s.addMessage("/h", b.getHeight() );
    s.addMessage("/color", sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha() );
    
    return s;
    
}
/******************
 * Imports components' data from the symbol's OSC bundle (can be overriden by sub-class)
 *****************/

void BaseComponent::importFromSymbol(const Symbol &s)
{
    string typeOfSymbol = s.getMessage( "/type" ).getString();
    if (typeOfSymbol.size() == 0)
    {
        DEBUG_FULL("BaseComponent import: Could not find '/type' message in OSC Bundle.. (size=" << "insert bundle size here" << ")" << endl)
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
        DEBUG_FULL("***** couldn't find x y w or h values " << endl)
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
        
    // Set /id via component ID
    string id = s.getMessage("/id").getString();
	
    // If the symbol id already exists, sets the component id with it.
	if (id.size() > 0)
		setComponentID(id);
	/* If the component is already visible, sets the component id
	 * with the id of ots attached symbol
	 */
    else if (isVisible())
		setIdFromSymbol();
	else
		setComponentID(typeOfSymbol + "/palette");
	
    staff_id = s.getMessage("/staff").getString();
    if(staff_id == "<none>")
        staff_id = "";
        
    attachToStaff();
	
}

void BaseComponent::setIdFromSymbol()
{
    PageComponent* pc = getPageComponent();
	
    if (pc)
    {
        Symbol* s = getScoreSymbol();
		String id;
		
		/* If the component is attached to a score symbol
		 * then gets its id or calculate a new one if it is an empty
		 * or a default id.
		 */
        if (s)
        {
            String typeOfSymbol = s->getType();
			id = s->getMessage("/id").getString();
            
            if( id.isEmpty() || id == (typeOfSymbol + "/palette")  || id == (name + "/palette") || !id.contains(String(name)))
                id = pc->getController()->createIdFromName(name);
			
            s->addMessage( "/id", id.toStdString() );
			
            setComponentID( id );
			
        }
		
    }

}

void BaseComponent::attachToStaff()
{
    PageComponent* scoreView = getPageComponent();
    if (scoreView)
    {
        if (staff_id.size() != 0)
        {
            auto c = scoreView->getSubcomponentByID(staff_id);
            StaffComponent* staffComponent = dynamic_cast<StaffComponent*>(c);
            
            // Checks the downcast result.
            if (staffComponent != NULL)
            {
                staffComponent->addObjectToStave(this);
                staff = staffComponent;
            }
        }
        
    }
}

void BaseComponent::setStaff(StaffComponent* c)
{
	staff_id = c->getScoreSymbol()->getID();
	staff = c;
	DEBUG_INLINE("/t/t ------------------------- \n" << this << " attached to staff " << staff_id << " " << staff << endl)
}

// a component can't access the score until it's been added and made visible, so we wait to process the naming
void BaseComponent::parentHierarchyChanged()
{
    setIdFromSymbol();
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
    ScoreComponent::selectComponent();
    
    if( isTopLevelComponent() )
        getSymbolistHandler()->addToInspector( this );
}

void BaseComponent::deselectComponent()
{
    ScoreComponent::deselectComponent();
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
        Rectangle<int> compBounds = getSubcomponentByIndex(i)->getBounds();
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
        Rectangle<int> compBounds = getSubcomponentByIndex(i)->getBounds();
        minx =  min( minx, compBounds.getX() );
        miny =  min( miny, compBounds.getY() );
        maxx =  max( maxx, compBounds.getRight() );
        maxy =  max( maxy, compBounds.getBottom() );
    }
    setBounds(minx, miny, maxx-minx, maxy-miny);
    for( int i = 0; i < getNumSubcomponents(); i++)
    {
        SymbolistComponent* subcomp = getSubcomponentByIndex(i);
        subcomp->setTopLeftPosition( subcomp->getX()-getX(), subcomp->getY()-getY() );
    }
}

// Maximize I think is the same for everyone...
void BaseComponent::setMaximalBounds ()
{
    for ( int i  = 0; i < getNumSubcomponents(); i++ )
    {
        SymbolistComponent* subcomp = getSubcomponentByIndex(i);
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
void BaseComponent::scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio)
{
    DEBUG_FULL(getSymbolTypeStr() << " " << this << " " << scaledWidthRatio << " " << scaledHeightRatio << endl);
    for ( int i = 0; i < getNumSubcomponents(); i++ )
    {
        BaseComponent* c = (BaseComponent*) getSubcomponentByIndex(i);
        c->setTopLeftPosition(c->getX() * scaledWidthRatio, c->getY() * scaledHeightRatio);
        c->scaleScoreComponent(scaledWidthRatio, scaledHeightRatio);
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
        BaseComponent* c = (BaseComponent*) getSubcomponentByIndex(i);
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
    // std::cout << "BaseComponent::mouseMove" << std::endl;
}

void BaseComponent::mouseDown( const MouseEvent& event )
{
    m_down = event.position;

    if ( in_edit_mode || (isTopLevelComponent() && getMainMouseMode() == UI_EditType::DRAW) )
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
    
    if ( ! isSelected() )
        parent->addToSelection(this);
	
	PageComponent* scoreView = getPageComponent();
	
	/*
	 * If the component is attached to the score view,
	 * then makes a copy.
	 */
	if (scoreView != NULL)
	{
		scoreView->copySelectedToClipBoard();
		
    	parent->unselectAllComponents();
    	scoreView->newFromClipBoard();
	}
    
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
    if ( in_edit_mode )
    {
        g.setColour( Colour::fromFloatRGBA(1.0f, 1.0f, 1.0f, 0.9f)  );
        g.fillRect( getLocalBounds() );
    }
}

