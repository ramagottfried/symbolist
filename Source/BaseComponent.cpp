
#include "BaseComponent.h"
#include "PageComponent.h"
#include "ScoreComponent.h"
#include "SymbolistMainComponent.h"

// do we need this ?
template <typename T> void printPoint(Point<T> point, String name = "point" )
{
    std::cout << name << " " << point.getX() << " " << point.getY() << "\n";
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
            updateRelativeAttributes();
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
    addSymbolMessages( s , String("") );
    setScoreSymbolPointer( s );
}

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
    
    addr = base_address + "/id";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,          getComponentID() );
        messages_added++;
    }
    
    addr = base_address + "/staff";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         staff_name );
        messages_added++;
    }
    
    addr = base_address + "/objectType";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         object_type );
        messages_added++;
    }
    
    addr = base_address + "/type";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr,         getSymbolTypeStr());
        messages_added++;
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
    
    addr = base_address + "/color";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( OSCMessage( addr,   sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha()  ) );
        messages_added++;
    }
    
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
            cout << "***** couldn't find x y w or h values " << endl;
    
        
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
        {
            name = s.getOSCMessageValue(name_pos).getString();
        }
        
        int staffname_pos = s.getOSCMessagePos("/staff");
        if( staffname_pos != -1 )
        {
            staff_name = s.getOSCMessageValue(staffname_pos).getString();
        }
        
        int objtype_pos = s.getOSCMessagePos("/objectType");
        if( objtype_pos != -1 )
        {
            object_type = s.getOSCMessageValue(objtype_pos).getString();
        }

    }
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
    //resizableBorder->setVisible( true ); // this makes the resizable border apera also in multiple selection
    
    getSymbolistHandler()->addToInspector( this );
}

void BaseComponent::deselectComponent()
{
    resizableBorder->setVisible( false );
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

// We don't want this to happen while in edit_mode
bool BaseComponent::inPlaceForRelativeUpdates()
{
    return ( getParentComponent() // we have a parent
             && getParentComponent() != getPageComponent() // the parent is not the page
             && ! ((BaseComponent*)getParentComponent())->isInEditMode() // the parent is not being edited
             && ( getPageComponent() == NULL || getPageComponent()->getEditedComponent() ==  getPageComponent()) // the page component is being edited
            ) ;
}

// update the relative pos according to container
// call this after a move

void BaseComponent::updateRelativePos()
{
    Rectangle<int> container_bounds = getParentComponent()->getBounds();
    relative_x = getPosition().x / (float) (container_bounds.getWidth());
    relative_y = getPosition().y / (float) (container_bounds.getHeight());
}

// update the relative size according to container
// call this after a resize
void BaseComponent::updateRelativeSize()
{
    Rectangle<int> container_bounds = getParentComponent()->getBounds();
    relative_w = getWidth() / (float) (container_bounds.getWidth());
    relative_h = getHeight() / (float) (container_bounds.getHeight());
}

void BaseComponent::updateRelativeAttributes()
{
    updateRelativePos();
    updateRelativeSize();
    
    /*
    if( in_edit_mode )
    {
        // because we're overrideing the mouseClickAdd for editing, we end up adding extra subcomponents instead
        for ( int i = 0; i < getNumSubcomponents(); i++ )
        {
            ((BaseComponent*)getSubcomponent(i))->updateRelativeAttributes();
        }
    }
     */
}

void BaseComponent::resizeToFit(int x, int y, int w, int h)
{
    setBounds( x, y, w, h);
    updateSubcomponents();
}

// update the subcomponents according to their relative pos and size
// call this from container at resize
void BaseComponent::updateSubcomponents ()
{
    for ( int i = 0; i < getNumSubcomponents(); i++ )
    {
        BaseComponent* c = (BaseComponent*)getSubcomponent(i);
        
        c->setBounds(c->relative_x * getWidth(),
                     c->relative_y * getHeight(),
                     c->relative_w * getWidth(),
                     c->relative_h * getHeight());

        c->updateSubcomponents();
    }
}

void BaseComponent::addSubcomponent( SymbolistComponent *c )
{
    ScoreComponent::addSubcomponent( c );
    
    if ( ((BaseComponent*)c)->inPlaceForRelativeUpdates() )
    {
        ((BaseComponent*)c)->updateRelativePos();
        ((BaseComponent*)c)->updateRelativeSize();
    }
}


void BaseComponent::resized ()
{
    if ( getPageComponent() &&
         (     getPageComponent()->getEditedComponent() == getPageComponent()
            || getPageComponent()->getEditedComponent() == getParentComponent() ))
    {
            updateSubcomponents ();
    }
    
    if( !resizableBorder ) // << probably better to initialize the resizable border somewhere else...
    {
        constrainer.setMinimumSize ( m_min_size, m_min_size );
        addChildComponent( resizableBorder = new ResizableBorderComponent(this, &constrainer) );
        resizableBorder->setBorderThickness( BorderSize<int>(6) );
        resizableBorder->setAlwaysOnTop(true);
    }
    
    
    if( getMainComponent() && getMainComponent()->getCurrentMods()->isShiftDown() )
    {
        auto xovery = (double)resizableBorder->getWidth() / (double)resizableBorder->getHeight();
        constrainer.setFixedAspectRatio( xovery );
    }
    else
    {
        constrainer.setFixedAspectRatio( 0.0 );
    }
        
    resizableBorder->setBounds( getLocalBounds() );
    
    if( is_selected )
    {
        ((ScoreComponent*) getParentComponent())->reportModificationForSelectedSymbols();
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
        ScoreComponent::mouseDown( event );
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
                resizableBorder->setVisible( true ); // here instead of the select callback makes it only when the symbol is clicked alone
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

