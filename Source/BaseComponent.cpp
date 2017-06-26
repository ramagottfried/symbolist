
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
    if ( getParentComponent() != NULL && getParentComponent() == getPageComponent() ) {
        if ( score_symbol != NULL ) return true;
        else {
            std::cout << "Warning: BaseComponent is TopLevel but has no attached score symbol!" << std::endl ;
            return false;
        }
    } else return false;
}


// This is the function to call when we want to update the score after a modification
void BaseComponent::reportModification()
{
    if ( getParentComponent() != NULL ) // we're in the score..
    {
        if ( isTopLevelComponent() )
        {
            getSymbolistHandler()->modifySymbolInScore( this );
        }
        else
        {
            ((BaseComponent*) getParentComponent())->reportModification() ;
        }
    }
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
    
    s->addOSCMessage ((String(base_address) += "/type") ,   getSymbolTypeStr());
    s->addOSCMessage ((String(base_address) += "/x") ,      b.getX() );
    s->addOSCMessage ((String(base_address) += "/y") ,      b.getY() );
    s->addOSCMessage ((String(base_address) += "/w") ,      b.getWidth() );
    s->addOSCMessage ((String(base_address) += "/h") ,      b.getHeight() );
    s->addOSCMessage ((String(base_address) += "/time/start") , b.getX() * 0.01f );
    s->addOSCMessage ((String(base_address) += "/duration"),    b.getWidth() * 0.01f );
    
    messages_added += 7;
    
    cout << "*********** START BASE ADD DATA ************ " << endl;
    s->printBundle();
    
    return messages_added;
}


/******************
 * Imports components' data from the symbol's OSC bundle (can be overriden by sub-class)
 *****************/

void BaseComponent::importFromSymbol( const Symbol &s )
{
    int typeMessagePos = s.getOSCMessagePos("/type");
    
    if ( typeMessagePos == -1 ) {
        
        cout << "BaseComponent import: Could not find '/type' message in OSC Bundle.. (size=" << s.getOSCBundle().size() << ")" << endl;
        
    } else {
        
        String typeStr = s.getOSCMessageValue(typeMessagePos).getString();
        cout << "Importing BaseComponent from Symbol: " << typeStr << endl;
        
        float x = s.getOSCMessageValue("/x").getFloat32();
        float y = s.getOSCMessageValue("/y").getFloat32();
        float w = s.getOSCMessageValue("/w").getFloat32();
        float h = s.getOSCMessageValue("/h").getFloat32();
        
        cout << "x " << x  << " y " << y << " w " << w << endl;

        
        setBoundsFromSymbol( x , y , w , h);
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

void BaseComponent::setSeleted(bool val) { is_selected = val; }
bool BaseComponent::isSelected() { return is_selected; }

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
    if ( ! isTopLevelComponent() )  ((BaseComponent*) getParentComponent())->recursiveMaximizeBounds();
    setMaximalBounds();
}

void BaseComponent::recursiveShrinkBounds()
{
    setMinimalBounds();
    if ( ! isTopLevelComponent() ) ((BaseComponent*) getParentComponent())->recursiveShrinkBounds();
}

/************************
 * Component modification callbacks
 ************************/

void BaseComponent::moved ()
{
    //reportModification(); // shoudl be smarter : call this when the move is over (mouse up)
}

void BaseComponent::resized ()
{
    //reportModification(); // shoudl be smarter : call this when the move is over (mouse up)

    if( !resizableBorder ) // << probably better to initialize the resizable border somewhere else...
    {
        constrainer.setMinimumSize ( m_min_size, m_min_size );
        addChildComponent( resizableBorder = new ResizableBorderComponent(this, &constrainer) );
        resizableBorder->setBorderThickness( BorderSize<int>(4) );
    }
    resizableBorder->setBounds( getLocalBounds() );
}


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
    
    if ( in_edit_mode ) ScoreComponent::mouseDown(event);
    
    else {
        
        ScoreComponent* parent = ((ScoreComponent*)getParentComponent());
        
        if ( respondsToMouseEvents() )
        {
            if ( event.mods.isShiftDown() )
            {
                if ( isSelected() ) parent->removeFromSelection(this);
                else parent->addToSelection(this);
            } else {
                if ( ! isSelected() )
                {
                    parent->unselectAllComponents();
                    parent->addToSelection(this);
                }
            }
            
        } else {
            parent->mouseDown(event.getEventRelativeTo(parent));
        }
    }
}


void BaseComponent::mouseDrag( const MouseEvent& event )
{
    if ( in_edit_mode ) ScoreComponent::mouseDrag(event);
    
    else {
        
        ScoreComponent* parent = ( (ScoreComponent*) getParentComponent() );
        
        if ( respondsToMouseEvents() )
        {
            if( is_selected && (getMainEditMode() == selection ) )
            {
                parent->translateSelectedSymbols( (event.position - m_down).toInt() );
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
    if ( in_edit_mode ) ScoreComponent::mouseUp(event);
    
    else {
        
        if ( respondsToMouseEvents() )
        {
            if( is_selected && getMainEditMode() == selection )
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
        g.setColour( Colour::fromFloatRGBA(1.0f,1.0f,1.0f,0.7f)  );
        g.fillRect( getLocalBounds() );
        g.setColour( Colour::fromFloatRGBA(0.8f,0.8f,0.8f,0.5f)  );
        g.fillRect( getLocalBounds() );
    }
}

