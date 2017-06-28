
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


void BaseComponent::initSymbolData()
{
    Symbol *s = getScoreSymbolPointer();
   
    auto b = symbol_export_bounds();
    int pos = -1;

    pos = s->getOSCMessagePos("/color");
    if( pos == -1 )
         s->addOSCMessage( OSCMessage( "/color", sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha() ) );
    
    
}


int BaseComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = 0;
    
    auto b = symbol_export_bounds();
    
    s->addOSCMessage ((String(base_address) += "/type") ,           getSymbolTypeStr());
    s->addOSCMessage ((String(base_address) += "/x") ,              b.getX() );
    s->addOSCMessage ((String(base_address) += "/y") ,              b.getY() );
    s->addOSCMessage ((String(base_address) += "/w") ,              b.getWidth() );
    s->addOSCMessage ((String(base_address) += "/h") ,              b.getHeight() );
    s->addOSCMessage ((String(base_address) += "/time/start") ,     s->pixelsToTime( b.getX() ) );
    s->addOSCMessage ((String(base_address) += "/time/duration"),   s->pixelsToTime( b.getWidth() ) );
    s->addOSCMessage (OSCMessage( (String(base_address) += "/color"),   sym_color.getFloatRed(), sym_color.getFloatGreen(), sym_color.getFloatBlue(), sym_color.getFloatAlpha()  ) );

    messages_added += 8;
    
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
            
            cout << "x " << x  << " y " << y << " w " << w << endl;
            
            setBoundsFromSymbol( x , y , w , h);
        }
        else
            cout << "***** couldn't find x y w or h values " << endl;
    
        
        int color_pos = s.getOSCMessagePos("/color");
        if( color_pos != -1  )
        {
            auto bndl = s.getOSCBundle();
            if( bndl[color_pos].getMessage().size() == 4 )
            {
                float r = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[0] );
                float g = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[1] );
                float b = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[2] );
                float a = Symbol::getOSCValueAsFloat( bndl[color_pos].getMessage()[3] );
                sym_color = Colour::fromFloatRGBA( r, g, b, a );
            }
        }
        else
        {
            
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
            mouseDownSelection(event);
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
                parent->translateSelectedComponents( (event.position - m_down).toInt() );
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

