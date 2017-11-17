
#include "EditSelectionBox.h"
#include "SymbolistMainComponent.h"

EditSelectionBox::Zone::Zone() noexcept
: zone (0)
{}

EditSelectionBox::Zone::Zone (const int zoneFlags) noexcept
: zone (zoneFlags)
{}

EditSelectionBox::Zone::Zone (const EditSelectionBox::Zone& other) noexcept
: zone (other.zone)
{}

EditSelectionBox::Zone& EditSelectionBox::Zone::operator= (const EditSelectionBox::Zone& other) noexcept
{
    zone = other.zone;
    return *this;
}

bool EditSelectionBox::Zone::operator== (const EditSelectionBox::Zone& other) const noexcept      { return zone == other.zone; }
bool EditSelectionBox::Zone::operator!= (const EditSelectionBox::Zone& other) const noexcept      { return zone != other.zone; }

EditSelectionBox::Zone EditSelectionBox::Zone::fromPositionOnBorder (const Rectangle<int>& totalSize,
                                                                                     const BorderSize<int>& border,
                                                                                     Point<int> position)
{
    int z = 0;
    
    if (totalSize.contains (position)
        && ! border.subtractedFrom (totalSize).contains (position))
    {
        const int minW = jmax (totalSize.getWidth() / 10, jmin (10, totalSize.getWidth() / 3));
        if (position.x < jmax (border.getLeft(), minW) && border.getLeft() > 0)
            z |= left;
        else if (position.x >= totalSize.getWidth() - jmax (border.getRight(), minW) && border.getRight() > 0)
            z |= right;
        
        const int minH = jmax (totalSize.getHeight() / 10, jmin (10, totalSize.getHeight() / 3));
        if (position.y < jmax (border.getTop(), minH) && border.getTop() > 0)
            z |= top;
        else if (position.y >= totalSize.getHeight() - jmax (border.getBottom(), minH) && border.getBottom() > 0)
            z |= bottom;
    }
    
    return Zone (z);
}

MouseCursor EditSelectionBox::Zone::getMouseCursor() const noexcept
{
    MouseCursor::StandardCursorType mc = MouseCursor::NormalCursor;
    
    switch (zone)
    {
        case (left | top):      mc = MouseCursor::TopLeftCornerResizeCursor; break;
        case top:               mc = MouseCursor::TopEdgeResizeCursor; break;
        case (right | top):     mc = MouseCursor::TopRightCornerResizeCursor; break;
        case left:              mc = MouseCursor::LeftEdgeResizeCursor; break;
        case right:             mc = MouseCursor::RightEdgeResizeCursor; break;
        case (left | bottom):   mc = MouseCursor::BottomLeftCornerResizeCursor; break;
        case bottom:            mc = MouseCursor::BottomEdgeResizeCursor; break;
        case (right | bottom):  mc = MouseCursor::BottomRightCornerResizeCursor; break;
        default:                break;
    }
    
    return mc;
}

//==============================================================================
EditSelectionBox::EditSelectionBox ( Array<SymbolistComponent*>* const selected_component_array ) :
borderSize (5), mouseZone (0)
{
    component_set = selected_component_array;
}

EditSelectionBox::~EditSelectionBox()
{
}

//==============================================================================
void EditSelectionBox::paint (Graphics& g)
{
    auto mc = ((SymbolistComponent*)getParentComponent())->getMainComponent();
    if( mc && mc->getCurrentMods()->isAltDown() )
    {
        //g.drawLine(prev_pos.getX(), prev_pos.getY(), getWidth() / 2, getWidth() / 2);
        getLookAndFeel().drawResizableFrame (g, getWidth(), getHeight(), borderSize);
    }
    else
    {
        getLookAndFeel().drawResizableFrame (g, getWidth(), getHeight(), borderSize);
    }
    
    // use palette technique to draw copy of symbols
    /*
    //g.setColour( Colours::white.withAlpha(0.7f) );
    //g.fillRect( getParentComponent()->getLocalBounds() );
    for ( auto c : *component_set )
    {
        //g.saveState();

        g.setColour( Colours::red );
        float relative_x = (float)(c->getX() - original_bounds.getX());
        float relative_y = (float)(c->getY() - original_bounds.getY());
       // g.setOrigin(relative_x * m_scale_w, relative_y * m_scale_h);
        g.drawRect(relative_x * m_scale_w, relative_y * m_scale_h, c->getWidth() * m_scale_w, c->getHeight() * m_scale_w);
        
        //g.addTransform( AffineTransform::scale(m_scale_w, m_scale_h));
        //c->setPreview(true);
        //c->paint(g);
        //c->setPreview(false);
        //c->repaint();

        //g.restoreState();
    }
     */
}

void EditSelectionBox::updateEditSelBox()
{
    if( component_set->size() == 0 )
    {
        setVisible(false);
    }
    else
    {
        setVisible(true);
        setBounds( getSelectionBounds() );
    }
}

void EditSelectionBox::mouseEnter (const MouseEvent& e)
{
    updateMouseZone (e);
}

void EditSelectionBox::mouseMove (const MouseEvent& e)
{
    updateMouseZone (e);
}

void EditSelectionBox::flipSelectedSymbols( int axis )
{
    auto center = getBounds().getCentre();
    for ( auto c : *component_set )
    {
        if( axis == 0)
            ((BaseComponent*)c)->v_flip( center.getX(), center.getY() );
        else
            ((BaseComponent*)c)->h_flip( center.getX(), center.getY() );
    }
    
}


Rectangle<int> EditSelectionBox::getSelectionBounds()
{
    // get the position an bounds of the group
    int minx = getParentWidth(), maxx = 0, miny = getParentHeight(), maxy = 0;
    for( auto it = component_set->begin(); it != component_set->end(); it++ )
    {
        Rectangle<int> compBounds = (*it)->getBounds();
        minx =  min( minx, compBounds.getX() );
        miny =  min( miny, compBounds.getY() );
        maxx =  max( maxx, compBounds.getRight() );
        maxy =  max( maxy, compBounds.getBottom() );
    }
    return Rectangle<int>(minx, miny, maxx-minx, maxy-miny);
}


Rectangle<int> EditSelectionBox::getPreviewBounds()
{
    // get the position an bounds of the group
    int minx = getParentWidth(), maxx = 0, miny = getParentHeight(), maxy = 0;
    for( auto it = preview_components.begin(); it != preview_components.end(); it++ )
    {
        Rectangle<int> compBounds = (*it)->getBounds();
        minx =  min( minx, compBounds.getX() );
        miny =  min( miny, compBounds.getY() );
        maxx =  max( maxx, compBounds.getRight() );
        maxy =  max( maxy, compBounds.getBottom() );
    }
    return Rectangle<int>(minx, miny, maxx-minx, maxy-miny);
}

void EditSelectionBox::mouseDown (const MouseEvent& e)
{
    if (component_set->size() == 0)
    {
        return;
    }
    updateMouseZone (e);
    prev_pos = e.getPosition();
    
    original_bounds = getBounds();

    SymbolistHandler *sh = (*component_set)[0]->getSymbolistHandler();
    
    for( int i = 0; i < component_set->size(); i++ )
    {
        BaseComponent* b = (BaseComponent*)(*component_set)[i];
        Symbol* s = new Symbol();
        //*b->getScoreSymbolPointer()
        b->addSymbolMessages(s, "");
        original_symbols.set(i, s);
        
        BaseComponent *newB = sh->makeComponentFromSymbol(s, false);
        newB->setTopLeftPosition( b->getPosition() - getPosition() );
        newB->setSize( b->getWidth(), b->getHeight() );
        newB->setSymbolComponentColor(Colours::red);
        preview_components.set(i, newB);
        addAndMakeVisible( newB );
    }
    
    
    /*
    if( e.mods.isAltDown() )
    {
        cout << "check" << endl;
        auto centre = getBounds().getCentre();
        for( auto it = component_set->begin(); it != component_set->end(); it++ )
        {
            (*it)->rotateScoreComponent( 0.1, centre.getX(), centre.getY() );
        }
        setBounds( getSelectionBounds() );

    }
    */
}

void EditSelectionBox::mouseDrag (const MouseEvent& e)
{
    if (component_set->size() == 0)
    {
        return;
    }
    
    
    /*
    if( e.mods.isShiftDown() )
    {
        auto xovery = (double)getWidth() / (double)getHeight();
        constrainer.setFixedAspectRatio( xovery );
    }
    else
    {
        constrainer.setFixedAspectRatio( 0.0 );
    }
    */
    
    Point<int> mouse_delta = e.getPosition() - prev_pos;
    
    prev_pos = e.getPosition();
    
    if( e.mods.isAltDown() ) // drag + alt = rotate
    {
        
        auto centre = original_bounds.getCentre();
        auto delta = getPosition() + e.getPosition() - centre;
        //printPoint(delta, "delta");

        auto dx = delta.getX(), dy = delta.getY();
        
       // auto dist = sqrt( dx*dx + dy*dy );
        auto theta = atan2(dy, dx) - float_Pi;
        
        if( m_prev_theta == -111 )
            m_prev_theta = theta;
        
        auto delta_rad = theta - m_prev_theta;
        m_prev_theta = theta;
        m_accum_theta += delta_rad;
        
        
        
        // rotation preview needs work, for now just rotating the real thing
        
        for( auto it = component_set->begin(); it != component_set->end(); it++ )
        {
            (*it)->rotateScoreComponent( delta_rad, centre.getX(), centre.getY() );
        }
        
        for( int i = 0; i < preview_components.size(); i++ )
        {
            SymbolistComponent *b = preview_components[i];
            //SymbolistComponent *c = (*component_set)[i];
            
            auto relpt = original_bounds.getCentre() - original_bounds.getPosition();
            
            b->rotateScoreComponent( delta_rad, relpt.getX() - b->getX(), relpt.getY() - b->getY() );
        }
        
        auto pbounds = getPreviewBounds();
        setCentrePosition( centre );
        setSize( pbounds.getWidth(), pbounds.getWidth() );
        
        
    }
    else
    {
        const Rectangle<int> scaledBounds = mouseZone.resizeRectangleBy( getBounds(), mouse_delta );
        m_scale_w = (float)scaledBounds.getWidth() / (float)original_bounds.getWidth();
        m_scale_h = (float)scaledBounds.getHeight() / (float)original_bounds.getHeight();
        
        setBounds( scaledBounds );

        float relative_x, relative_y;
        
        if (getWidth() > m_minw && getHeight() > m_minh )
        {
            
            for( int i = 0; i < component_set->size(); i++ )
            {
                SymbolistComponent *c = (*component_set)[i];
                SymbolistComponent *b = preview_components[i];
                
                // this is the current relative info for this component
                // if there is only one component the original bounds should be the same
                relative_x = (float)(c->getX() - original_bounds.getX());
                relative_y = (float)(c->getY() - original_bounds.getY());
                
                b->setTopLeftPosition(relative_x * m_scale_w, relative_y * m_scale_h);
                b->setScoreComponentSize(c->getWidth(), c->getHeight());
                b->scaleScoreComponent(m_scale_w, m_scale_h);
                
            }
        }
    }
}

void EditSelectionBox::mouseUp (const MouseEvent& e)
{
    cout << "\n\nEditSelectionBox::mouseUp\n\n" << endl;
    printRect(original_bounds, "original edit box");
    printRect(getBounds(), "scaled edit box");

    if( e.mods.isAltDown() ) // drag + alt = rotate
    {
        // in the future, I'd like to have the preview be separate from the mouse up actual symbol manipulation
        // but for now, I'm just spinning the object directly
        /*
        auto centre = original_bounds.getCentre();
        for( auto it = component_set->begin(); it != component_set->end(); it++ )
        {
            (*it)->rotateScoreComponent( m_accum_theta, centre.getX(), centre.getY() );
        }*/
    }
    else
    {

        if (getWidth() > m_minw && getHeight() > m_minh )
        {
            float scale_w = (float)getWidth() / (float)original_bounds.getWidth();
            float scale_h = (float)getHeight() / (float)original_bounds.getHeight();

            
            for( auto it = component_set->begin(); it != component_set->end(); it++ )
            {
                
                SymbolistComponent *c = (*it);
                
                // this is the current relative info for this component
                float relative_x = (float)(c->getX() - original_bounds.getX());
                float relative_y = (float)(c->getY() - original_bounds.getY());
                
                // ratio of current w/h to scaled selected w/h
                // relative_w = (float)c->getWidth() / (float)getWidth();
                // relative_h = (float)c->getHeight() / (float)getHeight();
                
                //printRect(c->getBounds(), "comp");
                //printRect(Rectangle<float>(relative_x, relative_y, relative_w, relative_h), "relative");
                
                
                int new_rel_x = roundToInt(relative_x * (float)getWidth());
                int new_rel_y = roundToInt(relative_y * (float)getHeight());
                // float new_w = roundToInt(relative_w * scale_w);
                // float new_h = roundToInt(relative_h * scale_h);
                
                
               // cout << relative_w << " " << new_w << " " << relative_h << " " << new_h << endl;
               // cout << "\t vs w " << (scale_w * c->getWidth()) << " h " << (scale_h * c->getHeight()) << endl;
                cout << "for comp " << c << " " << c->getSymbolTypeStr() << endl;
                printPoint(c->getPosition(), "current");
                cout << c << " relative_x " << relative_x << " new_rel_x " << new_rel_x << " new x " << c->getX() + new_rel_x << endl;
                cout << c << " relative_y " << relative_y << " new_rel_y " << new_rel_y << " new y " << c->getY() + new_rel_y << endl;
                
                c->setTopLeftPosition(getX() + relative_x * m_scale_w, getY() + relative_y * m_scale_h);
                c->scaleScoreComponent(scale_w, scale_h);
                
                Component::Positioner* const pos = c->getPositioner();
                if (pos)
                {
                    cout << "positioner not supported, let rama know if you see this message printed" << endl;
                }
                
            }
        }
    }
    
    removeAllChildren();
    preview_components.clear();
    
    m_prev_theta = -111;
    m_accum_theta = 0;
    m_scale_w = 1;
    m_scale_h = 1;
    
    cout << "---- end EditSelectionBox::mouseUp\n\n" << endl;

}

bool EditSelectionBox::hitTest (int x, int y)
{
    return x < borderSize.getLeft()
    || x >= getWidth() - borderSize.getRight()
    || y < borderSize.getTop()
    || y >= getHeight() - borderSize.getBottom();
}

void EditSelectionBox::setBorderThickness (const BorderSize<int>& newBorderSize)
{
    if (borderSize != newBorderSize)
    {
        borderSize = newBorderSize;
        repaint();
    }
}

BorderSize<int> EditSelectionBox::getBorderThickness() const
{
    return borderSize;
}

void EditSelectionBox::updateMouseZone (const MouseEvent& e)
{
    Zone newZone (Zone::fromPositionOnBorder (getLocalBounds(), borderSize, e.getPosition()));
    
    if (mouseZone != newZone)
    {
        mouseZone = newZone;
        setMouseCursor (newZone.getMouseCursor());
    }
}
