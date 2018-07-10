#include "EditSelectionBox.h"
#include "SymbolistMainComponent.h"
#include "ScoreComponent.h"

//==============================================================================
EditSelectionBox::EditSelectionBox ( Array<ScoreComponent*>* const selected_component_array ) : border_size (5), mouse_zone (0)
{
    component_set = selected_component_array;
}

EditSelectionBox::~EditSelectionBox()
{
    non_preview_components.clear();
}

//==============================================================================
void EditSelectionBox::paint (Graphics& g)
{
    auto mainComponent = dynamic_cast<SymbolistComponent*>(getParentComponent())->getMainComponent();
    
    // Checks downcast result and alt modifier key status.
    if( mainComponent != NULL && mainComponent->getCurrentMods()->isAltDown() )
        getLookAndFeel().drawResizableFrame (g, getWidth(), getHeight(), border_size);
    else
        getLookAndFeel().drawResizableFrame (g, getWidth(), getHeight(), border_size);

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

/***************************
 *      MOUSE CALLBACKS    *
 ***************************/
void EditSelectionBox::mouseEnter (const MouseEvent& e)
{
    updateMouseZone (e);
}

void EditSelectionBox::mouseMove (const MouseEvent& e)
{
    updateMouseZone (e);
}

void EditSelectionBox::mouseDown (const MouseEvent& e)
{
    createPreviewComponents(e);
}

void EditSelectionBox::mouseDrag (const MouseEvent& e)
{
	/*
	 * Calls mouseDrag on all the non-BaseComponent components
	 * selected by this EditSelectionBox instance.
	 */
    for( int i = 0; i < non_preview_components.size(); i++ )
        non_preview_components[i]->mouseDrag( e );
	
    bool inEditMode = (*component_set)[0]->getPageComponent()->getDisplayMode() == PageComponent::DisplayMode::EDIT;
    if (preview_components.size() == 0 || inEditMode )
        return;
	
    Point<int> mouseDelta = e.getPosition() - previous_position;
    previous_position = e.getPosition();
	
    /* Rotates the selected components when hitting "alt" on dragging. */
    if( e.mods.isAltDown() )
    	rotatePreviewComponents(e.getPosition());
    else
    	resizePreviewComponents(mouseDelta);
}

void EditSelectionBox::mouseUp (const MouseEvent& e)
{
    DEBUG_FULL("\n\nEditSelectionBox::mouseUp\n\n" << endl)
	
	/*
	 * Calls mouseUp on all the non-BaseComponent components
	 * selected by this EditSelectionBox instance.
	 */
    for( int i = 0; i < non_preview_components.size(); i++ )
        non_preview_components[i]->mouseUp( e );
	
    non_preview_components.clear();
    if (preview_components.size() == 0)
        return;
	
    printRect(original_bounds, "Original edit box");
    printRect(getBounds(), "Scaled edit box");

    if( e.mods.isAltDown() ) // drag + alt = rotate
        rotateComponentSet();
    else
		resizeComponentSet();
	
    setBounds( getSelectionBounds() );
	
    deleteAllChildren();
    preview_components.clear();
	
    previous_theta = -111;
    accumulation_theta = 0;
    scale_width = 1;
    scale_height = 1;
	
    DEBUG_FULL("---- end EditSelectionBox::mouseUp\n\n" << endl)

}

/*************************************
 *       ROTATE & RESIZE METHODS     *
 *************************************/
void EditSelectionBox::flipSelectedSymbols( int axis )
{
    auto center = getBounds().getCentre();
    for ( auto c : *component_set )
    {
        if( axis == 0)
            c->v_flip( center.getX(), center.getY() );
        else
            c->h_flip( center.getX(), center.getY() );
    }
    
}

void EditSelectionBox::rotatePreviewComponents( Point<int> mousePosition )
{
	auto centre = original_bounds.getCentre();
	auto delta = getPosition() + mousePosition - centre;
	auto dx = delta.getX(), dy = delta.getY();
	auto theta = atan2(dy, dx) - float_Pi;

	if( previous_theta == -111 )
		previous_theta = theta;

	auto deltaRad = theta - previous_theta;
	previous_theta = theta;
	accumulation_theta += deltaRad;

	/* Rotates offsets internally for it's current position, but since these are sub-components
	 * we need to set the rotation point in terms relative to the parent bounds.
	 */
	auto centreOffset = centre - getPosition();

	for( int i = 0; i < preview_components.size(); i++ )
	{
		BaseComponent *b = preview_components[i]->copy;
		b->rotateScoreComponent( deltaRad, centreOffset.getX(), centreOffset.getY() );

		/*
	  	 * This was experimental version where I returned the unapplied bounds of the rotated path for the preview,
	  	 * the tranform is faster than actually rotating, but it's more complicated, and has some ugly side effects when scaling.
	     *
	     * b->setTransform( AffineTransform::rotation(theta, relpt.getX(), relpt.getY() ) );
	     * b->setBounds( c->rotateScoreComponent( previous_theta, centre.getX(), centre.getY(), false ) );
	     */
	}

	auto pbounds = getPreviewBounds();
	setBounds( pbounds + getPosition() );

	for( int i = 0; i < preview_components.size(); i++ )
	{
		BaseComponent* b = preview_components[i]->copy;
		b->setTopLeftPosition( b->getPosition() - pbounds.getPosition() );
	}

}

void EditSelectionBox::resizePreviewComponents( Point<int> mouseDelta )
{
	// To do: add flips for when the resize box changes directions (dragging right edge over the left edge, etc.)
	if( mouseDelta.isOrigin() )
		return;

	const Rectangle<int> scaledBounds = mouse_zone.resizeRectangleBy( getBounds(), mouseDelta );
	scale_width = (float)scaledBounds.getWidth() / (float)original_bounds.getWidth();
	scale_height = (float)scaledBounds.getHeight() / (float)original_bounds.getHeight();

	float relativeScaleWidth = (float)scaledBounds.getWidth() / (float)getWidth();
	float relativeScaleHeight = (float)scaledBounds.getHeight() / (float)getHeight();

	printPoint(mouseDelta, "Mouse delta");
	DEBUG_FULL(relativeScaleWidth << " " << relativeScaleHeight << endl);

	setBounds( scaledBounds ); // << doing this before resize for a reason?

	printRect(getBounds(), "Edit box bounds");
	float relativeX, relativeY;

	if (getWidth() > min_width && getHeight() > min_height )
	{
		for( int i = 0; i < preview_components.size(); i++ )
		{
			BaseComponent *b = preview_components[i]->copy;
			BaseComponent *c = preview_components[i]->original;

			/* TEMPORARILY DISABLING PREVIEW SCALING FOR GROUPS AND STAFFS */
			if( c->getScoreSymbol()->getType() == "group" || c->getScoreSymbol()->getType() == "staff" )
				continue;
			
			DEBUG_FULL("Relative width = " << (float)c->getWidth() / original_bounds.getWidth() << endl)
			
			/* This is the current relative info for this component.
			 * If there is only one component the original bounds should be the same */
			relativeX = (float)(c->getX() - original_bounds.getX());
			relativeY = (float)(c->getY() - original_bounds.getY());
			
			// set top left for each as per it's original scale ratio
			b->setTopLeftPosition(relativeX * scale_width, relativeY * scale_height);
			b->setScoreComponentSize(c->getWidth(), c->getHeight()); // <<< this is maybe a problem
			/// ^^ might be that setScoreComponentSize doesn't scale groups correctly
			
			b->scaleScoreComponent(scale_width, scale_height);
			
			DEBUG_FULL("Post relative width = " << (float)b->getWidth() / getWidth() << endl)

		}
	}
}

void EditSelectionBox::createPreviewComponents( const MouseEvent& e )
{
	// Returns if component set is empty.
	if ( component_set->size() == 0 )
        return;
	
    ScoreComponent* first = (*component_set)[0];
	
    bool inEditMode = 0;
    if( first != NULL && first->getPageComponent() != NULL )
        inEditMode = (first->getPageComponent()->getDisplayMode() == PageComponent::DisplayMode::EDIT);
	
    updateMouseZone (e);
	
	// Stores the current position and bounds of this EditSelectionBox instance.
    previous_position = e.getPosition();
    original_bounds = getBounds();
	
    SymbolistHandler* mainController = first->getSymbolistHandler();
	
    /* Creates preview components based on
     * the current set of selected components.
	 */
    for( int i = 0; i < component_set->size(); i++ )
    {
        ScoreComponent* selectedComponent = (*component_set)[i];
        BaseComponent* castComponent = dynamic_cast<BaseComponent*>( selectedComponent );
		
		// Checks downcasting result.
        if( castComponent == NULL )
        {
            non_preview_components.add( selectedComponent );
            selectedComponent->mouseDown( e );
            continue;
        }
		
        // Copies castComponent to create a preview if not in edit mode.
        else if( !inEditMode )
        {
            Symbol* s = new Symbol();
			
            castComponent->addSymbolMessages(s);
            original_symbols.set(i, s);
			
            BaseComponent* newComponent = mainController->makeComponentFromSymbol(s, false);
			
            if (newComponent != NULL)
            {
				newComponent->setTopLeftPosition( castComponent->getPosition() - getPosition() );
				newComponent->setSize( castComponent->getWidth(), castComponent->getHeight() );
				newComponent->setSymbolComponentColor(Colours::red);
				
				preview_components.set(i, new PreviewComponent(newComponent, castComponent));
				addAndMakeVisible( newComponent );
			}
			
        }
		
    }
}

void EditSelectionBox::rotateComponentSet()
{
	auto centre = original_bounds.getCentre();
	for( auto it = preview_components.begin(); it != preview_components.end(); it++ )
	{
		BaseComponent* c = (*it)->original;
		
		// rotateScoreComponent uses the coordinate system of the parent.
		c->rotateScoreComponent( accumulation_theta, centre.getX(), centre.getY() );
	}
}

void EditSelectionBox::resizeComponentSet()
{
	if (getWidth() > min_width && getHeight() > min_height )
	{
		float scale_w = (float)getWidth() / (float)original_bounds.getWidth();
		float scale_h = (float)getHeight() / (float)original_bounds.getHeight();

		for( auto it = preview_components.begin(); it != preview_components.end(); it++ )
		{
			
			BaseComponent* c = (*it)->original;
			
			// this is the current relative info for this component
			float relative_x = (float)(c->getX() - original_bounds.getX());
			float relative_y = (float)(c->getY() - original_bounds.getY());
			
			c->setTopLeftPosition(getX() + relative_x * scale_width, getY() + relative_y * scale_height);
			c->scaleScoreComponent(scale_w, scale_h);
			
			Component::Positioner* const pos = c->getPositioner();
			if (pos)
			{
				DEBUG_FULL("Positioner not supported, let Rama know if you see this message printed." << endl)
			}
			
		}
	}
}

/*******************************
 *       GETTERS & SETTERS     *
 *******************************/
 
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
        Rectangle<int> compBounds = (*it)->copy->getBounds();
        minx =  min( minx, compBounds.getX() );
        miny =  min( miny, compBounds.getY() );
        maxx =  max( maxx, compBounds.getRight() );
        maxy =  max( maxy, compBounds.getBottom() );
    }
    return Rectangle<int>(minx, miny, maxx-minx, maxy-miny);
}

bool EditSelectionBox::hitTest (int x, int y)
{
    return x < border_size.getLeft()
    || x >= getWidth() - border_size.getRight()
    || y < border_size.getTop()
    || y >= getHeight() - border_size.getBottom();
}

void EditSelectionBox::setBorderThickness (const BorderSize<int>& newBorderSize)
{
    if (border_size != newBorderSize)
    {
        border_size = newBorderSize;
        repaint();
    }
}

BorderSize<int> EditSelectionBox::getBorderThickness() const
{
    return border_size;
}

void EditSelectionBox::updateMouseZone (const MouseEvent& e)
{
    Zone newZone (Zone::fromPositionOnBorder (getLocalBounds(), border_size, e.getPosition()));
    
    if (mouse_zone != newZone)
    {
        bool in_edit_mode = 0;
        
        if( component_set->size() > 0 )
            in_edit_mode = (*component_set)[0]->getPageComponent()->getDisplayMode() == PageComponent::DisplayMode::EDIT;

        mouse_zone = newZone;

        if( in_edit_mode )
        {
            setMouseCursor( MouseCursor::NormalCursor );
        }
        else
        {
            setMouseCursor (newZone.getMouseCursor());
        }
    }
}
