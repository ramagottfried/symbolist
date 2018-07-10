#ifndef ScoreComponent_h
#define ScoreComponent_h

#include "JuceHeader.h"
#include "SymbolistComponent.h"

class EditSelectionBox;

class SymbolistLasso : public Component {
    
public:
    
    void paint(Graphics &g) override;
    void begin(int x, int y);
    void update(int x, int y);
    void end();
    
private:
    
    float start_x, start_y;
};

/**
 * Superclass for score-editable components.
 * The score-editable components are either of type PageComponent, representing the score view, or
 * of type BaseComponent, representing the symbols in the score.
 *
 * For now, all ScoreComponent instances hold a list of sub-components.
 * In the future, only composite instances of ScoreComponent will possess
 * such a list.
 */
class ScoreComponent : public SymbolistComponent {
	
public:
    
    ScoreComponent();
    ScoreComponent(ScoreComponent* componentToCopy);
    ~ScoreComponent();
	
	/************************************
	 *          GETTERS & SETTERS       *
	 ************************************/
	inline bool isSelected() { return is_selected; }
	inline Array<ScoreComponent* >& getSelectedItems() { return selected_components; }
	inline Array<ScoreComponent* >* getSubcomponents() { return &subcomponents; }
	inline EditSelectionBox* getEditSelectionBox() { return selection_box.get(); }
	
	/****************************************
	 *          SUBCOMPONENTS METHODS       *
	 ****************************************/
	const size_t getNumSubcomponents();
	ScoreComponent* getSubcomponentByIndex(int i);
    ScoreComponent* getSubcomponentByID(const string& id);
	
	virtual void addSubcomponent( ScoreComponent *c );
	virtual void removeSubcomponent( ScoreComponent *c );
    void clearAllSubcomponents();
	
	/***********************************************
	 *          SELECTED COMPONENTS METHODS        *
	 ***********************************************/
	virtual void selectComponent();
	virtual void deselectComponent();

    void addToSelection(ScoreComponent* c);
    void removeFromSelection(ScoreComponent* c);
	
    void selectAllComponents();
    virtual void unselectAllComponents();
	virtual void deleteSelectedComponents();
	void translateSelectedComponents( Point<int> delta_xy );
	void reportModificationForSelectedSymbols();
	
    void selectedToFront();
    void selectedToBack();
	
    /* Method about grouping and ungrouping
	 * shouldn't be here because simple components
	 * (which are not a composition of components) don't
	 * care about these methods.
     */
    virtual void groupSelectedSymbols();
    void ungroupSelectedSymbols();
        
    void flipSelectedSymbols( int axis );
    void nudgeSelected( int direction );
	
	/*********************************************
	 *          GRAPHIC RENDERING METHODS        *
	 *********************************************/
	virtual inline void h_flip(float ax, float ay) {}
    virtual inline void v_flip(float ax, float ay) {}
	
    /**
     * Scales this instance of ScoreComponent in width and height
     * according to the two given ratios.
     *
     * @param scaledWidthRatio the ratio by which multiplying the current component width
     *				           to obtain the scaled width.
     *
     * @param scaledHeightRatio the ratio by which multiplying the current component height
     *				           to obtain the scaled height.
     */
	virtual inline void scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio) {}
    virtual inline void rotateScoreComponent(float theta, float ax, float ay) {}
	virtual inline void setScoreComponentSize(int w, int h) {}
	virtual inline void updateRelativeAttributes() {}
	
	Point<int> positionRelativeTo(ScoreComponent* to);
	virtual bool intersectRect(Rectangle<int> rect);
	Rectangle<int> getSelectionBounds();
	
	/****************************************
	 *          MOUSE EVENTS METHODS        *
	 ****************************************/
	void mouseDownSelection(const MouseEvent& event);
    virtual void mouseDown ( const MouseEvent& event ) override;
    virtual void mouseDrag ( const MouseEvent& event ) override;
    virtual void mouseUp ( const MouseEvent& event ) override;
    
    virtual void mouseAddClick ( const MouseEvent& event );
	
protected:
	
	ScopedPointer<EditSelectionBox > selection_box;
	SymbolistLasso                   s_lasso;
	
	bool                             is_selected = false;
	Array<ScoreComponent* >          subcomponents;
    Array<ScoreComponent* >          selected_components;
	
    void beginLassoSelection(Point<int> position);
    void dragLassoSelection(Point<int> position);
    void endLassoSelection();
	
};

#endif
