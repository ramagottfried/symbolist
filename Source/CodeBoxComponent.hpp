#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolistHandler.h"
#include "CodeBoxController.hpp"
#include "View.hpp"

/**
 * Describes the code box view (code text editor), used to write and associate
 * odot expressions to a symbol.
 */
class CodeBoxComponent : public virtual Component,
						 public virtual CodeDocument::Listener,
						 public virtual Button::Listener,
						 public virtual View<SymbolistModel, CodeBoxController>
{

public:
	
	/*****************************
	 *       CONSTRUCTORS		 *
     *****************************/
    CodeBoxComponent();
    ~CodeBoxComponent();
	
    /*********************************
	 *       GETTERS & SETTERS		 *
     *********************************/
	inline CodeEditorComponent* getCodeEditor() { return code_editor.get(); }
	inline CodeDocument*		getCodeDocument() { return &code_document; }
	const  string               getExpr();
	inline bool					isContentSaved() { return content_saved; }
	
	/**
	 * Checks if the attached symbol component posseses an /expr odot message.
	 *
	 * @return <code>true</code>, if the symbol component attached to this CodeBoxComponent
	 *		   posseses a /expr odot message. <code>false</code> otherwise, or if the symbol
	 *		   component pointer is <code>NULL</code>, or if the symbol attached to the
	 *		   symbol component is <code>NULL</code>.
	 */
	bool hasExpr();
	
	/**
	 * Sets the symbol component attached to this CodeBoxComponent.
	 * If the symbol (attached to the symbol component) possesses an /expr odot message,
	 * then its content is displayed in the code editor. Otherwise, the code editor
	 * is blurred and set in read only mode.
	 *
	 * @param symbolComponent the symbol component to be attached to this CodeBoxComponent.
	 *
	 **/
    void setSymbolComponent(BaseComponent* symbolComponent);
	
	/*********************************
	 *       LISTENER CALLBACKS		 *
     *********************************/
	void codeDocumentTextInserted(const String &newText, int insertIndex) override;
	void codeDocumentTextDeleted(int startIndex, int endIndex) override;
	void buttonClicked(Button* button) override;
	
	/**********************
	 *       LAYOUT       *
	 **********************/
	void resized() override;
	
	/* Overrides the update method inherited from the Observer class. */
    inline void update() override { }
	
private:
    CodeDocument                        code_document;
    ScopedPointer<CodeEditorComponent>  code_editor;
	
    TextButton							save_button;
	bool 								content_saved = false;
	
    TextButton							hide_button;
    BaseComponent* 						symbol_component = NULL;
	
    /*********************************
	 *        LAYOUT PROPERTIES      *
	 *********************************/
	int button_height = 30;
	
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CodeBoxComponent)
};






