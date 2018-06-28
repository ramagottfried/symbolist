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
						 public virtual View<SymbolistModel, CodeBoxController>
{

public:
	
	/*****************************
	 *       CONSTRUCTORS		 *
     *****************************/
    CodeBoxComponent()
    {
        addAndMakeVisible( code_editor = new CodeEditorComponent (code_document, nullptr) );
        code_editor->setLineNumbersShown( true );
        Font newFont(code_editor->getFont());
        newFont.setHeight(14.0f);
		
        code_editor->setFont(newFont);
        code_document.addListener(this);
    }
    
    ~CodeBoxComponent()
    {
    	code_document.removeListener(this);
    }
	
    /*********************************
	 *       GETTERS & SETTERS		 *
     *********************************/
	inline CodeEditorComponent* getCodeEditor() { return code_editor.get(); }
	inline CodeDocument*		getCodeDocument() { return &code_document; }
	const  string               getExpr();
	
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
	
	/*************************************************
	 *       CODE DOCUMENT LISTENER CALLBACKS		 *
     *************************************************/
     void codeDocumentTextInserted(const String &newText, int insertIndex) override;
	 void codeDocumentTextDeleted(int startIndex, int endIndex) override;
	
	
	inline void resized() override
    {
        Rectangle<int> r (getLocalBounds().reduced (8));
        code_editor->setBounds(r.withTrimmedTop (8));
    }
	
	/* Overrides the update method inherited from the Observer class. */
    inline void update() override { }
	
private:
    CodeDocument                        code_document;
    ScopedPointer<CodeEditorComponent>  code_editor;
    BaseComponent* 						symbol_component = NULL;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (CodeBoxComponent)
};






