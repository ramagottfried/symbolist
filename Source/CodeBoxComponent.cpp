#include "CodeBoxComponent.hpp"
#include "BaseComponent.h"
#include "SymbolistMainComponent.h"

CodeBoxComponent::CodeBoxComponent()
{
	addAndMakeVisible( code_editor = new CodeEditorComponent (code_document, nullptr) );
	code_editor->setLineNumbersShown( true );
	Font newFont(code_editor->getFont());
	newFont.setHeight(14.0f);
	
	code_editor->setFont(newFont);
	code_document.addListener(this);
	
	addAndMakeVisible(save_button);
	save_button.setColour(TextButton::buttonColourId, Colours::grey);
	save_button.setButtonText("Save");
	save_button.addListener(this);
	
	if (content_saved) save_button.setEnabled(false);
	
	addAndMakeVisible(hide_button);
	hide_button.setColour(TextButton::buttonColourId, Colours::grey);
	hide_button.setButtonText("Hide");
	hide_button.addListener(this);
		
}

CodeBoxComponent::~CodeBoxComponent()
{
	code_document.removeListener(this);
	save_button.removeListener(this);
	hide_button.removeListener(this);
}

void CodeBoxComponent::codeDocumentTextInserted(const String &newText, int insertIndex)
{
	if (!save_button.isEnabled())
	{
		save_button.setEnabled(true);
		content_saved = false;
	}
	
}

void CodeBoxComponent::codeDocumentTextDeleted(int startIndex, int endIndex)
{
	if (!save_button.isEnabled())
	{
		save_button.setEnabled(true);
		content_saved = false;
	}
}

void CodeBoxComponent::buttonClicked(Button* button)
{
	if (button == NULL)
		return;
	
	/* Click on save button */
	if (button == &save_button)
	{
		/* Calls the controller associated with this CodeBoxComponent in order to
	 	 * write in the symbol attached to the currently inspected component.
	     */
		if (hasExpr())
			getController()->updateExprInSymbol(symbol_component->getScoreSymbol(), code_document.getAllContent().toStdString());

		// Disables the save button when clicked.
		save_button.setEnabled(false);
		content_saved = true;
	
	}
	
	/* Click on hide button */
	else if (button == &hide_button)
	{
		// If content not saved, save the content before hiding the code box.
		if (!content_saved && hasExpr())
		{
			getController()->updateExprInSymbol(symbol_component->getScoreSymbol(), code_document.getAllContent().toStdString());
			content_saved = true;
		}
		
		SymbolistMainComponent* mainView = dynamic_cast<SymbolistMainComponent*>(getParentComponent());
		if (mainView != NULL)
			mainView->toggleCodeBox();
		
	}
	
}

void CodeBoxComponent::setSymbolComponent(BaseComponent* symbolComponent)
{
	if (symbolComponent != NULL)
	{
		symbol_component = symbolComponent;
		Symbol* associatedSymbol = symbol_component->getScoreSymbol();
		
		/* Activates the code editor if the associated symbol contains the message /expr. */
		if (associatedSymbol != NULL && associatedSymbol->addressExists("/expr"))
		{
			code_editor->loadContent(String(associatedSymbol->getMessage("/expr").getString()));
			code_editor->setReadOnly(false);
			code_editor->setColour(CodeEditorComponent::backgroundColourId, Colours::black);
		}
		/* Blurs the code editor if no symbol or no associated odot expression. */
		else
		{
			code_document.applyChanges("no content.");
			code_editor->setColour(CodeEditorComponent::backgroundColourId, Colours::grey);
			code_editor->setReadOnly(true);
			save_button.setEnabled(false);

		}
	}
	/* Blurs the code editor if symbolComponent is NULL. */
	else
	{
		code_document.applyChanges("no content.");
		code_editor->setColour(CodeEditorComponent::backgroundColourId, Colours::grey);
		code_editor->setReadOnly(true);
		save_button.setEnabled(false);
	}
	
	repaint();
}

bool CodeBoxComponent::hasExpr()
{
	return symbol_component != NULL
			&& symbol_component->getScoreSymbol() != NULL
			&& symbol_component->getScoreSymbol()->get_o_ptr() != NULL
			&& symbol_component->getScoreSymbol()->addressExists("/expr");
}

const string CodeBoxComponent::getExpr()
{
	if (hasExpr())
		return symbol_component->getScoreSymbol()->getMessage("/expr").getString();
	else return "";
}

void CodeBoxComponent::resized()
{
	// Resizes save button and hide but.
	auto area =  getLocalBounds().reduced(8);
	area.removeFromLeft(area.getWidth() * 0.7);
	area.removeFromTop(area.getHeight() - button_height);
	hide_button.setBounds(area.removeFromLeft(getWidth() * 0.15).reduced(4));
	save_button.setBounds(area.removeFromLeft(getWidth() * 0.15).reduced(4));
	
	// Resets area to local bounds to resize code editor.
	area = getLocalBounds().reduced(8);
	code_editor->setBounds(area.withTrimmedTop (8));
}
