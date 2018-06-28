#include "CodeBoxComponent.hpp"
#include "BaseComponent.h"

void CodeBoxComponent::codeDocumentTextInserted(const String &newText, int insertIndex)
{
	/* Calls the controller associated with this CodeBoxComponent in order to
	 * write in the symbol attached to the currently inspected component.
	 */
	if (hasExpr())
		getController()->updateExprInSymbol(symbol_component->getScoreSymbol(), code_document.getAllContent().toStdString());
	DEBUG_FULL("Text inserted in code box." << endl)
}

void CodeBoxComponent::codeDocumentTextDeleted(int startIndex, int endIndex)
{
	/* Calls the controller associated with this CodeBoxComponent in order to
	 * write in the symbol attached to the currently inspected component.
	 */
	if (hasExpr())
		getController()->updateExprInSymbol(symbol_component->getScoreSymbol(), code_document.getAllContent().toStdString());
	DEBUG_FULL("Text deleted in code box." << endl)
}

void CodeBoxComponent::setSymbolComponent(BaseComponent* symbolComponent)
{
	if (symbolComponent != NULL)
	{
		symbol_component = symbolComponent;
		Symbol* associatedSymbol = symbol_component->getScoreSymbol();
		/* Activates the code editor if the message /expr is in the associated symbol. */
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
		}
	}
	/* Blurs the code editor if symbolComponent is NULL. */
	else
	{
		code_document.applyChanges("no content.");
		code_editor->setColour(CodeEditorComponent::backgroundColourId, Colours::grey);
		code_editor->setReadOnly(true);
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
