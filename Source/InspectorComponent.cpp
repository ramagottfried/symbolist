#include "InspectorComponent.h"

InspectorComponent::InspectorComponent( SymbolistHandler* mainController ) : TabbedComponent(TabbedButtonBar::Orientation::TabsAtTop)
{
        symbol_panel_tab = new SymbolPropertiesPanel(mainController);
        const Colour c = Colours::lightgrey;
        addTab ("inspector",  c, symbol_panel_tab, false);
        symbol_panel_bundleviewer = new ScoreBundleViewer(mainController);
        addTab ("score bundle",  c, symbol_panel_bundleviewer, false);
		
        setSize (400, 600);
}

void InspectorComponent::updateSymbolFromComponent(BaseComponent* component)
{
	if (getController() != NULL)
		getController()->updateSymbolFromComponent(component);
	else throw logic_error("InspectorComponent has no attached controller.");
}
