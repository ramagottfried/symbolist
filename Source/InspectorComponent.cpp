#include "InspectorComponent.h"

InspectorComponent::InspectorComponent() : TabbedComponent(TabbedButtonBar::Orientation::TabsAtTop)
{
        symbol_panel_tab = new SymbolPropertiesPanel();
        const Colour c = Colours::lightgrey;
        addTab ("inspector",  c, symbol_panel_tab, false);
	
        symbol_panel_bundleviewer = new ScoreBundleViewer();
        addTab ("score bundle",  c, symbol_panel_bundleviewer, false);
		
        setSize (getParentWidth(), getParentHeight());
}

float InspectorComponent::getPreferedHeight()
{
	int overallMargin = 20;
	
	return getTabBarDepth() + symbol_panel_tab->getPreferedHeight() + overallMargin;

}

void InspectorComponent::updateSymbolFromComponent(BaseComponent* component)
{
	if (getController() != NULL)
		getController()->updateSymbolFromComponent(component);
	else throw logic_error("InspectorComponent has no attached controller.");
}
