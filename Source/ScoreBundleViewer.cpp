#include "ScoreBundleViewer.h"
#include "InspectorComponent.h"

TreeViewItem* ScoreBundleViewer::rebuildJson()
{
	var parsedJson = var::undefined();

	InspectorComponent* inspectorComponent = dynamic_cast<InspectorComponent*>(getParentComponent());

	if (inspectorComponent != NULL)
	{
		Result result = JSON::parse(inspectorComponent->getModel()->getScore()->getJSON(), parsedJson);
		
		if (!result.wasOk())
		{
			cout << "Error parsing JSON: " + result.getErrorMessage() << endl;
			return nullptr;
		}
	}
	
	return new JsonTreeItem (Identifier(), parsedJson);
}
