#include "TimeDisplayComponent.hpp"
#include "SymbolistMainComponent.h"

void TimeDisplayComponent::setTime( float t )
{
	m_time = t;
	
	SymbolistMainComponent* mainView = dynamic_cast<SymbolistMainComponent*>(getParentComponent());
	
	if (mainView != NULL)
		m_time_str = "t = " + String(mainView->getController()->getCurrentTime());
	
	updateText( m_time_str );
}
