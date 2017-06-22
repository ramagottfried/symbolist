
#include "LinePathComponent.h"
#include "PageComponent.h"

LinePathComponent::LinePathComponent(const Symbol &s) : PathBaseComponent( s )
{}


/*************
 *  SELECT, AND MODE SETTING FOR MOUSE LISTENING
 ************/

void LinePathComponent::componentCretated()
{
    m_path.clear();
    setEditMode(true);
}


/*************
 *  MOUSE UI
 ************/




