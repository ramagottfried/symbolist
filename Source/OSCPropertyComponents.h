
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolPropertiesPanel.h"

typedef std::function<void(OSCMessage&)> osc_callback_t;

class OSCFloatValueSlider : public SliderPropertyComponent
{
public:
    OSCFloatValueSlider ( const String& _addr, OSCMessage& msg, osc_callback_t change_fn )
    : SliderPropertyComponent (_addr, 0.0, 1000.0, 0.0001), osc_msg(msg)
    {
        change_callback = change_fn;
    }
    
    void setValue (double newValue) override
    {
        osc_msg.clear();
        osc_msg.addFloat32((float)newValue);
        
        slider.setValue ( newValue );
        
        change_callback( osc_msg );
    }
    
    virtual double getValue() const
    {
        return osc_msg[0].getFloat32();
    }
    
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCFloatValueSlider)
};



class OSCColourSelectorButton  : public PropertyComponent, public ChangeListener, private ButtonListener
{
public:
    OSCColourSelectorButton ( const String& _addr, OSCMessage& msg, osc_callback_t change_fn ) : PropertyComponent (_addr), osc_msg(msg)
    {
        change_callback = change_fn;
        addAndMakeVisible (button);
        button.setTriggeredOnMouseDown (true);
        button.addListener (this);
        refresh();
    }
    
    ~OSCColourSelectorButton(){}
    
    void buttonClicked (Button*) override
    {
        // launch color picker
        ColourSelector* colourSelector = new ColourSelector();
        colourSelector->setName ("color");
        colourSelector->setCurrentColour (findColour (TextButton::buttonColourId));
        colourSelector->addChangeListener (this);
        colourSelector->setColour (ColourSelector::backgroundColourId, Colours::transparentBlack);
        colourSelector->setSize (300, 400);
        
        CallOutBox::launchAsynchronously (colourSelector, getScreenBounds(), nullptr);
    }
    
    String getButtonText() const
    {
        return color.toString();
    }
    
    void refresh() override
    {
        button.setButtonText (getButtonText());
        button.setColour( button.buttonColourId, color );
    }
    
    void changeListenerCallback (ChangeBroadcaster* source) override
    {
        if (ColourSelector* cs = dynamic_cast<ColourSelector*> (source))
        {
            setColour (TextButton::buttonColourId, cs->getCurrentColour());
            color = cs->getCurrentColour();
            
            change_callback( osc_msg );
            
            refresh();
        }
    }
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;

    TextButton      button;
    Colour          color = Colours::black ;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCColourSelectorButton)
};


class OSCOptionMenu : public ChoicePropertyComponent
{
public:
    OSCOptionMenu(  const String& _addr,
                    OSCMessage& msg,
                    osc_callback_t change_fn,
                    StringArray choiceList ) :
    ChoicePropertyComponent( _addr ), osc_msg(msg)
    {
        choices = choiceList;
        change_callback = change_fn;
    }
    
    void setIndex (int newIndex) override
    {
        selected_index = newIndex;

        // update msg here
        change_callback( osc_msg );
    }
    
    int getIndex() const override
    {
        return selected_index;
    }
    
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
    
    int             selected_index = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCOptionMenu)

};




//==============================================================================
class StaffSelectionButton : public ButtonPropertyComponent
{
public:
    StaffSelectionButton (const String& propertyName)
    : ButtonPropertyComponent (propertyName, true)
    {
        refresh();
    }
    
    void buttonClicked() override
    {
        AlertWindow::showMessageBoxAsync (AlertWindow::InfoIcon, "Action Button Pressed",
                                          "Pressing this type of property component can trigger an action such as showing an alert window!");
        refresh();
    }
    
    String getButtonText() const override
    {
        return staff_name;
    }
    
private:
    String staff_name;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (StaffSelectionButton)
};


//==============================================================================
static Array<PropertyComponent*> createTextEditors()
{
    Array<PropertyComponent*> comps;
    
    comps.add (new TextPropertyComponent (Value (var ("This is a single-line Text Property")), "Text 1", 200, false));
    comps.add (new TextPropertyComponent (Value (var ("Another one")), "Text 2", 200, false));
    
    comps.add (new TextPropertyComponent (Value (var (
                                                      "Lorem ipsum dolor sit amet, cu mei labore admodum facilisi. Iriure iuvaret invenire ea vim, cum quod"
                                                      "si intellegat delicatissimi an. Cetero recteque ei eos, his an scripta fastidii placerat. Nec et anc"
                                                      "illae nominati corrumpit. Vis dictas audire accumsan ad, elit fabulas saperet mel eu.\n"
                                                      "\n"
                                                      "Dicam utroque ius ne, eum choro phaedrum eu. Ut mel omnes virtute appareat, semper quodsi labitur in"
                                                      " cum. Est aeque eripuit deleniti in, amet ferri recusabo ea nec. Cu persius maiorum corrumpit mei, i"
                                                      "n ridens perpetua mea, pri nobis tation inermis an. Vis alii autem cotidieque ut, ius harum salutatu"
                                                      "s ut. Mel eu purto veniam dissentias, malis doctus bonorum ne vel, mundi aperiam adversarium cu eum."
                                                      " Mei quando graeci te, dolore accusata mei te.")),
                                          "Multi-line text",
                                          1000, true));
    
    return comps;
}

/*
 static Array<PropertyComponent*> createSliders (int howMany)
 {
 Array<PropertyComponent*> comps;
 
 for (int i = 0; i < howMany; ++i)
 comps.add (new DemoSliderPropertyComponent ("Slider " + String (i + 1)));
 
 return comps;
 }
 
 static Array<PropertyComponent*> createButtons (int howMany)
 {
 Array<PropertyComponent*> comps;
 
 for (int i = 0; i < howMany; ++i)
 comps.add (new DemoButtonPropertyComponent ("Button " + String (i + 1)));
 
 for (int i = 0; i < howMany; ++i)
 comps.add (new BooleanPropertyComponent (Value (Random::getSystemRandom().nextBool()), "Toggle " + String (i + 1), "Description of toggleable thing"));
 
 return comps;
 }
  */

