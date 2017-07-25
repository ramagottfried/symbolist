
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolPropertiesPanel.h"

typedef std::function<void(const OSCMessage&)> osc_callback_t;

class OSCFloatValueSlider : public SliderPropertyComponent
{
public:
    OSCFloatValueSlider ( const String& _addr, OSCMessage& msg, osc_callback_t change_fn )
    : SliderPropertyComponent (_addr, 0.0, 1000.0, 0.0001), osc_msg(msg)
    {
        change_callback = change_fn;
        slider.setColour(Slider::textBoxTextColourId, Colours::black );
        slider.setColour(Slider::trackColourId, Colour::fromFloatRGBA(0, 0, 0, 0.1) );
    }
    
    void setValue (double newValue) override
    {
        osc_msg.clear();
        osc_msg.addFloat32((float)newValue);
        
        slider.setValue ( newValue );
        
        change_callback( osc_msg );
    }
    
    virtual double getValue() const override
    {
        return osc_msg[0].getFloat32();
    }
    
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCFloatValueSlider)
};


class OSCIntValueSlider : public SliderPropertyComponent
{
public:
    OSCIntValueSlider ( const String& _addr, OSCMessage& msg, osc_callback_t change_fn )
    : SliderPropertyComponent (_addr, 0, 1000, 1), osc_msg(msg)
    {
        change_callback = change_fn;
        
        slider.setColour(Slider::textBoxTextColourId, Colours::black );
        slider.setColour(Slider::trackColourId, Colour::fromFloatRGBA(0, 0, 0, 0.1) );
    }
    
    void setValue (double newValue) override
    {
        osc_msg.clear();
        osc_msg.addInt32((int)newValue);
        
        slider.setValue ( newValue );
        
        change_callback( osc_msg );
    }
    
    virtual double getValue() const override
    {
        return (double)osc_msg[0].getInt32();
    }
    
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCIntValueSlider)
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
        
        if( msg[0].isString() )
            color = Colour::fromString( msg[0].getString() );
        else if( msg.size() == 4 && msg[0].isFloat32() )
        {
            color = Colour::fromFloatRGBA(  msg[0].getFloat32(),
                                            msg[1].getFloat32(),
                                            msg[2].getFloat32(),
                                            msg[3].getFloat32() );
        }
        
        refresh();
    }
    
    ~OSCColourSelectorButton(){}
    
    void buttonClicked (Button*) override
    {
        // launch color picker
        ColourSelector* colourSelector = new ColourSelector();
        colourSelector->setName ("color");
        colourSelector->setCurrentColour ( color );
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
            
            osc_msg.clear();
            osc_msg.addString( color.toString() );
            
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
    ChoicePropertyComponent( _addr ),
    osc_msg(msg)
    {
        choices = choiceList;
        change_callback = change_fn;
        
        selected_index = choices.indexOf( msg[0].getString() );
 
        getLookAndFeel().setColour(ComboBox::textColourId, Colours::black );
        getLookAndFeel().setColour(ComboBox::backgroundColourId, Colours::transparentWhite );
        getLookAndFeel().setColour(ComboBox::arrowColourId, Colours::black );
        
        getLookAndFeel().setColour(PopupMenu::textColourId, Colours::black );
        getLookAndFeel().setColour(PopupMenu::backgroundColourId, Colour::fromFloatRGBA(0.9, 0.9, 0.9, 1) );
   
        refresh();
    }
    
    void setIndex (int newIndex) override
    {
        selected_index = newIndex;

        osc_msg.clear();
        osc_msg.addString( choices[selected_index] );
        
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

class OSCTextProperty : public TextPropertyComponent
{
public:
    OSCTextProperty(    const String& _addr,
                        OSCMessage& msg,
                        osc_callback_t change_fn ) :
    TextPropertyComponent( _addr, 0, false ),
    osc_msg(msg)
    {
        change_callback = change_fn;
        
        text = msg[0].getString();
        
        setColour(TextPropertyComponent::textColourId , Colours::black );
        setColour(TextPropertyComponent::backgroundColourId, Colours::transparentWhite );
    }
    
    void setText (const String& newText) override
    {
        text = newText;
        
        osc_msg.clear();
        osc_msg.addString( text );
        
        change_callback( osc_msg );

    }
    String getText() const override
    {
        return text;
    }
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
    
    String          text;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCTextProperty)
    
};


class OSCOnOffButton : public BooleanPropertyComponent
{
public:
   
    OSCOnOffButton( const String& _addr,
                    OSCMessage& msg,
                    osc_callback_t change_fn ) :
    BooleanPropertyComponent (_addr, "true", "false"),
    osc_msg(msg)
    {
        change_callback = change_fn;
        
        setState( msg[0].getInt32() > 0 );
    }
    
    void setState( bool newState ) override
    {
        state = newState;
        refresh();

    }
    
    bool getState() const override
    {
        return state;
    }
    
    void buttonClicked (Button* b) override
    {
        setState ( !state );
        
        osc_msg.clear();
        osc_msg.addInt32( (int)getState() );
        
        change_callback( osc_msg );
    }
    
private:

    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
        
    bool            state = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCOnOffButton)
};


class StaffSelectionButton : public ButtonPropertyComponent
{
public:
    StaffSelectionButton(   const String& _addr,
                            OSCMessage& msg,
                            osc_callback_t change_fn ) :
    ButtonPropertyComponent (_addr, true),
    osc_msg(msg)
    {
        change_callback = change_fn;
        
        text = msg[0].getString();
        refresh();

    }
/*
    void setText (const String& newText)
    {
        text = newText;
        
        osc_msg.clear();
        osc_msg.addString( text );
        
        change_callback( osc_msg );
        
    }
  */
    
    void buttonClicked() override
    {

        /*
         *  Enter Staff selection mode, where staff objects are clickable
         *  clicking on a staff fills the staff field.
         */
        
        AlertWindow::showMessageBoxAsync (AlertWindow::InfoIcon, "Action Button Pressed",
                                          "Pressing this type of property component can trigger an action such as showing an alert window!");
        refresh();
    }
    
    String getButtonText() const override
    {
        return text;
    }
    
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
    
    String          text;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (StaffSelectionButton)
    
};



// menu with font name, size and style options
class OSCFontMenu : public ChoicePropertyComponent
{
public:
    OSCFontMenu(const String& _addr,
                OSCMessage& msg,
                osc_callback_t change_fn,
                StringArray choiceList ) :
    ChoicePropertyComponent( _addr ),
    osc_msg(msg)
    {
        choices = choiceList;
        change_callback = change_fn;
        
        m_font = Font::fromString( msg[0].getString() );
        selected_index = choices.indexOf( m_font.getTypefaceName() );
        
        getLookAndFeel().setColour(ComboBox::textColourId, Colours::black );
        getLookAndFeel().setColour(ComboBox::backgroundColourId, Colours::transparentWhite );
        getLookAndFeel().setColour(ComboBox::arrowColourId, Colours::black );
        
        getLookAndFeel().setColour(PopupMenu::textColourId, Colours::black );
        getLookAndFeel().setColour(PopupMenu::backgroundColourId, Colour::fromFloatRGBA(0.9, 0.9, 0.9, 1) );
        
        refresh();
    }
    
    void setIndex (int newIndex) override
    {
        selected_index = newIndex;
        
        osc_msg.clear();
        
        m_font.setTypefaceName(choices[selected_index]);
        osc_msg.addString( m_font.toString() );
        
        change_callback( osc_msg );
        
        cout << "FONT SELECTION ISN'T COMPLETE YET, \n need to add size and style for conversion to str storage" <<  endl;
    }
    
    int getIndex() const override
    {
        return selected_index;
    }
    
    
private:
    OSCMessage      osc_msg;
    osc_callback_t  change_callback;
    
    
    Font            m_font;
    
    int             selected_index = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCFontMenu)
    
};
