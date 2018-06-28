
#pragma once

#include "../JuceLibraryCode/JuceHeader.h"
#include "SymbolPropertiesPanel.h"

typedef std::function<void(const OdotMessage&)> osc_callback_t;

class OSCFloatValueSlider : public SliderPropertyComponent
{
public:
    OSCFloatValueSlider ( const string& _addr, OdotMessage& msg, osc_callback_t change_fn )
    	: SliderPropertyComponent (_addr, -1000.0, 1000.0, 0.0001), osc_msg(msg)
    {
        change_callback = change_fn;
        slider.setColour(Slider::textBoxTextColourId, Colours::black );
        slider.setColour(Slider::trackColourId, Colour::fromFloatRGBA(0, 0, 0, 0.1) );
    }
    
    void setValue (double newValue) override
    {
        osc_msg.clear();
        osc_msg.appendValue( (float)newValue );
    
        slider.setValue ( newValue );
        
        change_callback( osc_msg );
    }
    
    virtual double getValue() const override
    {
        return osc_msg[0].getDouble();
    }
    
    
private:
    OdotMessage     osc_msg;
    osc_callback_t  change_callback;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCFloatValueSlider)
};


class OSCIntValueSlider : public SliderPropertyComponent
{
public:
    OSCIntValueSlider ( const string& _addr, OdotMessage& msg, osc_callback_t change_fn )
    : SliderPropertyComponent (_addr, 0, 1000, 1), osc_msg(msg)
    {
        change_callback = change_fn;
        
        slider.setColour(Slider::textBoxTextColourId, Colours::black );
        slider.setColour(Slider::trackColourId, Colour::fromFloatRGBA(0, 0, 0, 0.1) );
    }
    
    void setValue (double newValue) override
    {
        osc_msg.clear();
        osc_msg.appendValue((int)newValue);
        
        slider.setValue ( newValue );
        
        change_callback( osc_msg );
    }
    
    virtual double getValue() const override
    {
        return (double)osc_msg[0].getInt();
    }
    
    
private:
    OdotMessage      osc_msg;
    osc_callback_t  change_callback;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCIntValueSlider)
};


class OSCColourSelectorButton  : public PropertyComponent, public ChangeListener, private Button::Listener
{
public:
    OSCColourSelectorButton ( const string& _addr, OdotMessage& msg, osc_callback_t change_fn ) : PropertyComponent (_addr), osc_msg(msg)
    {
        change_callback = change_fn;
        addAndMakeVisible (button);
        button.setTriggeredOnMouseDown (true);
        button.addListener (this);
        
        if( msg[0].getType() == OdotAtom::O_ATOM_STRING )
            color = Colour::fromString( msg[0].getString().c_str() );
        else if( msg.size() == 4 && msg[0].getType() == OdotAtom::O_ATOM_FLOAT )
        {
            color = Colour::fromFloatRGBA(  msg[0].getFloat(),
                                            msg[1].getFloat(),
                                            msg[2].getFloat(),
                                            msg[3].getFloat() );
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
            
            osc_msg.appendValue( (const char *)color.toString().getCharPointer() );
            
            change_callback( osc_msg );
            
            refresh();
        }
    }
    
private:
    OdotMessage     osc_msg;
    osc_callback_t  change_callback;

    TextButton      button;
    Colour          color = Colours::black ;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCColourSelectorButton)
};


class OSCOptionMenu : public ChoicePropertyComponent
{
public:
    OSCOptionMenu(  const string& _addr,
                    OdotMessage& msg,
                    osc_callback_t change_fn,
                    StringArray choiceList ) :
    ChoicePropertyComponent( _addr ),
    osc_msg(msg)
    {
        choices = choiceList;
        change_callback = change_fn;
        
        selected_index = choices.indexOf( String( msg[0].getString() ) );
 
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
        osc_msg.appendValue( (const char *)choices[selected_index].getCharPointer() );
        
        change_callback( osc_msg );
    }
    
    int getIndex() const override
    {
        return selected_index;
    }
    
    
private:
    OdotMessage     osc_msg;
    osc_callback_t  change_callback;
    
    int             selected_index = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCOptionMenu)

};

class OSCTextProperty : public TextPropertyComponent {

public:
    OSCTextProperty(    const string& _addr,
                        OdotMessage& msg,
                        osc_callback_t change_fn,
                        bool multiline = false ) :
    TextPropertyComponent( _addr, 0, multiline ),
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
        osc_msg.appendValue( (const char *)text.getCharPointer() );
        
        change_callback( osc_msg );

    }
    String getText() const override
    {
        return text;
    }
    
private:
    OdotMessage     osc_msg;
    osc_callback_t  change_callback;
    
    String          text;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCTextProperty)
    
};

class OSCOnOffButton : public BooleanPropertyComponent
{
public:
   
    OSCOnOffButton( const string& _addr,
                    OdotMessage& msg,
                    osc_callback_t change_fn ) :
    BooleanPropertyComponent (_addr, "true", "false"),
    osc_msg(msg)
    {
        change_callback = change_fn;
        
        setState( msg[0].getInt() > 0 );
    }
    
    void setState( bool newState ) override
    {
        state = newState;
		
        osc_msg.clear();
        osc_msg.appendValue( (int)getState() );
		
        change_callback( osc_msg );
		
        refresh();

    }
    
    bool getState() const override
    {
        return state;
    }
    
private:

    OdotMessage      osc_msg;
    osc_callback_t  change_callback;
        
    bool            state = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCOnOffButton)
};

class StaffSelectionButton : public ButtonPropertyComponent
{
public:
    StaffSelectionButton(   const string& _addr,
                            OdotMessage& msg,
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
    OdotMessage      osc_msg;
    osc_callback_t  change_callback;
    
    String          text;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (StaffSelectionButton)
    
};

// menu with font name, size and style options
class OSCFontMenu : public ChoicePropertyComponent
{
public:
    OSCFontMenu(const string& _addr,
                OdotMessage& msg,
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
        
        m_font.setTypefaceName( choices[selected_index] );
        osc_msg.appendValue( (const char *)m_font.toString().getCharPointer() );
        
        change_callback( osc_msg );
        
        cout << "FONT SELECTION ISN'T COMPLETE YET, \n need to add size and style for conversion to str storage" <<  endl;
    }
    
    int getIndex() const override
    {
        return selected_index;
    }
    
    
private:
    OdotMessage      osc_msg;
    osc_callback_t  change_callback;
    
    
    Font            m_font;
    
    int             selected_index = 0;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCFontMenu)
    
};

class NonEditableTextObj : public Label
{
public:
    NonEditableTextObj( String str) :
    Label( String(), str )
    {
        setEditable (false, false, false);
        setColour(textColourId, Colour::fromFloatRGBA(0., 0., 0., 0.4) );
        setColour(backgroundColourId, Colours::transparentWhite );
        setColour(outlineColourId, Colour::fromFloatRGBA(0., 0., 0., 0.4)  );
        Font f = getFont();
        f.setItalic(true);
        setFont( f );
    }
    
    ~NonEditableTextObj(){}
    
private:
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (NonEditableTextObj)
    
};

        
class OSCValueDisplay  : public PropertyComponent
{
public:
    OSCValueDisplay ( const string& _addr, OdotMessage& msg ) :
    PropertyComponent (_addr), osc_msg(msg)
    {
        
        String str = msg[0].getString();
        
        label = new NonEditableTextObj( str );
        addAndMakeVisible(label);
        
        refresh();
    }
    
    inline ~OSCValueDisplay() = default;
    
    void refresh() override {}
    
    
private:
    OdotMessage      osc_msg;
    ScopedPointer<NonEditableTextObj> label;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (OSCValueDisplay)
};
