
#pragma once

#include "BaseComponent.h"

// Use Label for creating text editor on click
using namespace std;

class EditableTextObj : public Label
{
public:
    EditableTextObj(BaseComponent *c) :
    Label( String(), String() ),
    owner(c)
    {
        setEditable (false, true, false);
        setJustificationType (Justification::centredLeft);
        setBorderSize( BorderSize<int>(0,0,0,0) );
    }
    
    void paint (Graphics& g) override
    {
        owner->paint(g);
        setColour( Label::textColourId, owner->getCurrentColor() );
        Label::paint(g);
    }
        
    void mouseMove( const MouseEvent& event ) override
    {
        owner->mouseMove(event);
        Label::mouseMove(event);
    }
    
    void mouseDown( const MouseEvent& event ) override
    {
        owner->mouseDown(event);
        Label::mouseDown(event);
    }

    void mouseDrag( const MouseEvent& event ) override
    {
        owner->mouseDrag(event);
        Label::mouseDrag(event);

    }

    void mouseUp( const MouseEvent& event ) override
    {
        owner->mouseUp(event);
        Label::mouseUp(event);

    }
        

private:
    BaseComponent *owner;
        
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (EditableTextObj)

};


class TextGlphComponent : public BaseComponent
{
public:
    TextGlphComponent()
    {
        textobj = new EditableTextObj(this);
        textobj->setText (m_text, sendNotificationSync );
        textobj->setColour( Label::textColourId, getCurrentColor() );
        textobj->setColour( Label::backgroundColourId, Colours::transparentWhite );
        addAndMakeVisible(textobj);

    }
    
    ~TextGlphComponent(){
        cout << "deleting text " <<  this << endl;
    };
    

    void setBoundsFromSymbol( float x, float y , float w , float h) override final
    {
        //setBounds( x, y - (h * 0.5), w , h);
        textobj->setFont( Font(h) );
        setBounds( x, y, textobj->getFont().getStringWidth(m_text), h );
    }
    
    Rectangle<float> symbol_export_bounds() override
    {
        auto b = getBounds().toFloat();
        return Rectangle<float>( b.getX(), b.getY() + b.getHeight()/2, b.getWidth(), b.getHeight() );
    }
    
    void importTextFromSymbol( const Symbol& s ) {}

    void resized() override
    {
        BaseComponent::resized();
        int h = getHeight();
        textobj->setFont( Font( h ) );
        textobj->setBounds( 0, 0, textobj->getFont().getStringWidth(m_text), h );
    }
    
    String getSymbolTypeStr() const override { return "text"; }

private:
    
    String      m_text = "text";
    Font        m_font;
    
    ScopedPointer<EditableTextObj>    textobj;
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TextGlphComponent)
    
};
