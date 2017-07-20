
#include "TextGlyphComponent.h"

void EditableTextObjListener::labelTextChanged (Label* l)
{
    cout << l->getText() << endl;
    owner->updateText( l->getText() );
    owner->resized();
}

EditableTextObj::EditableTextObj(BaseComponent *c) : Label( String(), String() ), owner(c)
{
    setEditable (false, true, false);
    setJustificationType (Justification::centredLeft);
    setBorderSize( BorderSize<int>(0,0,0,0) );
    
    listener = new EditableTextObjListener((TextGlphComponent*)c);
    addListener(listener);
    
}
    
void EditableTextObj::paint (Graphics& g)
{
    owner->paint(g);
    setColour( Label::textColourId, owner->getCurrentColor() );
    Label::paint(g);
}

void EditableTextObj::mouseMove( const MouseEvent& event )
{
    owner->mouseMove(event);
    Label::mouseMove(event);
}

void EditableTextObj::mouseDown( const MouseEvent& event )
{
    owner->mouseDown(event);
    Label::mouseDown(event);
}

void EditableTextObj::mouseDrag( const MouseEvent& event )
{
    owner->mouseDrag(event);
    Label::mouseDrag(event);
    
}

void EditableTextObj::mouseUp( const MouseEvent& event )
{
    owner->mouseUp(event);
    Label::mouseUp(event);
    
}

//==========================================================================================

TextGlphComponent::TextGlphComponent()
{
    textobj = new EditableTextObj(this);
    textobj->setText (m_text, sendNotificationSync );
    textobj->setColour( Label::textColourId, getCurrentColor() );
    textobj->setColour( Label::backgroundColourId, Colours::transparentWhite );
    textobj->setMinimumHorizontalScale(1.0f);
    addAndMakeVisible(textobj);
    
}
    
TextGlphComponent::~TextGlphComponent(){
    cout << "deleting text " <<  this << endl;
};

void TextGlphComponent::setBoundsFromSymbol( float x, float y , float w , float h)
{
    //setBounds( x, y - (h * 0.5), w , h);
    textobj->setFont( Font(h) );
    setBounds( x, y, textobj->getFont().getStringWidth(m_text), h );
    textobj->setBounds( getLocalBounds() );
}

Rectangle<float> TextGlphComponent::symbol_export_bounds()
{
    auto b = getBounds().toFloat();
    return Rectangle<float>( b.getX(), b.getY() + b.getHeight()/2, b.getWidth(), b.getHeight() );
}

void TextGlphComponent::importTextFromSymbol( const Symbol& s ) {}

void TextGlphComponent::resized()
{
    BaseComponent::resized();
    int h = getHeight();
    setBounds(getX(), getY(), textobj->getFont().getStringWidth(m_text), h );
    
    textobj->setFont( Font( h ) );
    textobj->setBounds( getLocalBounds() );
}

void TextGlphComponent::updateText( String str)
{
    m_text = str;
}
