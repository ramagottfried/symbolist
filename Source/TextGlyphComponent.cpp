
#include "TextGlyphComponent.h"

void TextEditorObjListener::textEditorTextChanged (TextEditor& t)
{
    owner->updateText( t.getText() );
}


void EditableTextObjListener::labelTextChanged (Label* l)
{
    cout << l->getText() << endl;
    owner->updateText( l->getText() );
}

void EditableTextObjListener::editorShown (Label* l, TextEditor& t)
{
    t.addListener( editlistener );
    t.setBorder( l->getBorderSize() );
    t.setFont(  l->getFont() );
    
    t.setColour( TextEditor::shadowColourId, Colours::transparentWhite );
    t.setColour( TextEditor::focusedOutlineColourId, Colours::transparentWhite );
    t.setIndents(0, 4);
    
    // maybe use subtracted from...
    // const Rectangle<int> centreArea (border.subtractedFrom (fullSize));

//    f.getStringWidth( l->getText() );
    
    // t.setBounds( owner->getLocalBounds().translated(1, 0) );
    //owner->setBounds( owner->getBounds().expanded(2) );
}

void EditableTextObjListener::editorHidden (Label* l, TextEditor& t)
{
    
}


EditableTextObj::EditableTextObj(BaseComponent *c) : Label( String(), String() ), owner(c)
{
    setEditable (false, true, false);
    setJustificationType (Justification::centredLeft);
    setBorderSize( BorderSize<int>(0,0,0,0) );
    setColour( Label::backgroundColourId, Colours::transparentWhite );
    setMinimumHorizontalScale(1.0f);
    
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
    addAndMakeVisible(textobj);
}
    
TextGlphComponent::~TextGlphComponent(){
    cout << "deleting text " <<  this << endl;
};

void TextGlphComponent::setBoundsFromSymbol( float x, float y , float w , float h)
{
    //setBounds( x, y - (h * 0.5), w , h);
    m_font.setHeight( h );
    textobj->setFont( m_font );
    setBounds( x, y - (h * 0.5), textobj->getFont().getStringWidth(m_text) + 8, h );
    textobj->setBounds( getLocalBounds().translated(4, 0) );
}

Rectangle<float> TextGlphComponent::symbol_export_bounds()
{
    auto b = getBounds().toFloat();
    return Rectangle<float>( b.getX(), b.getY() + b.getHeight()/2.0f, b.getWidth(), b.getHeight() );
}

void TextGlphComponent::importFromSymbol( const Symbol& s )
{
    BaseComponent::importFromSymbol(s);
    
    int pos = s.getOSCMessagePos("/text");
    if( pos != -1 )
    {
        m_text = s.getOSCMessageValue(pos).getString();
    }
    
    pos = s.getOSCMessagePos("/font");
    if( pos != -1 )
    {
        m_font = Font::fromString( s.getOSCMessageValue(pos).getString() );
    }

    textobj->setFont( m_font );
    textobj->setText( m_text, sendNotificationSync );
    
    setBounds(getX(), getY(), textobj->getFont().getStringWidth(m_text) + 8, m_font.getHeight() );
    textobj->setBounds( getLocalBounds().translated(4, 0) );

}

int TextGlphComponent::addSymbolMessages( Symbol* s, const String &base_address )
{
    int messages_added = BaseComponent::addSymbolMessages( s, base_address );
    
    auto b = symbol_export_bounds();
    
    String addr;
    
    addr = base_address + "/text";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr, m_text );
        messages_added++;
    }

    addr = base_address + "/font";
    if( s->getOSCMessagePos(addr) == -1 )
    {
        s->addOSCMessage( addr, m_font.toString() );
        messages_added++;
    }
    
    return messages_added;
}


void TextGlphComponent::resized()
{
    BaseComponent::resized();
    int h = getHeight();
    setBounds(getX(), getY(), textobj->getFont().getStringWidth(m_text) + 8, h );
    
    //textobj->setFont( Font( h ) );
//    textobj->setBounds( 4, 0, textobj->getFont().getStringWidth(m_text) + 4, h );
    textobj->setBounds( getLocalBounds().translated(4, 0) );
}

void TextGlphComponent::updateText( String str)
{
    m_text = str;
    resized();
}
