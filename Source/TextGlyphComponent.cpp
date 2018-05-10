
#include "TextGlyphComponent.h"
#include "SymbolistMainComponent.h"

void TextEditorObjListener::textEditorTextChanged (TextEditor& t)
{
    owner->updateText( t.getText() );
}

void EditableTextObjListener::labelTextChanged (Label* l)
{
    owner->updateText( l->getText() );
}

void EditableTextObjListener::editorShown (Label* l, TextEditor& t)
{
    owner->setEditMode( true );
    t.addListener( editlistener );
    t.setBorder( l->getBorderSize() );
    t.setFont(  l->getFont() );
    
    t.setColour( TextEditor::shadowColourId, Colours::transparentWhite );
    t.setColour( TextEditor::focusedOutlineColourId, Colours::transparentWhite );
    t.setIndents(0, 4);
    // t.setMultiLine(true);
    // maybe use subtracted from...
    // const Rectangle<int> centreArea (border.subtractedFrom (fullSize));

	// f.getStringWidth( l->getText() );
    
    // t.setBounds( owner->getLocalBounds().translated(1, 0) );
    // owner->setBounds( owner->getBounds().expanded(2) );
}

void EditableTextObjListener::editorHidden (Label* l, TextEditor& t)
{
    //owner->setBounds( owner->getBounds().expanded(2) );
    owner->setEditMode( false );

}

EditableTextObj::EditableTextObj(BaseComponent *c) : Label( String(), String() ), owner(c)
{
    setEditable (false, true, false);
    //setJustificationType (Justification::centredLeft);
    setBorderSize( BorderSize<int>(0,0,0,0) );
    setColour( Label::backgroundColourId, Colours::transparentWhite );
    setMinimumHorizontalScale(1.0f);
    
    listener = new EditableTextObjListener( (TextGlphComponent*)c );
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
    
TextGlphComponent::~TextGlphComponent()
{	
    DEBUG_FULL("Deleting text " << this << endl)
};

void TextGlphComponent::setBoundsFromSymbol( float x, float y , float w , float h)
{
    // setBounds( x, y - (h * 0.5), w , h);
    m_font.setHeight( h );
    textobj->setFont( m_font );
    setBounds( x, y - (h * 0.5), m_font.getStringWidth(m_text) + m_width_offset, h );
    textobj->setBounds( getLocalBounds().translated(m_x_offset, 0) );
}

Rectangle<float> TextGlphComponent::symbolExportBounds()
{
    auto b = getBounds().toFloat();
    return Rectangle<float>( b.getX(), b.getY() + b.getHeight()/2.0f, b.getWidth(), b.getHeight() );
}

void TextGlphComponent::importFromSymbol( const Symbol& s )
{
    BaseComponent::importFromSymbol(s);
    
    if( s.addressExists("/text") )
        m_text = s.getMessage("/text").getString();
    
    if( s.addressExists("/font") )
        m_font = Font::fromString( s.getMessage("/font").getString() );
    
    if( s.addressExists("/kerning") )
        m_extrakerning = s.getMessage("/kerning").getFloat();
    
    if( s.addressExists("/h_scale") )
        m_horz_scale = s.getMessage("/h_scale").getFloat();
    
    // init
    m_font = m_font.withExtraKerningFactor( m_extrakerning ).withHorizontalScale( m_horz_scale );
    textobj->setFont( m_font );
    textobj->setText( m_text, sendNotificationSync );
    setBounds(getX(), getY(), m_font.getStringWidth(m_text) + m_width_offset, m_font.getHeight() );
    textobj->setBounds( getLocalBounds().translated(m_x_offset, 0) );

}

void TextGlphComponent::addSymbolMessages(Symbol* s)
{
    BaseComponent::addSymbolMessages( s );
    
    s->addMessage( "/text", m_text.getCharPointer() );
    s->addMessage( "/font", m_font.toString().getCharPointer() );
    s->addMessage( "/kerning", m_extrakerning );
    s->addMessage( "/h_scale", m_horz_scale );
}


//void TextGlphComponent::resized()
//{
//    BaseComponent::resized();
//
//    /*
//    int h = getHeight();
//    
//    m_font.setHeight( h );
//    textobj->setFont( m_font );
//
//    setBounds(getX(), getY(), m_font.getStringWidth(m_text) + m_width_offset, h );
//    textobj->setBounds( getLocalBounds().translated(m_x_offset, 0) );
//    */
//}

void TextGlphComponent::setWidthInPixels(float w)
{
    float current_w = m_font.getStringWidthFloat(m_text);
    if( current_w > 0 )
    {
        float dst_scale = w / current_w;
        float current_scale = m_font.getHorizontalScale();
        
        if( current_scale > 0 )
        {
            m_font.setHorizontalScale( dst_scale / current_scale );
        }
    }
    
}

/*
void TextGlphComponent::resizeToFit(int x, int y, int w, int h)
{
    if( w > 0 && h > 0)
    {
        m_font.setHeight( h );
        setWidthInPixels( w );
        
        textobj->setFont( m_font );
        setBounds(x, y, m_font.getStringWidth(m_text), h );
        textobj->setBounds( getLocalBounds().translated(0, 0) );
    }
}
*/

void TextGlphComponent::scaleScoreComponent(float scaledWidthRatio, float scaledHeightRatio)
{
    //printRect(getBounds(), getSymbolTypeStr() + " pre");

    float newHeight = scaledHeightRatio * getHeight();
    float newWidth = scaledWidthRatio * getWidth();

    m_font.setHeightWithoutChangingWidth( newHeight );
    
    float current_w = m_font.getStringWidthFloat(m_text);
    
    float current_scale = m_font.getHorizontalScale();
    float targetWidth = scaledWidthRatio * getWidth() - 1;
    float fontscale = targetWidth / current_w;
    
//    cout << "h scale 1 " << current_scale << " text width " << current_w << endl;
    m_horz_scale = current_scale * fontscale;
    m_font.setHorizontalScale( m_horz_scale );

    textobj->setFont( m_font );
    setSize( newWidth, newHeight );
    textobj->setBounds( getLocalBounds().translated(0, 0) );
    
    /*
    cout << "h scale 2 " << m_font.getHorizontalScale() << " width " << m_font.getStringWidth(m_text) << endl;
    printRect(getBounds(), getSymbolTypeStr() + " post" );
    printRect(textobj->getBounds(), getSymbolTypeStr() + " inner");
     */
}

void TextGlphComponent::updateText( String str)
{
    if( m_text != str )
    {
        m_text = str;
        if( !in_edit_mode )
            textobj->setText (m_text, sendNotificationAsync );
        
        setSize(m_font.getStringWidth(m_text), getHeight() );
        textobj->setBounds( getLocalBounds().translated(m_x_offset, 0) );
    }
}

