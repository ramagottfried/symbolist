
#pragma once

#include "BaseComponent.h"

// Use Label for creating text editor on click
using namespace std;

class TextGlphComponent;


class TextEditorObjListener : public TextEditor::Listener
{
public:
    TextEditorObjListener(TextGlphComponent* c) : owner(c) {}
    void textEditorTextChanged (TextEditor& t) override;
    
private:
    TextGlphComponent*    owner;
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TextEditorObjListener)
};

class EditableTextObjListener : public Label::Listener
{
public:
    EditableTextObjListener(TextGlphComponent* c) : owner(c)
    {
        editlistener = new TextEditorObjListener(c);
    }
    
    void editorShown (Label* l, TextEditor& t) override;
    void editorHidden (Label*, TextEditor&) override;
    
    void labelTextChanged (Label* l) override;
    
private:
    TextGlphComponent*                      owner;
    ScopedPointer<TextEditorObjListener>    editlistener;
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (EditableTextObjListener)
};


class EditableTextObj : public Label
{
public:
    EditableTextObj(BaseComponent *c);
    void paint (Graphics& g) override;
    void mouseMove( const MouseEvent& event ) override;
    void mouseDown( const MouseEvent& event ) override;
    void mouseDrag( const MouseEvent& event ) override;
    void mouseUp( const MouseEvent& event ) override;

private:
    BaseComponent *owner;
    ScopedPointer<EditableTextObjListener> listener;
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (EditableTextObj)

};


class TextGlphComponent : public BaseComponent
{
public:
    TextGlphComponent();
    ~TextGlphComponent();
    
    void setBoundsFromSymbol( float x, float y , float w , float h) override final;
    
    Rectangle<float> symbol_export_bounds() override;
    
    void importFromSymbol( const Symbol& s ) override;
    int addSymbolMessages( Symbol* s, const String &base_address ) override;

//    void resized() override;
    
//    void resizeToFit(int x, int y, int w, int h) override;
    virtual void scaleScoreComponent(float scale_w, float scale_h) override;
//    virtual void rotateScoreComponent(float theta, float ax, float ay) override;
    
    void setWidthInPixels(float w);
    
    String getSymbolTypeStr() const override { return "text"; }
    
    void updateText( String str);

    void setFont( Font f ){ m_font = f; }
    
    // override to avoid entering edit mode
    void mouseDoubleClick(const MouseEvent& event) override {}
    
protected:
    
    String      m_text = "T";
    Font        m_font = Font( "Times", 20.0f, Font::plain );
    
    float       m_extrakerning = 0.0;
    float       m_horz_scale = 1.0;
    
    
    float       m_width_offset = 12;
    float       m_x_offset = m_width_offset / 2;
    
    ScopedPointer<EditableTextObj>    textobj;
    
    //==============================================================================
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TextGlphComponent)
    
};
