//
//  OSCParser.h
//  symbolist
//
//  Created by Jean Bresson on 12/05/2017.
//
//

#include "../JuceLibraryCode/JuceHeader.h"
//#include "../../JUCE/modules/juce_osc/osc/juce_OSCReceiver.cpp"

#ifndef OSCParser_h
#define OSCParser_h

class OSCParser : public OSCReceiver {
  
    public :
    
    OSCBundle parseData (void* bundle_data) {
        OSCInputStream inStream (bundle_data, sizeof(bundle_data));
        return inStream.readBundle();
    };
    
    private :
    
};

#endif /* OSCParser_h */
