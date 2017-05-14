//
//  OSCParser.cpp
//  symbolist
//
//  Created by Jean Bresson on 13/05/2017.
//
//

#include "OSCParser.h"
#include <stdio.h>


OSCArgument OSCParser::readArgument (OSCType type)
{
    switch (type)
    {
            //case OSCTypes::int32:       return OSCArgument (readInt32());
            //case OSCTypes::float32:     return OSCArgument (readFloat32());
            //case OSCTypes::string:      return OSCArgument (readString());
            //case OSCTypes::blob:        return OSCArgument (readBlob());
        case 'i':       return OSCArgument (readInt32());
        case 'f':       return OSCArgument (readFloat32());
        case 's':       return OSCArgument (readString());
        case 'b':       return OSCArgument (readBlob());
            
        default:
            // You supplied an invalid OSCType when calling readArgument! This should never happen.
            jassertfalse;
            throw OSCInternalError ("OSC input stream: internal error while reading message argument");
    }
}
