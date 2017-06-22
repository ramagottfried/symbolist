
#include "TimePointArray.h"

void TimePointArray::addSymbolTimePoints( Symbol *s )
{
    float start_t = s->getTime();
    float end_t = s->getEndTime();

    addSymbol_atTime( s, start_t );
    addSymbol_atTime( s, end_t );
}

void TimePointArray::printTimePoints()
{
    cout << "-------- timepoint list ----------" << endl;
    int count = 0;
    for( auto t : (*this) )
    {
        cout << "timepoint " << count << " " << t->symbols_at_time.size() << " overlapping at " << t->time << endl;
        count++;
    }
}

void TimePointArray::addSymbol_atTime( Symbol *s, float time)
{
    bool match;
    int idx = getTimePointIndex( time, match );
    if( match )
    {
        (*this)[ idx ]->addSymbol( s );
    }
    else
    {
        auto newTimePoint = insert( idx, new SymbolTimePoint( s, time ) );
        
        if( idx - 1 >= 0 )
        {
            auto prevTimePoint = (*this)[idx-1];

            for( auto prev_s : prevTimePoint->symbols_at_time )
            {
                if( s == prev_s ) continue;
                
                if( prev_s->hitTest( time ) )
                {
                    newTimePoint->addSymbol( prev_s );
                }
                else if ( s->hitTest( prevTimePoint->time ) )
                {
                    prevTimePoint->addSymbol( s );
                }

            }
    
        }
    }
    
    printTimePoints();
}

void printBundle(OSCBundle bndl)
{
    std::cout << "==== OSC BUNDLE ====" << std::endl;
    for (auto osc : bndl )
    {
        OSCMessage msg = osc.getMessage();
        std::cout << msg.getAddressPattern().toString();
        
        for (auto arg : msg )
        {
            if( arg.isString() )
                std::cout << " " << arg.getString();
            else if( arg.isFloat32() )
                std::cout << " " << (String)arg.getFloat32();
            else if( arg.isInt32() )
                std::cout << " " << (String)arg.getInt32();
            else if( arg.isBlob() )
                std::cout << " " << "blob";
        }
        
        std::cout << std::endl;
    }
    std::cout << "====-===-======-====" << std::endl;
    
}

odot_bundle* symbolBundleToOdot( const OSCBundle& osc )
{
    OSCWriter w ;
    w.writeBundle( osc );
    size_t size = w.getDataSize();
    
    odot_bundle *bundle = new odot_bundle;
    
    bundle->len = static_cast<long>(size);
    bundle->data = new char[size];
    std::memcpy(bundle->data, w.getData() ,size );
    
    return bundle;
}

odot_bundle *TimePointArray::symbolVectorToOSC( const vector<Symbol*> vec )
{
    OSCBundle bndl;
    int count = 0;
    String prefix = "/symbolsAtTime/";
    cout << "vec len " << vec.size() << endl;
    for (auto s : vec )
    {
        auto s_bndl = s->getOSCBundle();

        for ( auto osc : s_bndl )
        {
            OSCMessage msg = osc.getMessage();
            
            String newaddr = prefix + String(count) + msg.getAddressPattern().toString();
            
            msg.setAddressPattern(newaddr);
            
            bndl.addElement(msg);

        }
        
        count++;
    }
    
//    printBundle(bndl);
    
    return symbolBundleToOdot( bndl );
}



odot_bundle *TimePointArray::getSymbolsAtTime( float t )
{
    bool match;
    int idx = getTimePointIndex( t, match );
    
    if( match )
    {
        auto symbs = (*this)[idx]->symbols_at_time;
        cout << "(maptch) timepoint: " << idx << " at " << (*this)[idx]->time << " with: " << symbs.size() << " overlaping" << endl;
        return symbolVectorToOSC( symbs );
    }
    else if( idx > 0 )
    {
        auto symbs = (*this)[idx-1]->symbols_at_time;
    cout << "timepoint: " << idx << " at " << (*this)[idx-1]->time << " with: " << symbs.size() << " overlaping" << endl;
        return symbolVectorToOSC( symbs );
    }
    
    cout << "timepoint: -1" << endl;

    
    return nullptr;
}

int TimePointArray::getTimePointIndex( float t, bool& match )
{
    match = false;
    
    int firstElement = 0, lastElement = size();
    while (firstElement < lastElement)
    {
        if (compareTimes ( (*this)[firstElement]->time, t ) == 0)
        {
            match = true;
            break;
        }
        else
        {
            const int halfway = (firstElement + lastElement) >> 1;
            
            if (halfway == firstElement)
            {
                if (compareTimes ((*this)[firstElement]->time, t ) >= 0)
                {
                    ++firstElement;
                }
                break;
            }
            else if (compareTimes ((*this)[firstElement]->time, t ) >= 0)
            {
                firstElement = halfway;
            }
            else
            {
                lastElement = halfway;
            }
        }
    }
    
    return firstElement;
}
