#pragma once

#include "OdotBundle.hpp"
#include <unordered_map>

using namespace std;

// add separate message hash?

class OdotBundleHash
{
public:
    
    OdotBundleHash(OdotBundle& bndl, const string& prefix_selector = string() ) :
        m_selector(prefix_selector),  m_bndl(bndl)
    {
        rehash();
    }
    
    /**
    * Peform iteration of bundle and add all subbundles to hash table.
    *
    */
    void rehash();

    /**
     * Gets subbundle associated with the address key.
     *
     * @param addr       the address to look for.
     *
     * @return           <code>OdotBundle</code>, associated with the address;
     */
    OdotBundle get( const string& addr )
    {
        return OdotBundle( m_map[ addr ] );
    }
    
    void print()
    {
        cout << "hash map:" << endl;
        for( auto e : m_map )
        {
            cout << e.first << " " << e.second << endl;
        }
    }
    
    vector<OdotBundle> getVector()
    {
        vector<OdotBundle> vec;
        for( auto e : m_map )
        {
            vec.emplace_back( OdotBundle(e.second) );
        }
    }
    
private:
    
    void recursiveAddSubs( const t_osc_bndl_u *bndl );
    
    void add( const char * addr, t_osc_bndl_u * bndl )
    {
        m_map.emplace( string(addr), bndl );
    }
    
    // add prefix selector?
    string                                          m_selector;
    OdotBundle&                                     m_bndl;
    unordered_map< string, t_osc_bndl_u * >   m_map;
};
