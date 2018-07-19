#pragma once

#include "OdotBundle.hpp"
#include <unordered_map>

using namespace std;

// add separate message hash?

// check on address syntax, might need to concatentate with '.'

// hash should probably be part of the Bundle since it's storing pointers

class OdotBundleHash
{
public:
    
    OdotBundleHash( OdotBundle& bndl ) : m_bndl(bndl) {}
    ~OdotBundleHash() {}
    
    /**
    * Select all subundles for hash table.
    *
    */
    void select();

    /**
     * Select all subundles with a given address prefix
     *
     * @param   addr_prefix     prefix to select, e.g. /foo will match any address starting with /foo
     *
     */
    void select( const string& addr_prefix, bool fullmatch = 0 );
    
    /**
     * Select all subundles with a given address prefix
     *
     * @param   addr_prefix     prefix to select, e.g. /foo will match any address starting with /foo
     *
     */
    void select( const char * addr_prefix, bool fullmatch = 0 )
    {
        select( string(addr_prefix), fullmatch );
    }
    
    /**
     * Select all subundles with a given OdotMessage (address and value)
     *
     * @param   select_msg     OdotMessage to select, e.g. /foo : 1 will match any address starting with /foo that has the value of 1
     *
     */
    void select( const OdotMessage& select_msg, bool fullmatch = 0 );
    
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
    
    /*
    OdotBundle operator[]( const int i )
    {
        return OdotBundle( m_map[ addr ] );
    }
    */
    
    /**
     * Gets vector of Bundles in hash table
     *
     * @return           <code>vector<OdotBundle></code>
     */
    vector< pair<string, OdotBundle> > getVector();
    
    /**
     * Gets vector of Bundle with subbundles selected by hash table
     *
     * @return           <code>OdotBundle</code>
     */
    OdotBundle getBundle();

    inline size_t size() const { return m_map.size(); }
    
    void print()
    {
        cout << "hash map:" << endl;
        for( auto e : m_map )
        {
            cout << e.first << " " << e.second << endl;
        }
    }
    
private:
    
    void add( const string& addr, t_osc_bndl_u * bndl )
    {
        // note: we need to avoid adding the same entry twice...
        m_map.emplace( addr, bndl );
    }
    
    void recursiveSelect( const t_osc_bndl_u *bndl );
    void recursiveSelect( const t_osc_bndl_u *bndl, const string& selector );
    void recursiveSelect( const t_osc_bndl_u *bndl, const string& bndl_addr, const OdotMessage& select_msg );
    
    // add prefix selector?
    string                                      m_selector;
    OdotMessage                                 m_msg;
    OdotBundle&                                 m_bndl;
    unordered_map< string, t_osc_bndl_u * >     m_map;
};
