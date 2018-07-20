#pragma once

#include "OdotBundle.hpp"
#include <unordered_map>

using namespace std;

// check on address syntax, might need to concatentate with '.'

/**
 *  @class OdotSelect
 *
 *  Creates a hash table (unordered map) of OSC Messages.
 *  Optionally will return a vector of OdotMessages from the hash table.
 *  If an OdotMessage is passed to the select function, the selection will be of the parent subbundle message address.
 *
 *  OdotSelect hashes t_osc_message_u * since the pointer address stays stable using the replace_message function, however,
 *  OdotSelect is storing pointers, so remember that the pointers may not be valid if messages are removed.
 *  For safety, run the select() function immediately after any change is made.
 */

class OdotSelect
{
public:
    
    OdotSelect( OdotBundle& bndl ) : m_bndl(bndl) {}
    ~OdotSelect() {}
    
    /**
     * Select all messages and store in hash table for fast lookup.
     *
     */
    void select();
    
    /**
     * Select all messages with a given address prefix
     *
     * @param   addr_prefix     prefix to select, e.g. /foo will match any address starting with /foo
     *
     */
    void select( const string& addr_prefix, bool fullmatch = 0 );
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
     * Gets message associated with the address key.
     *
     * @param addr       the address to look for.
     *
     * @return           <code>OdotBundle</code>, associated with the address;
     */
    OdotMessage get( const string& addr )
    {
        return OdotMessage( m_map[ addr ] );
    }
    OdotMessage operator[]( const string& addr )
    {
        return OdotMessage( m_map[ addr ] );
    }
    
    /**
     * Gets vector of OdotMessages in hash table
     *
     * @return           <code>vector<OdotMessages></code>
     */
    vector< OdotMessage > getVector();
    
    /**
     * Gets vector of OdotBundle with OdotMessages selected by hash table
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
    
    void add( const string& addr, t_osc_msg_u * msg )
    {
        // note: we need to avoid adding the same entry twice...
        m_map.emplace( addr, msg );
    }
    
    void recursiveSelect( const t_osc_bndl_u *bndl );
    void recursiveSelect( const t_osc_bndl_u *bndl, const string& selector );
    void recursiveSelect( const t_osc_bndl_u *bndl, t_osc_msg_u * parent, const OdotMessage& select_msg );
    
    // add prefix selector?
    string                                      m_selector;
    OdotMessage                                 m_msg;
    OdotBundle&                                 m_bndl;
    unordered_map< string, t_osc_msg_u * >      m_map;
};

