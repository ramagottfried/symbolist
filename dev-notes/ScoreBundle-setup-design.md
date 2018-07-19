Symbol-Component
when a new symbol/component is created:
1. copy the default bundle prototype from the palette
2. update the prototype based on the mouse interaction details (position etc.)
3. set z-order id for the symbol as unique id connection for component->symbol lookup
4. add symbol to score /symbol list with /number as id

when a component is altered in the GUI:
1. use the id in the component to lookup the symbol-bundle in the score, which returns a copy of the previous value
2. update (union or replace?) the symbol with the component's new location, rotation, color etc.
3. add symbol back into score, replacing previous version

if the z-order of a component changes:
1. reorder the addresses of the bundle to match the order of the component stack
    (how to do that exactly I'm not sure yet)


Timepoint Array
after moving to a single OSC bundle the symbol locations will be slightly harder to find.
option 1: copy the symbol values into the timepoints themselves
option 2: store the names of the symbols and use a hashtable to look up the symbol bundle every time you need it
option 3: store the t_osc_bndl_u pointer instead of the Symbol (since the Symbol is always a copy),
option 4: use a SymbolRef which wraps the pointer but doesn't free it (maybe just release() the unique_ptr in the destructor?)


Time / Stave array setup:
when ever a symbol changes we should rebuild the time sequence -- this might add a little overhead in comparison to adding/removing symbols, but it will likely be more stable and make the code easier to read.

SortedStaves
use hash table to store staves
do they need to be in order?

Time calc process:
1. connect Staves in time and set /time bundle in stave symbols
    1. find all staves and put them into order based on sorting rules
    2. set start and end times (also duration?) for each stave accumulating time from the previous staves
    note: this sorted vector doesn't really need to be saved if we are recalculating everything whenever a new symbol is added
2. find all Symbols that are linked to staves and set the symbol /time bundle
3. create TimePoint Array to optimize lookup into score sequence
