# Symbol-Component
when a new symbol/component is created:
1. copy the default bundle prototype from the palette
2. update the prototype based on the mouse interaction details (position etc.)
3. set z-order layer id for the symbol
4. add symbol to score /symbol list with /number as id

when a component is altered in the GUI:
1. use the id in the component to lookup the symbol-bundle in the score, which returns a copy of the previous value
2. update (union or replace?) the symbol with the component's new location, rotation, color etc.
3. add symbol back into score, replacing previous version

if the z-order of a component changes:
1. reorder the addresses of the bundle to match the order of the component stack
    (how to do that exactly I'm not sure yet)

# Symbol / Stave


# Add Symbol
based on the symbol/stave relation, adding a symbol will be one of these processes:

### 1. Add from GUI without Stave/System Reference -> Purely Graphic
If no `/staff` is defined the Symbol is considered strictly graphic, and no `/time` information is added.

### 2. Add from GUI With Stave/System Reference -> Generate Time
When a symbol is added from the GUI and has a Stave reference, the `/time` must be calculated, using the reference Stave instance (a specific Stave Symbol).

If a Stave is added to a System:
1. Get System bundle for coordinates and time information.
    * *Note: For for the quick dev version, use main PageComponent as System (i.e. time will start at the left edge of the component).*
2. Set /time information for Stave based on Symbol's placement relative to the System.

When Stave is selected (ctrl-cmd click?) Palette changes to show Stave Symbols for this stave. You can also make new Symbols in this mode, which then get stored into the Stave Palette.

If a Symbol is added to a Stave:
1. if not already present, set /staff name in Symbol to reference in palette
2. union with Stave Symbol that will be used for relative calculations.
3. check to make sure that the Symbol is defined in the Stave Palette (i.e. has a mapping), if not, highlight the symbols and print an error message notifying the user that the Symbols are not recognized by the Stave. Provide mechanism to copy the mapping from one stave to another based on the definition in the Symbol's /stave id.
4. execute View-Controller interface if there is any custom UI for the Symbol creation routine in the Symbol Prototype definition.
5. after ViewController UI processes, add Symbol to score:
6. get /time information from Stave
7. set /time information for Symbol
8. add Symbol as top layer within Stave

### 3. Add from OSC input with `/time` -> Generate Graphics
Symbols that have a `/time` value, can be added to the appropriate Stave instance based on the Stave's `/time` value. The Symbol's input `/time` value will be used to position the symbol graphically based on the `/set` expression, similarly if there are other mapping parameters, use the `/set` function to set graphics from parameters.

If a `/layer` value is set, the Symbol will replace (union with?) the Symbol currently at that layer. If no `/layer` the Symbol is added to the end of the list.

### 4. OSC input without `/time` -> Insert Graphic without Time
Purely graphic addition, no stave information will be processed.

If a `/layer` value is set, the Symbol will replace (union with?) the Symbol currently at that layer. If no `/layer` the Symbol is added to the end of the list.

### 5. OSC input from Symbolist Score File (or Application State change)
When processing a Score from a file, we should attempt to do the same process as above: if there is no `/time` value, calculate the `/time`, if there is `/time` but no graphic positioning, set the position of the Symbol based on the `/set` function for the Symbol found in the Stave/Palette.



# Timepoint Array



# Time / Stave array setup:
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

*actually, staves may not need the /staff/id tag in their bundle. The linkage is only useful when finding the /time values for a given symbol from a graphic manipulation.*

# New Time Calc design:
1. Symbols may have a /staff address that specifies which type of staff to attach to. Only one type of staff may be happening at the same time. A System type can specify a group of Stave names to synchronize for time setup.

# New setup without /ids:
To attach to a staff, Symbols must have either a /staff name and /time/start, or /staff and /id.
the /id will be removed after the /time is calculated since the ids could change based on layer order, add/removing symbols.

#### Problem 1: what about the bounds? if there is a /time value, but it doesn't match the bounds?
    Answer: this is an invalid Symbol.

#### Problem 2: if there are no /id's how can we translate/alter/remove an existing Symbol?
    Answer: the identifier can still be the /layer. If you send in a /symbol/1 or /symbol : { /1 ...etc } bundle it will replace any matching layers.

#### Problem 3: what if a Stave is sent in with a score, and has /time information, but no graphic position information?
    Answer options:
        1. this is an invalid Stave. Generally speaking scores should be created in the Editor and therefore have position information.
        2. auto calculate Stave position based on positioning rules:
            1. the Score should have a Page Size, Margins, and line-break rules.
                * Page Size, Page Number location
                * Margin (Left, Right, Top, Bottom)
                * System Spacing
                This could all be CSS.
            2. note also the Stave could be auto created when a symbol has a time element and set/parameters that determine its position.

*actually the system maybe should hold the time information, since there could be multiple staves, and the staff/clef mapping could change mid-system*


Maybe the staff/clef should contain scripts to position graphic objects, and then the score symbols don't contain their own bounds.

# System holds Time
To do later, add /system is a list of stave types that get grouped together? like /system : ["foo", "bar"]
* the System connects Staves.
* Staves have Clefs which define the Symbol Mapping to control parameters
* Systems define the time flow logic, and therefore all Staves within a System should use the same logic?
    - for example, if there was a 2D space that was using a lookup point cursor, the System would not actually need a time mapping, since there is no time lookup
    - if there are Staves in the System, they need to define how they sit in the System's Time flow
        * for example, Laban Notation might have Left and Right sides
    Somewhat like Iannix, a Cursor moves along a System, reading the contents, the cursor might

* Specifying the margins and spacing will make it possible to insert time points and generate the system automatically.
#### Problem: what about override situations?
    not sure. maybe there should be just a default position in the case where it gets autocreated, but then if you make a stave, it's position is just relative to the System like a Symbol.

* System will be a Component that has children Components, the bounds of the System will be hard, with no overflow.

*see osc-design2.md*

* Stave sorting will become System sorting, since the Stave exists within the System. Staves can remap the time, but should be relative to the global (possibly absolute) time.

#### Problem: what about the position of the Stave within the System? What marks the beginning of the time in the System?
    Option: the Clef could have a mapping that defines a time offset -- if multiple Clefs are there, then use the widest? but then you have to go through and check each one which might not be that fun.
    *For simplicity we can start by not switching Staves mid-system. And then add that functionality later.*
* System will need a bracket or some kind of graphic

##### Notes:
Maybe a Stave is the parent container (group) for a set of Symbols, and a System is a container for a set of Staves. In this case the bounds of the Stave would need to be adapted to contain the placement range of the Symbols. Or at the minimum, the Symbol would need to be able to locate the associated Stave based on the /time value. In this case, does the Symbol need to hold the /time anymore? maybe still good to keep since that makes it more human readable. Or then: does it need the graphic information? or only the semantic values?


# Palette:
* on empty score, the Palette shows:
    1. pure graphic objects with no mapping, (can also be user created)
    2. Stave objects that have mappings associated (with defined symbol types and input/output mappings)
* With a Stave selected, you can add predefined Symbols to the Stave. The Palette shows a selection of predefined types, and offers an option to create a new Stave Symbol. The Symbol details are saved as a Palette within the Stave object.


# Memory Design:
Avoid storing values that might get out of sync from Score i.e. timepoints, staves, etc.
TimePoints are a necessary evil in order to make a fast lookup for streaming OSC, but processes that only happen when Symbols are added/removed can be slower, and safer.
