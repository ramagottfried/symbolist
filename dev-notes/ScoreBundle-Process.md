# Add Symbol
adding a symbol will be one of these processes, based on the symbol/stave relation, and whether the Symbol was added from the GUI or via OSC input.

## Add from GUI
1. *<u>Without</u>* Stave/System Reference -> Purely Graphic.

    If no `/staff` is defined the Symbol is considered strictly graphic, and no `/time` information is added.

2. *<u>With</u>* Stave/System Reference -> Generate Time.

    When a symbol is added from the GUI and has a Stave reference, the `/time` must be calculated, using the reference Stave instance (a specific Stave Symbol).

##### When a Stave is added to a System from the GUI:
1. Get System bundle for coordinates and time information.
    * *Note: For for the quick dev version, use main PageComponent as System (i.e. time will start at the left edge of the component).*
2. Set `/time` information for Stave based on Symbol's placement relative to the System.

When Stave is selected (ctrl-cmd click?) Palette changes to show Stave Symbols for this stave. You can also make new Symbols in this mode, which then get stored into the Stave Palette.

##### When a Symbol is added to a Stave:
1. if not already present, set `/staff` name in Symbol to reference in palette
2. union with Stave Symbol that will be used for relative calculations.
3. check to make sure that the Symbol is defined in the Stave Palette (i.e. has a mapping), if not, highlight the symbols and print an error message notifying the user that the Symbols are not recognized by the Stave. Provide mechanism to copy the mapping from one stave to another based on the definition in the Symbol's /stave id.
4. execute View-Controller interface if there is any custom UI for the Symbol creation routine in the Symbol Prototype definition.
5. after ViewController UI processes, add Symbol to score:
6. get /time information from Stave
7. set /time information for Symbol
8. add Symbol as top layer within Stave

### 3. Add from OSC input *with* `/time` -> Generate Graphics
Symbols that have a `/time` value, can be added to the appropriate Stave instance based on the Stave's `/time` value. The Symbol's input `/time` value will be used to position the symbol graphically based on the `/set` expression, similarly if there are other mapping parameters, use the `/set` function to set graphics from parameters.

If a `/layer` value is set, the Symbol will replace (union with?) the Symbol currently at that layer. If no `/layer` the Symbol is added to the end of the list.

### 4. OSC input *without* `/time` -> Insert Graphic without Time
Purely graphic addition, no stave information will be processed.

If a `/layer` value is set, the Symbol will replace (union with?) the Symbol currently at that layer. If no `/layer` the Symbol is added to the end of the list.

### 5. OSC input from Symbolist Score File (or Application State change)
When processing a Score from a file, we should attempt to do the same process as above: if there is no `/time` value, calculate the `/time`, if there is `/time` but no graphic positioning, set the position of the Symbol based on the `/set` function for the Symbol found in the Stave/Palette.
