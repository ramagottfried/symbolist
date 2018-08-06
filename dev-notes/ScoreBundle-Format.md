
* [Symbol Notation](#symbol-notation)
* [OSC Notation](#osc-notation)
* [Symbolist Naming Conventions](#symbolist-naming-conventions)
* [Symbolist File Format](#symbolist-file-format)
    * [Layout](#layout)
    * [Score](#score)
        * [Time assignment through Symbol / Stave / System linkage](#time-assignment-through-symbol-stave-system-linkage)
    * [Palette](#palette)
        * [Symbol Prototype](#symbol-prototype-reference)
        * [Group Symbol](#group-symbol)
        * [Parameter and Mapping Expressions](#parameter-and-mapping-expressions)
* [Symbol Graphics](#symbol-graphics)
    * [/graphic](#graphic)
    * [/bounds](#bounds)

# Symbol Notation
A `Symbol` in Symbolist is an Odot-OpenSoundControl (OSC) bundle, used as a container for graphic and parameter values. Through a set of semantic rules and organizing principles, Symbolist sets up methods for composing and streaming OSC parameters in real time.

Using Odot we are able to take advantage of sub-bundle hierarchies, and make use of Odot's expression language for composing scripts for input and output mappings.

### OSC Notation
An Odot-OSC bundle is notated in a similar way to JSON, using `:` to define address/value pairs. In OSC all addresses must begin with a `/` forward slash.


Lists are indicated as a comma separated sequence of values within square brackets `[ 0, 1, 2 ]`. A *sub-bundle* is notated by `{ }` with comma-separated OSC messages inside. In Odot, all sub-bundles must be assigned to an address. The top-level, parent bundle is notated by an anonymous outer bundle `{ }` (not assigned to an address.). For example, here is a bundle containing two OSC messages:

```
{
    /addr/1 : 1,
    /addr/2 : 2    
}
```

The central data structure used for organizing Symbols is a list of Symbol sub-bundles, with addresses indicating their z-layer drawing order (`/1`, `/2`, ...etc.).

```
{
    /symbol : {
        /1 : {},
        /2 : {},
        /3 : {},
        [...]
    }    
}
```

# Symbolist Naming Conventions
In Symbolist there is a set of basic objects that can be used in different ways, for convenience we are using a typical musical score naming scheme (but of course the "score" might not look or act anything like a musical score!).

The basic objects are:
* `Symbol` : the root graphic type
* `Stave` : a container object that contains Symbols and defines how to interpret them. Additionally, Staves have their own graphic Symbols as well (e.g. a Clef and Staff lines).
* `System` : a set of one or more Staves which are synchronized in time. The System acts as the top-level time holding object, Staves and Symbols define their time settings in relation to the System.
* `Page` : a set of one or more Systems that are though of as a printed page, or screen size.
* `Palette` : a set of prototypes for that can be a Symbol, Group, or Stave.
* `Group` : is a type of Symbol which functions as a multi-Symbol container, which are handled together as single Symbol on the Stave. For example a "note" group might contain notehead + stem + beam sub-symbols.

# Symbolist File Format

The Score format is a single bundle that contains sub-bundles which set different aspects of the parsing and execution of the Score. The main sections in the score bundle are:

```
{
    /layout : {},
    /score : {},
    /palette : {}

}
```

* `/layout` holds definitions for the page size, margins and other top-level display settings.
* `/score` is the container and graphic Symbols and Stave/Symbol hierarchies.
* `/palette` is a sub-bundle containing Stave and Symbol prototypes. Note that Staves may also contain a contextual Palette.

## Layout
In the Score format, the empty screen is though of as a `/page` container of the symbols. The `/layout` section sets the page (canvas) size, and sets some margin values that add a buffer around the edges of the page for printing purposes. The basic setup would be something like:

```
/layout : {
    /page : {
        /size : [600, 1600],
        /margin : {
            /left : 10,
            /right : 10,
            /top : 10,
            /bottom : 10
        }
    }
}
```
Larger scores may have multiple `Pages`, with multiple `Systems` per page, and multiple `Staves` per `System`.

Eventually we might add a default Stave layout on a System:
```
/system : {
    /margin/topbottom : [20, 20],
    /staves : {
        /piccolo : {
            /margin : [10, 10],
            /name : "/piccolo"
        },
        /oboe : {
            /margin : [10, 10],
            /name : "/oboe"
        },
    }
}
```

## Score
The `/score` sub-bundle is where the Symbols displayed on the Page are laid out. Each parent object can contain one or more child objects.

`Score` : `Page` : `System` : `Stave` : `Symbol`

Child objects are defined as a list of Symbol sub-bundles, with addresses indicating their z-layer drawing order (`/1`, `/2`, ...etc.).

```
/score : {
    /page : {
        /1 : {
            /system : {
                /1 : {
                    /stave : {
                        /1 : {
                            /symbol : {
                                /1 : {},
                                /2 : {},
                                /3 : {},
                                [...]
                            }
                        }
                    }
                }
            }
        }
    }    
}
```

#### Time assignment through Symbol / Stave / System linkage
Symbols are linked to their containing Stave through their placement in the bundle hierarchy. Likewise, Staves are attached to their containing System.

Systems, are the top-level time reference, and may only contain Staves. Timed Symbols cannot be placed directly into a System, but require a Stave to be timed. When a Stave is placed in a Score it should always have a System, and when editing in the Symbolist GUI, this will be automatic.

Untimed Symbols may be added to a System as a general graphic object but will not be part of the Time lookup system.

Timed objects (System, Stave, Symbol, Group) should contain a `/time` bundle which defines the object's start and end times:

```
/time : {
    /start : 0,
    /end : 1
}
```

In most cases, the Stave will be the same duration as the System. However, in complex score Staves could be of different lengths, or start/stop mid-System.

A simple illustration of System, Stave and Symbol `/time` bundles:

```
/score : {
    /page : {
        /1 : {
            /system : {
                /1 : {
                    /time : {
                        /start : 0,
                        /end : 10
                    },
                    /stave : {
                        /1 : {
                            /time : {
                                /start : 0,
                                /end : 10
                            },
                            /symbol : {
                                /1 : {
                                    /time : {
                                        /start : 5,
                                        /end : 6
                                    },
                                /2 : {
                                    ... etc.
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

```

See the [Symbol Graphics](#symbol-graphics) section for more information about Symbol data.

## Palette
The `/palette` sub-bundle defines a set of contextual prototypes that can be used in the Score.

Since Staves are containers for Symbols, and determine how a given Symbol is interpreted. A Stave prototypes also contains Symbol prototypes, which include a set of input and output scripts that map between graphic and control parameters, relative to the Stave. The `/script` mappings can be thought of as a kind of notation key for a custom clef and staff (see [Parameter and Mapping Expressions](#parameter-and-mapping-expressions) below for more information).

The format for a Stave/Symbol prototype is as follows:

```
/palette : {
    /exampleStavePrototype : {
        /name : "/exampleStavePrototype",
        /type : "stave",
        /graphic : {},
        /palette : {
            /exampleSymbolPrototype : {
                /name : "/exampleSymbolPrototype",
                /type : "symbol",
                /graphic : {},
                /script : {
                    /param : {},
                    /set/fromOSC : "lambda([stave, t], ... )",
                    /set/fromGUI : "lambda([stave, mouse], ... )",
                    /get : "lambda([stave, t], ... )"
                }
            }
        }
    }
}
```

The master `/palette` sub-bundle is at the root level of the bundle namespace (as noted above). Within the `/palette` an array of sub-bundles define the prototypes, listed by name as an OSC address. In this example `/exampleStavePrototype` is the name of the object.

The basic `Stave` prototype contains:
* `/name` : provides the name of the object prototype for later lookup.
* `/type` : sets the object type, current types are "symbol", "stave", and "group".
* `/graphic` : the graphic drawing element defaults (described below)
* `/palette` : a contextual palette of objects which have mappings related to this Stave.

Within the Stave's `/palette` sub-bundle, there are prototypes for Symbols which function within this Stave.

The basic `Symbol` prototype contains:
* `/name` : provides the name of the object prototype for later lookup.
* `/type` : "symbol" or "group"
* `/graphic` : the graphic drawing element defaults (described below)
* `/param` : a sub-bundle holding user defined parameters that are mapped to and from graphic data as defined in the `/script` functions.
* `/script` : `/param`, `/set` and `/get` functions.
    * The `/set/fromOSC` functions takes the relative Stave object and time `time` as arguments, and defines the Input mapping from Parameter values to Graphic values.
    * The `/set/fromGUI` functions takes the relative Stave object and `mouse` information arguments, and defines the Input mapping from Parameter values to Graphic values.
    * The `/get` function takes the relative Stave object and the current time (`time`) as arguments, and defines the Output mapping, from Graphic values to Parameter values.

### Symbol Prototype Reference
When a Symbol is looked up in the score it can be found by sub-bundle reference, for example: `/exampleStavePrototype./exampleSymbolPrototype` would be the address of the prototype within the `/palette` sub-bundle.

### Group Symbol
In the case of `Group` Symbols there is an additional sub-bundle:
* `/subsymbol` : a set of Symbols that are grouped together.

In the case of Group Symbols, the top-most symbol will contain the `/script` bundle pertaining to all of its the sub-symbols.

```
/exampleGroupPrototype : {
    /name : "/exampleGroupPrototype",
    /type : "group",
    /graphic : {},
    /param : {},
    /script : {
        /set/fromOSC : "lambda([stave, time], ... )",
        /set/fromGUI : "lambda([stave, mouse], ... )",
        /get : "lambda([stave, time], ... )"
    },
    /subsymbol : {
        /notehead : {
            /type : "symbol",
            /param : {},
            /graphic : {},
        },
        /stem : {
            /type : "symbol",
            /param : {},
            /graphic : {},
        }
    }
}
```

### Parameter and Mapping Expressions

The ideal for Symbolist is that Symbols should be thought of as representing semantic parameters which can be used for controlling synthesis, or other electronic processes. The `/script` sub-bundle defined in the Palette prototype provides a means to define input and output processes, and other behaviors using the Odot Expression Language. (see `o.expr.codebox`).

The `/param` sub-bundle sets the default parameter values.

`/script` expressions are evaluated within the scope of the Symbol, with the relative `stave`, `mouse` and `time` passed in as function arguments.

The `/set` functions uses `/param` values to set Graphic values, and may be used to manipulate other graphic information within the Symbol.
* `/set/fromOSC` function defines how the graphic values should be configured based on the reference stave and the time point for the symbol, with the function signature: `lambda([stave, time], ... )`
  * The `stave` argument is a sub-bundle containing the relative Stave that the Symbol is attached to.
  * The `time` argument is a floating point value of the lookup time.
* `/set/fromGUI` function defines how the graphic values should be configured based on the reference stave and the mouse event information, with the function signature: `lambda([stave, mouse], ... )`
  * The `mouse` argument is a sub-bundle containing the mouse event information, with the messages:
    * `/x` and `/y` : the mouse position coordinates
    * `/state` : the mouse state, possible values are: `down`, `drag`, `up`
    * `/origin` : the mouse position on mouse down, can be used to determine drag distance and angle

The `/get` function uses Graphic information to produce parameter values.


```
/exampleSymbol : {
    /type : "symbol",
    /name : "/exampleSymbol",
    /graphic : {},
    /param : {
        /duration : 1,
        /pitch : 60,
        /amp : 0.5
    },
    /script : {
        /set/fromOSC : "lambda([stave, time],
            /bounds./x = stave./x + ( (time - stave./time/start) * stave./time/timePixScale ),
            /bounds./y = /stave./y + scale(/param./pitch, 0, 127, 0, stave./h),
            /bounds./w = /param./duration * stave./time/timePixScale,
            /graphic./style./stroke_width = scale( /param./amp, 0, 1, 0, 10),
            /subsymbol./notehead./bounds./x = /x,
            /subsymbol./stem./bounds./y = /y
        )",
        /set/fromGUI : "lambda([stave, mouse],
          if( mouse./state == "down",
            progn(
              /bounds./x = mouse./x,
              /bounds./y = mouse./y,
              /bounds./w = /param./duration * stave./time/timePixScale,
              /graphic./style./stroke_width = scale( /param./amp, 0, 1, 0, 10),
              /subsymbol./notehead./bounds./x = /bounds./x,
              /subsymbol./stem./bounds./y = /bounds./y
            )
          )
        )",
        /get : "lambda([stave, time],
            /param./pitch = scale(/bounds./y, 0, stave./h, 0., 127.),
            /param./relative/time = time,
            /param./amp = scale( /graphic./style./stroke_width, 0, 10, 0, 1)
        )"
    }    
}

```

Each Symbol of a given type shares the same `/script` expressions. The Palette prototypes are used as reference when processing input and output bundles.  Therefore, the `/script` sub-bundle is not included in the `/score` hierarchy.


## Symbol Graphics
Symbol, Stave and System bundles will contain a `/graphic` sub-bundle which defines the object's graphic drawing routine, using SVG style drawing parameters, and a `/bounds` sub-bundle with the bounding-box size and position.

### `/graphic`
The `/graphic` sub-bundle contains the drawing information for the Symbol.

There are essentially three types of graphic Symbols: `shape`, `text`, and `path`.

In the cases of `shape` and `text` the graphic drawing routine will stay the same, even if the Symbol is transformed by stretching or rotating. In these cases the default path data address `/d` can be used from the Symbol `/palette` prototype without copying the data into the `/score`.

We use the name `path` to describe graphic objects that are potentially different in every instance, for example representation of bow movement over time, or a trajectory through space. In these cases, the `/d` path data value *is* included within the Symbol bundle in the Score (since potentially each version is different, but they are all the same type of object).

Note that the Symbolist `/graphic` coordinate system is relative to the parent bounding box, not to the top-level Page component. For this reason, the SVG drawing elements will have 0,0 as their top-left corner.

Within the `/graphic` bundle there are the following messages:

* `/d` : a SVG format "path data" string, see the [SVG Path Specification](https://www.w3.org/TR/SVG/paths.html) for more details. All graphic shapes are able to be converted to a Path.

* `/text` : a sub-bundle, with drawing parameters (currently in development)
  * `/string` : the string that will be displayed.
  * `/tspan` : an optional SVG text control, see [SVG TSpan](https://www.w3.org/TR/SVG/text.html#TSpanElement)<br><br>

* `/transform` : a transformation matrix, as a list of six numbers `[a, b, c, d, e, f]`, see [SVG Transformation Matrix](https://www.w3.org/TR/SVG/coords.html#TransformAttribute) for more details.

* `/style` : a sub-bundle containing CSS style information which is applied to the SVG object. See [SVG Styling](https://www.w3.org/TR/SVG/styling.html) for more details. <br><br>
*Note: The Odot Expression Language uses the `-` sign for subtraction, so use an `_` (underscore) in place of `-` (hyphen) for SVG/CSS style attribute names that have a `-` in them, for example `stroke-width`, becomes `/stroke_width` etc.*

### `/bounds`
The `/bounds` sub-bundle is separate from the `/graphic` information, and acts to place the `/graphic` graphic commands in the JUCE graphic user interface (*post SVG transform rendering*).

`/bounds` : a sub-bundle containing the bounding-box positioning values:
* `/x` : the leftmost point of the object's bounding box
* `/y` : the topmost point of the object's bounding box (where `0` is the top of the parent component)
* `/w` : the width of the component
* `/h` : the height of the component

*Note: the bounding box should generally be created algorithmically by one of the `/set` functions*

In the palette prototype for a graphic object, a default bounds aids in scaling the graphic if necessary (e.g. a stave will normally need to be scaled to span the dimensions of the time duration). ---> or if I can get the path bounds put into the score/model then we can probably autogenerate that? ...


## example

```
/palette : {
  /exampleStavePrototype : {
    /name : "/exampleStavePrototype",
    /type : "stave",
    /time : {
      /timePixScale : 100.
    },
    /bounds : {
      /x : 0,
      /y : 0,
      /w : 10,
      /h : 10
    }
    /graphic : {
      /type : "shape",
      /d : "M0,0 L0,10 L10,10 L10,0 Z",
      /transform : [],
      /style : {
        /fill : "none",
        /stroke : "#000000",
        /stroke_miterlimit : 10
      }
    },
    /script : {
      /set/fromOSC : "lambda([system, time],
          /bounds./x = system./x + ( (time - system./time./start) * system./time./timePixScale ),
          /bounds./y = system./y + scale(/param./pitch, 0, 127, 0, stave./h),
          /bounds./w = /param./duration * system./time./timePixScale,
          /graphic./style./stroke_width = scale( /param./amp, 0, 1, 0, 10)
      )",
      /set/fromGUI : "lambda([system, mouse],
        if( mouse./state == "down",
          progn(
            /bounds./x = mouse./x,
            /bounds./y = mouse./y,
            /bounds./w = /param./duration * system./time./timePixScale,
            /graphic./style./stroke_width = scale( /param./amp, 0, 1, 0, 10)
          )
        )
      )",
      /get : "lambda([system, time],
          /param./pitch = scale(/bounds./y, 0, stave./h, 0., 127.),
          /param./relative/time = time,
          /param./amp = scale( /graphic./style./stroke_width, 0, 10, 0, 1)
      )"
    }
    },
    /palette : {
      /exampleSymbolPrototype : {
        /name : "/exampleSymbolPrototype",
        /type : "symbol",
        /graphic : {
          /type : "shape",
          /d : "M204.7-0.7C119.3,53.7-19.8-118.2,2.2,201.8c79.7-22.7,156-46.9,202-84c9.2-36.7,6.8-77.2,0-119",
          /style : {
            /fill : "none",
            /stroke : "#000000",
            /stroke_miterlimit : 10,
            /stroke_width : 1
          }
        },
        /param : {
            /duration : 1,
            /pitch : 60,
            /amp : 0.5
        },
        /script : {
          /set/fromOSC : "lambda([stave, time],
              /bounds./x = stave./x + ( (time - stave./time./start) * stave./time./timePixScale ),
              /bounds./y = /stave./y + scale(/param./pitch, 0, 127, 0, stave./h),
              /bounds./w = /param./duration * stave./time./timePixScale,
              /graphic./style./stroke_width = scale( /param./amp, 0, 1, 0, 10)
          )",
          /set/fromGUI : "lambda([stave, mouse],
            if( mouse./state == "down",
              progn(
                /bounds./x = mouse./x,
                /bounds./y = mouse./y,
                /bounds./w = /param./duration * stave./time./timePixScale,
                /graphic./style./stroke_width = scale( /param./amp, 0, 1, 0, 10)
              )
            )
          )",
          /get : "lambda([stave, time],
              /param./pitch = scale(/bounds./y, 0, stave./h, 0., 127.),
              /param./relative/time = time,
              /param./amp = scale( /graphic./style./stroke_width, 0, 10, 0, 1)
          )"
        }
      }
    }
  }
},
/score : {
  /page : {
    /1 : {
      /system : {
        /1 : {
          /time : {
            /start : 0,
            /end : 8
          },
          /stave : {
            /1 : {
              /name : "/exampleStavePrototype",
              /time : {
                /start : 0,
                /end : 8
              },
              /symbol : {
                /1 : {
                  /name : "/exampleSymbolPrototype"
                  /time : {
                    /start : 5,
                    /end : 6
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

```

**Dev notes:**
* How to deal with more advanced mouse interaction?
* should `/time` be part of the `/param` bundle?
* should `/param` be a top-level Symbol sub-bundle? ... I think so.
