```
{
    /palette : {
        /line : {
            /name : "line",
            /type : "path",

            # SVG drawing information:
            /svg : {
                /type : "path",
                /d : "M0,0 L100,100 Z",
                /style : {
                    /color : "red",
                    /stroke_width: 1
                }
            },

            # default JUCE positioning information
            /x : 100,
            /y : 200,
            /w : 10,
            /h : 10
        },

        /notehead : {
            /name : "notehead",
            /type : "circle",

            # SVG drawing information:
            /svg : {
                /type : "circle",
                /cx
                /cy
                /r
                /style : {
                    /color : "red",
                    /stroke_width: 1
                }
            },

            # default JUCE positioning information
            /x : 100,
            /y : 200,
            /w : 10,
            /h : 10
        },

        /simpleClef : {
            /name : "simpleClef",
            /type : "clef",

            # SVG drawing information:
            /svg : {
                /type : "path",
                /d : "M0,0 [...]",
                /style : {
                    /color : "black",
                    /stroke_width: 1
                }
            },

            /clefScript/output : "lambda([symbol],
                /output = {},
                /output./pitch = scale( symbol./y, 0, /h, 0, 127 )
                /output./amp = scale( symbol./svg./style./stroke_width, 0, 10, 0., 1. ),
                /output./dur = symbol./w * 0.01,
                /output./env = cos( aseq( 0, /output./dur ) * pi() * 0.5 )
            )",

            /clefScript/input : "lambda([data],
                /sym = /palette./notehead,
                /sym./y = scale( data./pitch, ...  inverse of output
                ...
            )",


            # default JUCE positioning information
            /x : 100,
            /y : 200,
            /w : 10,
            /h : 10
        }
    },

    /symbol : {
        /1 : {

            /layer : 1,
            /name : "path",  # type name
            /id : "path/1", # unique id able to be referred to by name
            /type : "line", # possibly, as in this case, the type could be different from the svg/type

            # SVG drawing information
            # a question here is whether the svg drawing coordinates should be relative or absolute
            # in the case of JUCE they need to be relative to the container
            # so, in JUCE each component is its own SVG box
            # if we use SVG as the main drawing info format, we need a custom function to get the bounding box
            # and translate the SVG to the origin point
            # when drawing in JUCE we could quickly convert the OSC to SVG
            /svg : {
                /type : "path",
                /d : "M0,0 L100,100 Z",
                /style : {
                    /color : "red",
                    /stroke_width: 1
                }
            }
            # when sending to JUCE becomes: <path id="path/1" d="M0,0 L100,100 Z" style="color='red';stroke-width:1">
            # what's the benefit of this?
            #   - isolates drawing information from

            # JUCE positioning information (determined by mouse placement, and graphic bounds):
            /x : 100,
            /y : 200,
            /w : 10,
            /h : 10,

            # scripts
            # can allow the creation of temporary subsymbols, actual new symbols,
            # and handle dynamic positioning based on clef grammar
            # called from layer so it can reference anything within this symbol layer
            # I guess the JUCE positioning information might need to change if the sublayer
            # is larger than the current parent.

            /onmouse : {
                /down : "lambda([x,y,modifiers],
                    /cx = /x + /w * 0.5,
                    /cy = /y + /h * 0.5,
                    /subsymbol/1 = /~./newRectangle( "overlay", /cx, /cy, /w, /h )
                )"
            }
        },

        /2 : {
            /layer : 2,
            /name : "box",
            /id : "box/1",
            /type : "rectangle",

            # SVG drawing information, in this case it's the same as the type
            /svg : {
                /type : "rectangle",
                /cx : 105,
                /cy : 205,
                /r : 5,
                /style : {
                    /color : "red",
                    /stroke_width: 1
                }
            },

            # JUCE positioning information (determined by mouse placement, and graphic bounds):
            /x : 100,
            /y : 200,
            /w : 10,
            /h : 10,

        },
        /3 : {
            /layer : 3,
            /name : "zwei",
            /id : "zwei/1",
            /type : "group",

            # SVG drawing information, if it's a group,
            /svg : {
                /type : "group",
                /group : {
                    /1 : {
                        /type : "circle",
                        /cx : 205,
                        /cy : 305,
                        /r : 10,
                        /style : {
                            /stroke_width : 1
                        }
                    },
                    /2 : {
                        /type : "line",
                        /x1 : 205,
                        /y1 : 305,
                        /x2 : 205,
                        /y2 : 305,
                        /style : {
                            /stroke_width : 1
                        }
                    }
                }
            },

            # JUCE positioning information (determined by mouse placement, and graphic bounds):
            /x : 200,
            /y : 300,
            /w : 20,
            /h : 20,

        }
    },

    # temporary overlay graphic layer
    /overlay : {
        /numlayers : 0
    },

    # global scripts available in all layers/sub-bundles
    /~ : {
        /newLayer : "lambda([parentlayer],
            /_parent = value(parentlayer),
            if( bound(/_parent),
                progn(
                    /_layernum = /_parent./numlayers++,
                    assign_to_bundleMember(/_parent, "/" + /_layernum, emptybundle() )
                )

            ),
            prog1(
                value( parentlayer + "/." + /_layernum),
                delete( /_layernum ), delete(/_parent)
            )

        )",

        /newRectangle : "lambda([layer, cx, cy, w, h],
            /_layer = if( layer == null,
                /~./newLayer( "/score" ),
                value( "/score./" + layer )
            ),

            /_layer./x = cx - w * 0.5,
            /_layer./y = cy - h * 0.5,
            /_layer./w = w,
            /_layer./h = h,

            /_layer./type = "rectangle",
            /_layer./rectangle = {
                /cx = cx,
                /cy = cy,
                /width = w,
                /height = h,
                /style = {
                    /color = "black",
                    /stroke_width = 2,
                }

            }
        )"
    }

}
```
