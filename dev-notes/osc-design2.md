```
/layout : {
    /page : {
        /size : [600, 1600],
        /margins : {
            /left : 10,
            /right : 10,
            /top : 10,
            /bottom : 10
        }
    },
    /system : {
        /margin/topbottom : [20, 20],
        /staves : {
            /piccolo : {
                /margin : [10, 10],
                /name : "piccolo"
            },
            /oboe : {
                /margin : [10, 10],
                /name : "oboe"
            },
        },
        /system/sort : "lambda([a, b],
            (a./y < b./y) && (b./x < (a./x + a./w))
        )"
    }
},
/palette : {
    /foo : {
        /type : "stave",
        /graphic : {
            /clef : {
                /path :  "M path data, if there are sub paths, we can parse them on edit",
                /style : {
                    /color : "black"
                }
            },
            /staff : {
                /path :  "M path data, if there are sub paths, we can parse them on edit",
                /style : {
                    /color : "black"
                }
            },
            /bounds : {
                /x : 0,
                /y : 10,
                /h : 10,
                /w : 10
            },
            /transform : [matrix]
        },
        /script : {
            /self/pixTime : "lambda([t],
                /time/start = t,
                /time/end = t + (/w * 0.01)
            )",
            /event/pixTime : "lambda([stave],
                /time/start = stave./time/start + (/x - stave./x) * 0.01 ,
                /time/end = /time/start + (/w * 0.01)
            )",
            /event/timePix : "lambda([stave],
                /x = stave./x + ( (/time/start - stave./time/start) * 100. ),
                /w = (/time/end - /time/start) * 100.
            )"
        },
        /palette : {
            /staveEvent : {
                /type : path, # or is everything a path?
                /graphic : {
                    /path : "M0,0 ...",
                    /style : {
                        /css : "attributes"
                    }
                },
                /scripts : {
                    /position : "lambda([stave, start, end],
                        /x = stave./x + ( (start - stave./time/start) * 100. ),
                        /w = (end - start) * 100.
                    )",
                    /mapping : {
                        /set : "lambda([stave],
                            #setup parameters
                                /params = {},
                                /params./start ??= 0,
                                /params./pitch ??= 60,
                                /params./duration ??= 1,
                                /params./amp ??= 0.5,
                            # map to graphic parameters
                                /x = stave./x + ( (/params./start - stave./time/start) * stave./time/timePixScale ),
                                /w = /params./duration * stave./time/timePixScale,
                                /y = /stave./y + scale(/params./pitch, 0, 127, 0, stave./h) #here 0 is the bottom,
                                /style./stroke_width = scale( /params./amp, 0, 1, 0, 10)
                        )",
                        /get : "lambda([stave, t],
                            # map graphic to output parameters
                                /pitch = scale(/y, 0, stave./h, 0., 127.),
                                /relative/time = t,
                                /amp = scale( /style./stroke_width, 0, 10, 0, 1)

                                #... there's a valid argument to be had that this should be in Max... but having it embedded means you can quickly add / switch mappings without having to route them in Max which is quite time consuming.

                        )"
                    }
                }
            }
        }
    }
},
# the scripts are all in the palette (aka notation key)
# the score sets up the hierarchy and stores graphic transformations of the symbols
# the graphic is stored as well since in the case of a trajectory path it could be altered
/score : {
    /page : {
        /system : {
            /1 : {
                /stave : {
                    /1 : {
                        /type : "/foo",
                        /name : "foo",
                        /time/start : 0,
                        /time/end : 10,
                        /graphic : {
                            /clef : {

                            },
                            /staff : {

                            }
                        },

                        /symbols : {
                            /1 : {
                                /name : "note",
                                /graphic : {

                                },
                                /bounds : {
                                    /x : 0,
                                    /y : 30,
                                    /h : 10,
                                    /w : 100,
                                    /transform : [matrix]
                                }
                            },
                            /2 : {
                                /name : "note",
                                /graphic : {

                                },
                                /bounds : {
                                    /x : 0,
                                    /y : 30,
                                    /h : 10,
                                    /w : 100,
                                    /transform : [matrix]
                                }
                            },
                            /3 : {
                                /name : "note",
                                /graphic : {

                                },
                                /bounds : {
                                    /x : 0,
                                    /y : 30,
                                    /h : 10,
                                    /w : 100,
                                    /transform : [matrix]
                                }
                            },
                            /4 : {
                                /name : "note",
                                /graphic : {

                                },
                                /bounds : {
                                    /x : 0,
                                    /y : 30,
                                    /h : 10,
                                    /w : 100,
                                    /transform : [matrix]
                                }
                            }
                        }
                    }
                }
            }
        },
        /symbols : {
            /1 : {
                /type : other, non timed symbols
            }
        }
    }
}

```
