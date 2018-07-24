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
            /1 : {
                /margin : [10, 10],
            },
            /2 : {
                /margin : [10, 10],
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
                },
                /bounds : {
                    /x : 0,
                    /y : 10,
                    /h : 10,
                    /w : 10
                }
            },
            /staff : {
                /path :  "M path data, if there are sub paths, we can parse them on edit",
                /style : {
                    /color : "black"
                },
                /bounds : {
                    /x : 0,
                    /y : 10,
                    /h : 10,
                    /w : 10
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
            )"
        },
        /palette : {
            /staveEvent : {
                /type : "group",
                /graphic : {
                    /path : "M0,0 ...",
                    /style : {
                        /css : "attributes"
                    }
                },
                /subsymbol : {
                    /notehead : {
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
                    /stem : {
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
                }
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
                                /style./stroke_width = scale( /params./amp, 0, 1, 0, 10),
                                /subsymbols./notehead./bounds./x = /x,
                                /subsymbols./stem./bounds./y = /y
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
                                /params : {
                                    /pitch : 60,
                                    /amp : 1
                                }
                                /graphic : {

                                },
                                /subsymbol : {
                                    /notehead : {
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
                                    /stem : {
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
                                /params : {
                                    /pitch : 60,
                                    /amp : 1
                                }
                                /graphic : {

                                },
                                /subsymbol : {
                                    /1 : {
                                        /name : "notehead",
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
                                        /name : "stem",
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
