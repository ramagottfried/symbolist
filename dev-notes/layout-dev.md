
# Layout Option 1
With layout option 1, the Symbols are sorted into a hierarchy of Page/Symbol/Stave/Symbol. This makes their grouped situated-ness more obvious.

All symbols are on the page. If a Symbol is within a System, it is timed. Otherwise, it's just a graphic object, which optionally could have other types of script/mapping in the future.

```
/score : {
    /page : {
        /system : {
            /1 : {
                /stave : {
                    /1 : {
                        /type : "piccolo",
                        /symbols : {
                            /1 : {
                                /type : rectangle,
                                ...
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
