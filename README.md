# symbolist
A library for graphic/symbolic score editing

## DOXYGEN :

The Doxygen progam enables the automatic generation of documentation following 
the instructions contained in the configuration file (the Doxyfile).
To generate the doc, download the Doxygen .dmg at http://www.stack.nl/~dimitri/doxygen/download.html.
When the download is completed, add the binaries to your PATH variable :
```
export PATH=$PATH:/Applications/Doxygen.app/Resources
```

Go to the Doxygen directory under your local symbolist repository, then launch the doxygen command :
```
cd /path_to_your_local_repo/Doxygen
doxygen Doxyfile
```
This will create an html directory and with it generate all documentation files.

### Writing block comments for documentation :

Doxygen supports the javadoc style for documentation's block comments.
All block comments beginning strictly with /** will be considered as
documentation comments.
These blocks can precede all elements of a class definition : class name, fields, constructors, methods... 
Comments for documentation must be written in the **header files**.
Here is an example of block comment for the documentation of a class method 
presented in the Oracle's website, on the page [How to Write Doc Comments for the Javadoc Tool](http://www.oracle.com/technetwork/java/javase/tech/index-137868.html):
```java
/** 
* Draws as much of the specified image as is currently available
* with its northwest corner at the specified coordinate (x, y).
* This method will return immediately in all cases, even if the
* entire image has not yet been scaled, dithered and converted
* for the current output device.
*
* @param img       the image to be drawn
* @param x         the x-coordinate of the northwest corner
*                  of the destination rectangle in pixels
* @param y         the y-coordinate of the northwest corner
*                  of the destination rectangle in pixels
* @param observer  the image observer to be notified as more
*                  of the image is converted.  May be 
*                  <code>null</code>
* @return          <code>true</code> if the image is completely 
*                  loaded and was painted successfully; 
*                  <code>false</code> otherwise.
* @see             Image
* @see             ImageObserver
* @since           1.0
*/
public abstract boolean drawImage(Image img, int x, int y, 
                                      ImageObserver observer);
```                                      
