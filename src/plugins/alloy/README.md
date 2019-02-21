For Alloy support, you will need to first run `make` in `src`.

You will also need to download `alloy4.2.jar` from [here](http://alloy.lcs.mit.edu/alloy/download.html) and place it in both `src` and `plugins/alloy`.

NOTE: this plugin requires Java 1.6. On Mac OS Sierra and above, please install https://support.apple.com/kb/DL1572?locale=en_US

To build the plugin, run `make`. 

To run it, run `java -cp ".:alloy4.2.jar" AlloyPlugin <input-file-name>`
