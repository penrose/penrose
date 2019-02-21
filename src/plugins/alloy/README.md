# Alloy Plugin for Penrose

## Dependencies

- NOTE: although the `.jar` files are provided in the repo, they might not compile in another machine due to unknown reasons (TODO: might be different JRE versions). So please redownload them whenever errors appear
- Alloy: [`alloy4.2.jar`](http://alloy.lcs.mit.edu/alloy/downloads/alloy4.2.jar)
    - NOTE: this plugin requires Java 1.6. On Mac OS Sierra and above, please install https://support.apple.com/kb/DL1572?locale=en_US
- org.json: [`json.jar`](http://central.maven.org/maven2/org/json/json/20180813/json-20180813.jar)

## Build and run

- To build the plugin, run `make`.
- To run it, run `java -cp "*.:" AlloyPlugin <input-file-name>`
