# Alloy Plugin for Penrose

## Dependencies

- NOTE: although the `.jar` files are provided in the repo, they might not compile in another machine due to unknown reasons (TODO: might be different JRE versions). So please redownload them whenever errors appear
- Alloy: [`org.alloytools.alloy.dist.jar`](https://github.com/AlloyTools/org.alloytools.alloy/releases/download/v5.1.0/org.alloytools.alloy.dist.jar)
    - For Alloy specific issues: https://github.com/AlloyTools/org.alloytools.alloy
    - NOTE: According to the Alloy doc: "JAR should run on macOS with Java 9"
- org.json: [`json.jar`](http://central.maven.org/maven2/org/json/json/20180813/json-20180813.jar)

## Build and run

- To build the plugin, run `make`.
- To run it, run `java -cp "*.:" AlloyPlugin <input-file-name>`
