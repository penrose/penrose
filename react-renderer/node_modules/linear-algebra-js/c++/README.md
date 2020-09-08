
To compile this wrapper of eigen to asm.js, download and install the emscripten SDK from [here](http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html). Then run the following instruction:
```
./emcc -O3 --memory-init-file 0 -I/PATH_TO_EIGEN -I/c++/eigen-wrapper/ -s ALLOW_MEMORY_GROWTH=1 -s DEMANGLE_SUPPORT=1 --bind -o OUTPUT_FILENAME c++/embind.cpp
```
