# Haskell Slime Simulation
To be written.

Before you start, install LLVM on your device (version 9 on Ubuntu; do not forget that there might be a problem with llvm-config; use ln to "rename" it)
On this stage of project you might want to check if it is built correctly.
Also you might need to install `pkg-config` and `libffi-dev`. 

Try
1. `stack update`
2. `stack build`
3. `stack runghc -- -O2 -threaded Mandelbrot.hs`

An image called `mandelbrot.bmp` should be generated. 