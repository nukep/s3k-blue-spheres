# Sonic 3 (& Knuckles) Blue Spheres demo

This is a work-in-progress demo of the Special Stage in Sonic 3 & Knuckles, written in ClojureScript!

## Setup

To get an interactive development environment run:

```
yarn watch
```

and open your browser at [localhost:3000](http://localhost:3000/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL.

To create a production build run:

```
yarn build
```

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.
