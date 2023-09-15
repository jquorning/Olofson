
## Olofson

SDL2 demos using the SDLAda binding.

## Requirements

* Ada 2012 compiler
* GPRbuild
* [SDLAda](https://github.com/Lucretia/sdlada)

## Build and run with Alire

```sh
alr get --build olofson
cd olofson_*
alr run parallax3
alr run parallax4
alr run ballfield
alr run fixratepig
```

## Build

```sh
gprbuild parallax3.gpr -XSDL_PLATFORM=macosx
gprbuild parallax4.gpr -XSDL_PLATFORM=macosx
gprbuild ballfield.gpr -XSDL_PLATFORM=macosx
gprbuild fixedratepig.gpr -XSDL_PLATFORM=macosx
```

## Screenshots

![Parallax on Linux](screenshots/linux.png)

## Origin

The originals are found [here](http://olofson.net/examples.html)
and 1.0 version of pig [here](http://olofson.net/mixed.html) or in 
1.1 version [here]((http://www.olofson.net/pig/).
[YouTube video](https://www.youtube.com/watch?app=desktop&v=VvgSGAuxCvg)

