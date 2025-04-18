Kotlin bootstrapping
--------------------

This project is an attempt to build the Kotlin compiler and its standard
libraries with no prebuilt version of Kotlin. See the
[Bootstrappable Builds](https://bootstrappable.org) project to understand
why it matters.

Everything available in GUIX is assumed to be bootstrappable

## Status

The build works up to Kotlin 1.1.2-5 (2017-06-12).


## How to build

    guix build -f kotlin.scm

## Credits

This project includes contributions from Emmanuel Bourg, Julien Lepiller and TarCV.
