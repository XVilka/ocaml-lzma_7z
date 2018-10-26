# Intro

This is OCaml Ctypes wrapper for [7Zip SDK](https://www.7-zip.org/sdk.html) library, providing decompression routines.

This version includes C sources from 18.05 SDK version (2018-04-30) which can be downloaded [here](https://www.7-zip.org/a/lzma1805.7z).

If you want to update the sources and there were no API changes - just copy corresponding files into
`7zip` directory, but keep the provided Makefile.

This wrapper was created as an alternative to [ocaml-lzma](https://github.com/XVilka/ocaml-lzma) due
to the thread unsafety of xz-utils.
