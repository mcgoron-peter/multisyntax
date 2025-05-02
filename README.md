# Multisyntax

Implementation of a fragment of the Macrological Fascicle using the
[Hieb, Dybvig, and Bruggeman][1] algorithm.

The goal of this library is to provide a portable core for

* Future implementations of Scheme with full `syntax-case`
* Hosted languages inside of Scheme whose macros are written in Scheme
  (outside of the hosted language)

[1]: https://legacy.cs.indiana.edu/~dyb/pubs/LaSC-5-4-pp295-326.pdf
