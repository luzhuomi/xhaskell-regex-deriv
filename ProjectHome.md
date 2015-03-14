An implementation of posix matching using derivative.

The implementation consists of two variants.

  1. A general parsing scheme which produces parse trees, e.g. all sub-matches under the kleene's star patterns are maintained.
  1. A specialised sub-matching which only keep tracks of the the last match of the kleene's star pattern.

## News ##
BitCoding implementation is added. It does not support the full set of regex syntax.
  * It only supports binary union, e.g. (a|(b|c)) is fine , but (a|b|c) will be rejected.

## Benchmark Results ##

https://docs.google.com/spreadsheet/ccc?key=0AsJzxtc70DgEdE9YM1hxdXRiQmRFZHRFOVIwOUwzQ0E#gid=0

## Reference Implementation presented in papers ##

  1. GeneralParsing
  1. SubMatchingOnly


## NFA DFA Hybrid Approach Prototype ##
See here NfaDfaHybridApproach

## Papers ##
  1. POSIX Regular Expression Parsing with Derivatives [paper](https://sites.google.com/site/luzhuomi/file/regex-parsing-derivatives.pdf?attredirects=0?attredirects=0)
  1. Correct and Efficient POSIX  Submatch Extraction with Regular Expression Derivatives  [paper](https://sites.google.com/site/luzhuomi/file/icfp13.pdf?attredirects=0)