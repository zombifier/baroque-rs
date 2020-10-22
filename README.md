# baroque-rs
An implementation of [Ultima](http://www.inference.org.uk/mackay/ultima/ultima.html), complete with rudimentary AI.

To run, supply 2 arguments indicating the players for Black and White. 0 for human/manual, and 1+ for AI. The higher the number the deeper in the decision tree the AI searches, and the longer it takes.

`./baroque 0 2 # To play against a moderate AI`

To make a move, type 2 square coordinates separated by a space; `a1 c4` for example.
