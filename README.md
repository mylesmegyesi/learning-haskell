# Learning Haskell

Code I wrote while learning [Haskell](http://www.haskell.org/)

## Setup

1. Install the [The Haskell Platform](https://www.haskell.org/platform/).
2. Clone this repository and change to the directory.
3. Configure cabal.

    `$ cabal sandbox init`

4. Download dependencies:

    `$ cabal install --only-dependencies`

5. Run the tests to verify everything works:

    `$ cabal run spec`

## Retracing my steps:

1. I read [*Learn You A Haskell For Great Good!*](http://learnyouahaskell.com/)

    And did Exercises:

    * [Reverse Polish Notation Calculator](http://learnyouahaskell.com/functionally-solving-problems#reverse-polish-notation-calculator) - `LearnYouAHaskell.SolveRPN`
    * [Heathrow To London](http://learnyouahaskell.com/functionally-solving-problems#heathrow-to-london) - `LearnYouAHaskell.HeathrowToLondon`

2. Lisp Interpreter - `Lisp`

3. [UPenn CIS 194: Introduction to Haskell (Fall 2014)](http://www.cis.upenn.edu/~cis194/)

    * [Homework 1](http://www.cis.upenn.edu/~cis194/hw/01-intro.pdf) - `CIS194.HW01`
    * [Homework 2](http://www.cis.upenn.edu/~cis194/hw/02-dict.pdf) - `CIS194.HW02`
