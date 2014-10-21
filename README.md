# Destyle

Destyle is a work-in-progress attempt at creating a CSS parser in CL. It is a very (very) naïve implementation of [this][w3ccr] W3C Candidate Recommendation.

Currently (mostly) implemented is the tokenization step.

[w3ccr]: http://www.w3.org/TR/css-syntax-3/

## Goals

A long-term goal is to implement [Less][less] implementation in Common Lisp.

[less]: http://lesscss.org/

Another idea I've had is to build a CSS analyzer that suggests ways to improve your CSS/Less, because CSS tends to turn into a horrible mess. Pre-processors like Less help to fix this a little bit, but some more guidance might be nice at times.

## TODO

Pretty much everything.

## License

    Copyright (c) 2014 Joram Schrijver

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
