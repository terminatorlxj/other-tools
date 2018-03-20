# Description

This is an input file for the Kripke model of the Small Aircraft Transpotation System (SATS), which is designed in [NASA](https://www.nasa.gov/).

Please see the technical report https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/20040047188.pdf for futher details.

# How to Verify This Model

First, please go to the version 2.0 of SCTLProV, which is in https://github.com/terminatorlxj/SCTLProV2.0.

* SCTLProV2.0 is also an implementation of the SCTL proof system, only with a more expressive input language, and the input language of SCTLProV2.0 is compatible with SCTLProV as much as possible.

Then, compile the code using either `make linux` or `make win`.

At last, run the verification process by `sctl2 sats.model`.