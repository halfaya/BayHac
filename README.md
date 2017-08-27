Source and slides for BayHac 2017 presentation. [Video here](https://www.youtube.com/watch?v=buVyfrU6QF4&t=2122s).
Note that there are some minor errors in the presentation; I will add errata here some day.

To make from source, Agda, XeLaTeX and the XITS font must be installed.
* In the agda directory, run `agda --latex BayHac.lagda`.
* Then in the slides directory, run `xelatex bayhac.tex`.
  This may need to be run more than once, for example if the number of pages changes.
