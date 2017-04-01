# Type Theory References

## Textbooks

* Benjamin Pierce, [Types and Programming
  Languages](https://www.cis.upenn.edu/~bcpierce/tapl/).  Known as TAPL,
  this book contains the essentials of type theory that everyone
  interested in programming languages should know.  One of the best
  books written on any subject, extremely lucid and well-organized.

* Robert Harper, [Practical Foundations for Programming
  Languages](http://www.cs.cmu.edu/~rwh/pfpl.html).  Another excellent
  textbook complementing TAPL, concentrating more on program language
  design.

* Rob Nederpelt and Herman Geuvers, [Type Theory and Formal Proof: An
  Introduction](https://www.cambridge.org/core/books/type-theory-and-formal-proof/0472640AAD34E045C7F140B46A57A67C).
  Another excellent complement to TAPL, this one concentrating on type
  theory with extensive information on dependent types and their
  applications to formal proof.

## Minimalist Dependently Typed Languages

* Thierry Coquand, et. al., [A simple type-theoretic language:
  Mini-TT](http://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf).
  Includes an implementation in Haskell.

* Thorsten Altenkirch and Nicolas Oury, [ΠΣ: A Core Language for
  Dependently Typed Programming](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma.pdf) 

* Stephanie Weirich, [Designing Dependently-Typed Programming
  Languages](https://www.cs.uoregon.edu/research/summerschool/summer14/curriculum.html).
  Video lectures and code for a simple language called Pi-Forall.

## Propositions as Types

* Philip Wadler, [Propositions as
  Types](http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf).
  See also one of his very entertaining presentations, for example at
  [Strange Loop](https://www.youtube.com/watch?v=IOiZatlZtGU).


## Languages

### Haskell

* Richard Eisenberg, [Dependent Types in Haskell: Theory and
  Practice](http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf).
  The primary source for dependent types in Haskell.  See it's
  references for further background.

* Richard Eisenberg, [Types and
  Kinds](https://typesandkinds.wordpress.com/).
  Eisenberg's blog; the July 2016 entry "Dependent types in Haskell:
  Progress Report" contains the expected timeline for dependent types
  in GHC.

* Stephanie Weirich, et. al., [A Specification for Dependently-Typed
  Haskell](https://www.seas.upenn.edu/~sweirich/papers/systemd-submission.pdf).
  A more recent proposal for a core language for Dependent Haskell.

### Agda

* Ulf Norell and James Chapman, [Dependently Typed Programming in
  Agda](http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf). Excellent tutorial.
* Ana Bove and Peter Dybjer, [Dependent Types at
  Work](http://www.cse.chalmers.se/~peterd/papers/DependentTypesAtWork.pdf).
  Another great tutorial, complementing the first one. 
* Jan Malakhovski, [Brutal [Meta]Introduction to Dependent Types in
  Agda](http://oxij.org/note/BrutalDepTypes/).  Another great
  resource.

### Idris

* Edwin Brady, [Type-Driven Development with
  Idris](https://www.manning.com/books/type-driven-development-with-idris).
  Written by the language creator, this is the closest you'll find to
  a "dependent types for dummies" book.  Very well-written and
  detailed, it guides the reader step-by-step and is an
  excellent first introduction to dependent types and why they are
  useful in everyday programming.

### Coq

* Benjamin Pierce, et. al., [Software
  Foundations](https://www.cis.upenn.edu/~bcpierce/sf/current/index.html).
  An interactive online textbook for learning Coq and its
  applications. The best introduction to Coq and dependent types
  available.  Note that it will be reorganized into two volumes soon;
  see [DeepSpec](https://deepspec.org/page/SF/).

* Andrew Appel, [Verified Functional
  Algorithms](https://www.cs.princeton.edu/~appel/vfa/).  Part of the
  Software Foundations series, introducing exactly what its title says.

* Assia Mahboubi, Enrico Tassi, et. al., [Mathematical
  Components](https://math-comp.github.io/mcb/) Library used for the
  mechanization of major mathematical theorems.  The accompanying
  textbook is another excellent introduction to Coq.  Whereas SF
  concentrates on tacics using LTAC, this book concentrates on Gallina
  and tactics using its own Ssreflect library.

## Applications of Dependent Types

### Software Quality

* [CompCert](http://compcert.inria.fr/).  Verfied C compiler.
* [DeepSpec](https://deepspec.org/main).  Ambitious five-year project
  to formally specify and prove correctness from the hardware level to
  compilers, operating systems, and beyond.

### Formal Mathematical Proof

* Georges Gonthier, [Formal Proof—The FourColor
  Theorem](http://www.ams.org/notices/200811/tx081101382p.pdf).  Also
  a [presentation](https://www.youtube.com/watch?v=yBXGdJw1xBI).

* Georges Gonthier, et. al, [A Machine-Checked Proof of the Odd Order
  Theorem](https://hal.inria.fr/hal-00816699/file/main.pdf)

* [Big Proof](https://www.newton.ac.uk/event/bpr)

### Computational Semantics

* [Grammatical Framework](http://www.grammaticalframework.org/)

### Homotopy Type Theory

* [Homotopy Type Theory](https://homotopytypetheory.org/). The
  standard textbook is available online here.

* Robert Harper, [15-819 Advanced Topics in Programming
  Languages](https://scs.hosted.panopto.com/Panopto/Pages/Sessions/List.aspx?#folderID="07756bb0-b872-4a4a-95b1-b77ad206dab3").
  CMU course on HoTT, beautifully taught and filled with deep insights.

* Cyril Cohen, et. al., [Cubical Type Theory: a constructive
  interpretation of the univalence
  axiom](https://www.math.ias.edu/~amortberg/papers/cubicaltt.pdf). See
  also Simon Huber's
  [Thesis](http://www.cse.chalmers.se/~simonhu/misc/thesis.pdf) and
  the [GitHub repo](https://github.com/mortberg/cubicaltt) which contains an
  implementation in Haskell.

### Category Theory and Functional Programming

* Ralf Hinze, [Generic Programming with
  Adjunctions](http://www.cs.ox.ac.uk/ralf.hinze/publications/LNCS-7470.pdf). One
  of a series of related articles by Hinze, and probably the best one
  to start with.  Based on lecture notes for a summer school, and
  contains exercises.

## Schools

* [Oregon Programming Languages Summer
  School](https://www.cs.uoregon.edu/research/summerschool/summer17/).
  I went in 2015 and will return in 2017.  An incredible two weeks of
  type theory and applications; the program changes every year.  All
  lectures are videotaped in high quality and available online;
  replace "summer17" with the previous years to find them.  Many are
  also available on YouTube.
