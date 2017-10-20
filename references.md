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

* Thorsten Altenkirch, et. al., [ΠΣ: Dependent Types without the Sugar](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma-new.pdf)

* Andres Löh, et. al., [A Tutorial Implementation of a Dependently Typed Lambda
  Calculus](https://www.andres-loeh.de/LambdaPi/).
  Excellent tuturial on implementing a minimal depdendently typed language LambdaPi in Haskell.

* Stephanie Weirich, [Designing Dependently-Typed Programming
  Languages](https://www.cs.uoregon.edu/research/summerschool/summer14/curriculum.html).
  Video lectures and code for a simple language called Pi-Forall,
  implemented in Haskell.

## Propositions as Types

* Philip Wadler, [Propositions as
  Types](http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf).
  See also one of his very entertaining presentations, for example at
  [Strange Loop](https://www.youtube.com/watch?v=IOiZatlZtGU).

* [Dependent Types at
  Work](http://www.cse.chalmers.se/~peterd/papers/DependentTypesAtWork.pdf)
  also gives a very clear exposition.

## Languages

### Haskell

* Richard Eisenberg, [Dependent Types in Haskell: Theory and
  Practice](http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf).
  The primary source for dependent types in Haskell.  See its
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
  Another excellent tutorial, more theoretically oriented, complementing the first one. 

* Jan Malakhovski, [Brutal [Meta]Introduction to Dependent Types in
  Agda](http://oxij.org/note/BrutalDepTypes/).  Another valuable
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
  Foundations](https://softwarefoundations.cis.upenn.edu/draft/).
  An interactive online textbook for learning Coq and its
  applications. The best introduction to Coq and dependent types
  available.

* Assia Mahboubi, Enrico Tassi, et. al., [Mathematical
  Components](https://math-comp.github.io/mcb/) Library used for the
  mechanization of major mathematical theorems.  The accompanying
  textbook is another excellent introduction to Coq.  Whereas SF
  concentrates on tacics using LTAC, this book concentrates on Gallina
  and tactics using its own Ssreflect library.

* Adam Chlipala, [Certified Programming with Dependent
  Types](http://adam.chlipala.net/cpdt/). Advanced use of Coq for
  serious program verification.

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

* [Big Proof](https://www.newton.ac.uk/event/bpr).  Note that [videos](https://www.newton.ac.uk/event/bpr/seminars)
  of the talks are available.

### Computational Semantics

* [Grammatical Framework](http://www.grammaticalframework.org/)

## Homotopy Type Theory

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

* Anders Mörtberg, [Cubical Type Theory: lectures](https://github.com/mortberg/cubicaltt/tree/master/lectures).
  A hands-on introduction to programming in cubical type theory, using the Haskell implementation.

* [Workshop on Homotopy Type Theory and Univalent Foundations of Mathematics](http://www.fields.utoronto.ca/activities/15-16/homotopy-type).
  Mini-courses and research talks by experts.  Videos of all talks are available.

* [Foundations of Mathematics: Univalent Foundations and Set Theory](http://fomus.weebly.com/talks-abstracts--videos.html).
  Another excellent collection of lectures by experts.

## Category Theory and Functional Programming

* Jeremy Gibbons, [Calculating Functional Programs](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/acmmpc-calcfp.pdf).
  Notes for a summer school course including a large number of exercises.  A very clear and well-written introduction to the
  key concepts of the algebra of programming, and good to read before tackling Hinze's paper below.

* Ralf Hinze, [Generic Programming with
  Adjunctions](http://www.cs.ox.ac.uk/ralf.hinze/publications/LNCS-7470.pdf). One
  of a series of related articles by Hinze, and probably the best one
  to start with.  Based on lecture notes for a summer school, and
  contains exercises.

* Martin Hofmann, [Syntax and Semantics of Dependent
  Types](https://www.tcs.ifi.lmu.de/mitarbeiter/martin-hofmann/pdfs/syntaxandsemanticsof-dependenttypes.pdf).
  Combines a very careful development of dependent types with an excellent explanation of how to represent
  dependent types as categories with families. Also discusses and contrasts alternative categorical models
  of dependent types and includes many exercises.

## Schools

* [Oregon Programming Languages Summer
  School](https://www.cs.uoregon.edu/research/summerschool/summer17/).
  I went in 2015 and 2017.  An incredible two weeks of
  type theory and applications; the program changes every year.  All
  lectures are videotaped in high quality and available online;
  replace "summer17" with the previous years to find them.  Many are
  also available on YouTube.

* [DeepSpec Summer School 2017](https://deepspec.org/event/dsss17/index.html).
  Lectures by experts on Coq and its applications.  All lectures were
  filmed and are available on the YouTube channel.

* [Summer School on Generic and Effectful Programming](https://www.cs.ox.ac.uk/projects/utgp/school/).
  One-time school with lectures on various advanced topics, with lecture notes available.
  I personally find Conor McBride's course on Datatypes of Datatypes to be particularly
  valuable, but there are also courses on Idris and Haskell.