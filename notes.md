1. Living in Seattle. Bay Area for 12+ years.

2. Source and references available.  Everything in talk is referenced.

3. Not much one can say in 50 minutes.  Questions, may defer answering.  Available both days.

4. Agda most aesthetic; written in Haskell.  Vectors: basic but important, show many issues, subtleties.  Null pointer and array bounds.
   Set is Agda's term for Type.  Implicit arguments (like Haskell visibility), mixfix.  : and :: reversed.  Parameters vs indices.

5. Append just like Haskell, but length is in type.

6. One way to do lookup.

7. If don't like finite sets, an alternative.  First some preliminaries.  Only proof of equality in MLTT is refl.

8. lookup': cases for vector; suc m < suc n beta reduces to m < n

9. 

10. Colors.  Type in Type.  Thm proving vs programming. 

11. Haskell just the beginning.  25 years into the future.  Speeding it up.  Where are we heading?

12. Disbelief to disinterest.

13. 

14. 

15. Language options: first two just for aesthetics, Type in Type just one place, but want to show where it is insufficient.
    Implies DataKinds, PolyKinds.  Use Type instead of *.
    Nat is promoted to a Kind, Zero and Succ to types.

16. Use ' to disambiguate types and promoted terms (not needed here).
    Added explicit forall.  Parameter vs index.

17. Type families not first-class.

18. 

19. 

20. So dependent types already work in Haskell?

21. Can't quantify over k in data.

22. -> is not dependent function type.  Fin works because Nat is embedded into a type.

23. There are other attempts as well in the source code.
    Try to lift everything up to the type level.
    Types can depend on types.  Need to make A explicit.
    Now what goes wrong?
    Can use kind! to evaluate, but only small-step.  Can't assign to a variable.

24. Have to inject terms into types, but 1-1.

25. Works, but have to add singleton.  Singletons library helps automate, promote functions to type families.

26. No more type families, no more promotion.

27. Again no promotion.

28. No singletons.  A goal of DH is to obsolete singletons.

29. Also Idris.