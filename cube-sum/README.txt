Ideas to go faster:
- more mod checks (hs uses 3 3-digit numbers..)
- cube root caching (but has to be faster than normal hash overhead?
  use a really lame hash function?)
  could use up to quite a lot of memory though, like 1 GB? 100M uint64's.

Notes:
- Parallelism of 4 seems best: (I guess because it's so CPU bound.)
  - 4@[0101363 .. 0101462]: 2176.64
  - 5@[0101463 .. 0101562]: 2242.58 (3% longer)
  - 6@[0101563 .. 0101662]: 2271.06
  - 4@[0101663 .. 0101762]: 2217.81

Bounds:
- 2*a^3 fits in uint64_t:
  a <= 2,097,151
- Can use (float) instead of (double) for cbrt() testing:
  a <= 1,392,200

== How factoring speeds up finding cubes ==

Given a and b (with a > b), let
S := a + b
D := a - b
So a = (S + D) / 2 and b = (S - D) / 2.

Note that:
(a^3 + b^3) = S (a^2 - ab + b^2)
D^2 = a^2 - 2ab + b^2
S^2 = a^2 + 2ab + b^2

So:
4 (a^3 + b^3) = S (3 D^2 + S^2)

So we factor a3b3 and for each possible S dividing it,
we see if an integer D results.

We also know that S < 4 (a^3 + b^3) ^ (1/3) which we could use
to limit the divisors we need to check..
