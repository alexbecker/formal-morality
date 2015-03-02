Formal-Morality
---------------

A heavily mathematical Pseudo-Rawlsian moral framework. For an example, compile Example.hs using GHC.

## Philosophical Background

The full philosophical arguments are beyond the scope of this repo, but a brief summary is necessary.
John Rawls's *A Theory of Justice*, perhaps the most influential piece of modern philosophy,
defines both a framework for creating moral theories and 
a moral theory to which he argues the framework gives rise.
This framework revolves around the central principle of the *Veil of Ignorance*, essentially asking
how a moral theory would be devised by individuals who had no knowledge of their station in life.

I find this notion very attractive, but it has two key difficulties:

1. People value different things differently, so some sort of correspondence is required.

2. People have different risk adversities.

I attempt to formalize a method for overcoming these obstacles.
Other such formalisms exist, most notably Utilitarianism. However, the assumptions of Utilitarianism
seem too strong to me. My formalism resembles Utilitarianism in some ways, but with the utility
function replaced by the much weaker notion of a preorder.
As a consequence, theories it creates generally impose relatively few duties.

## Moral Theory

The theory requires that a each member of a community have:

1. A preorder of values for probability distributions of states of the world. 
Simplified, that means that given the two statements of the form 
"The world will look like P with probability X, and like Q with probability Y",
you may be able to say that one is better than the other, or that they are equal good.
However, it is also possible that they are incomparable.
For example, I might love my children and love my spouse, but could not choose which I valued more not
because I love them equally but because I love them differently.
This model fits with the observation that when asked to produce a utility function,
people are highly inconsistent--not because their utility changes rapidly, but because their utility
function is ill-defined.

2. The ability to recognize a state of the world from their own perspective as essentially
equivalent to another state of the world from another's perspective. For example, I might recognize that
your being in love from your perspective is qualitatively the same as my being in love from mine,
but perhaps your experience of writing Haskell is not the same as mine.

A moral theory is constructed by linking together the states of the world from each person's perspective,
using their recognition of other's perspectives, forming the *Consensus Family*. 
If everyone values their equivalent for A over their
equivalent for B, I believe that this automatically imposes a moral weight of A over B on 
every member of the community. This seems natural because it obeys the values and mutual recognition
of every member.

Given a choice between probability distributions A and B, choosing A is a moral duty for a person
if the following conditions hold:

1. (Consensus Behind the Veil) When randomized uniformly across the equivalent states for all members 
of the community, the randomized version of A is ≥ the randomized version of B in the subset
of that person's preorder contained in the Consensus Family, and the reverse (with B and A switched) is not true.

2. (No Duty to Self) The same holds when the person choosing is omitted.

3. (Self-Preservation) The choice of A over B does not pose a significant risk of compromising the person's
membership in the moral community, e.g. by killing them or by so emotionally damaging them that they
are no longer able to recognize the same values.

## Formal Mathematics

Requirement 1 is a preorder of probability distributions, and the second requirement lifts
naturally to isomorphisms between them.
The process of "connecting" the preorders is just taking the unique maximal groupoid of preorders
constructed from sub-preorders of those given.
