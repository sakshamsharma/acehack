------
title: Pairing up n people into 2 teams
musing: true
summary: Try this next time you play pool
tags: algorithm
category: algorithm
id: team-selection
author: Saksham Sharma
------

While at [MPI](www.mpi-sws.org) during the Summer, a friend ([Venkat](https://venkatarun.wordpress.com)) inspired me by presenting some real world algorithm hacks. A few hours later, I found myself playing pool with 3 others, and we tried doing a toss of hands to pair ourselves up into 2 teams. 7 tosses later, we still didn't have a pairing of 2 each.

This inspired me to think of a way to divide `n` people into 2 teams using a single random choice by every player.

## Scenario and problem definition
Here are the desired characteristics of such an algorithm:

* Should require only 1 random computation from each party.
* Should return 2 disjoint sets (whose union is the complete group of people) after a trivial (`O(n)` desired) computation.
* If 2 people want to be in the same team, even if they talk beforehand, they must not be able to increase the probability of them being in the same team from more than half.
* If 2 people *don't* want to be in the same team, even if they talk beforehand, they must not be able to decrease the probability of them being in the same team from more than half.
* There should be no need for any external material (like paper slips).

## Candidate algorithms and analysis
Some of the candidate techniques are listed here

### Paper slip based team ups

#### Algorithm
Make `n` paper slips, where `n/2` of them have `1` written, and the remaining have `0` written on them. Place them inverted after shuffling, and ask everyone to pick one up each.

#### Pros
* Simple and easy to use
* Requires no computation after the initial random choice from each person's side

#### Cons
* Requires use of paper slips, which may not always be at hand

### Random number selection
I would explain this algorithm bit by bit, posing challenges to the algorithm, and proposing workarounds. The issues are listed in the order I had encountered them while analysing the algorithm that fine day.

#### Algorithm
Ask everyone to choose a number between `1` to, say `10*n` (10 is just a heuristic choice here, tradeoff between calculation complexity and avoiding collisions). Arrange people in ascending order of their chosen number such that every alternate person is in the same team.

> Collisions can still occur

Ask every one to stand in a circle before this exercise, and decide on clockwise or anticlockwise direction. Since there are even number of people, use a pre-decided-location dummy person for making the count odd. When two people have the same number, let them be `A` and `B`. From `A` to `B`, let there be *k* people, and from `B` to `A`, there would be `n-k-2-1` people. If *k <= n-k-3*, `A` gets precedence. This is an easy computation, and can be done in a distributed manner, thus taking up `O(n)` time for any number of collisions possible.

> People can choose numbers at the extremes to defeat the algorithm

Take the mean of the numbers chosen (`O(n)`) and use it to define a mean position to wrap around. Now start counting from the mean position as the *0* index. This defeats any attempt by anywhere less than *n/2* people to influence the teams meaningfully, since this adds an influence of the whole group.

> But wait, cyclic order is something like sorting, which is O(n log(n))

Point taken. But for *n* humans, perhaps sorting is not a challenge. As for computers, since it is only integers here, which are bounded by a small value, computers can actually use [radix sort](https://en.wikipedia.org/wiki/Radix_sort) to sort the numbers in `O(n)` time.

> People can choose same or numbers differing by 1

This is harder to defeat. A possible way is the following:

* Using the cyclic numbering assigned above to each person, everyone multiplies his own number by his position received (this should perhaps be improved to be something like 10 times the position received)
* Each person returns the above value modulo `10*n`.

After the above steps, any pair of people won't know what positions they might get, and how many people will find a numeric value between them after the multiplication and modulo calculation.

#### Pros
* No paper needed.
* Only `O(n)` computation needed (if you consider that humans are capable of doing sorting in `O(n)` time).
* Nice theoretical problem to solve :smile:
* Mostly individual calculations (except mean calculation and sorting), thus can be done in parallel.

#### Cons
* Insanely complicated to explain to a group of people :smile:

You can actually validate the computations by enforcing that every person also checks the computation of the person having the value just greater than his value (thus ensuring fairness). This way, everyone can do only 3 computations, and the computations can be finished in a distributed manner.

I don't suppose a computer would want to use this algorithm, since it can always do a random sorting of the list, and select the first `n/2`. Of course there are easier ways. I believe the sole purpose of this algorithm is to illustrate how one can patch up a bad algorithm to make it work.


### Central random number choosing party
One person is selected randomly, and he turns his back. The remaining people shuffle around into a random order, and he chooses random `n/2-1` people to be in his team.

#### Pros
* Simple and easy
* No computation needed

#### Cons
* If the person chosen is a biased person, he may have it precided with his desired team mates to choose positions towards a certain end.

The above *con* can be defeated to certain extents if the remaining people choose a random 0 index (using a mean of randomly chosen numbers) without telling the random number chooser.

## Conclusion
As stated above, the second algorithm seems to be pretty much useless in practical terms, except maybe for a group of people so mathematically inclined. The third algorithm should be enough for normal situations.
