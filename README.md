# nominal-unification

Prototypical Haskell implementation based on [Efficiency of a good but not linear nominal unification algorithm](https://easychair.org/publications/preprint/DLVk).

This isn't really loglinear time, because I cut corners when implementing the substitution map and was not particularly smart about which multiequation to solve first.