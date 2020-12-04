Merge sort is an efficient, general-purpose, comparison-based
sorting algorithm.

**Course**: [Software Foundations], Spring 2020<br>
**Taught by**: Prof. Venkatesh Chopella

[Software Foundations]: https://github.com/iiithf/software-foundations

Merge sort is modeled as a transition system in various stages.

1. Manual Swap
2. Manual Order
3. Manual Order, Manual Merge
4. Automatic Order, Manual Merge
5. Automatic Order, Automatic Merge

*Stage 5* is the actual *merge sort* algorithm, which is obviously automatic
(the computer does it for you). The idea is to see the merge sort algorithm
as a system derived from the very simple **Swap** system. Proofs of each
such system are taught in the course.
