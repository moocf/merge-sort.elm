Merge sort is an efficient, general-purpose, comparison-based
sorting algorithm.

**Course**: [Software Foundations], Spring 2020<br>
**Taught by**: Prof. Venkatesh Chopella

[Software Foundations]: https://github.com/iiithf/software-foundations

Merge sort is modeled as a transition system in various stages.

1. [Manual Swap](https://htmlf.github.io/merge-sort/MergeSort1.html)
2. [Manual Order](https://htmlf.github.io/merge-sort/MergeSort2.html)
3. [Manual Order, Manual Merge](https://htmlf.github.io/merge-sort/MergeSort3.html)
4. [Automatic Order, Manual Merge](https://htmlf.github.io/merge-sort/MergeSort4.html)
5. [Automatic Order, Automatic Merge](https://htmlf.github.io/merge-sort/MergeSort5.html)

*Stage 5* is the actual *merge sort* algorithm, which is obviously automatic
(the computer does it for you). The idea is to see the merge sort algorithm
as a system derived from the very simple **Manual Swap** system. Proofs of each
such system are taught in the course.

![](https://ga-beacon.deno.dev/G-G1E8HNDZYY:v51jklKGTLmC3LAZ4rJbIQ/github.com/moocf/merge-sort.elm)
