
# Table of Contents

1.  [Manual Sort](#org99919bf)
    1.  [Purpose](#orgcb87226)
    2.  [Specifications](#org42ada17)
    3.  [Feasability](#orged56331)


<a id="org99919bf"></a>

# Manual Sort

Intended for CPSC 312 Project 1, this program will allow users to sort a list of items manually, prompting the user for pair-wise comparison until the list is fully sorted.


<a id="orgcb87226"></a>

## Purpose

User provides a list of todo items in a text file. Running the program prompts the user to decide (pair-wise) which item is more important to complete. The program presents 3 choices for an unknown pair of items: 

-   Option 1 is more important
-   Option 2 is more important
-   Indifferent

After the user answers enough questions, the program writes the sorted list back to a file.


<a id="org42ada17"></a>

## Specifications

-   **Input:** New-line separated .txt file of todo items
-   **Output:** New-line separated .txt file of sorted items according to user’s answers
-   **Enough Questions:** Assume that comparisons are transitive, thus if A > B and later B > C, then we should implicitly order A > C to minimize the # of comparisons.


<a id="orged56331"></a>

## Feasability

Use a comparison matrix of Int{-1,0,1, Inf}  to represent the currently known ordering

Use file input/output to load strings to print to user

Updates to the matrix can be updated by recursive calls to helper functions (mapping over rows/columns and joining back together)

