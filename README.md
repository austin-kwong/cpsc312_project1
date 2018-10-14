
# Table of Contents

1.  [Manual Sort](#orgfe5a16d)
    1.  [Purpose](#orgf100da4)
    2.  [Specifications](#org349c0a5)
    3.  [Feasability](#org4f6026e)


<a id="orgfe5a16d"></a>

# Manual Sort

Intended for CPSC 312 Project 1, this program will allow users tto sort a list of items manually, prompting the user for pair-wise comparison until the list is fully sorted.


<a id="orgf100da4"></a>

## Purpose

User provides a list of todo items in a text file. Running the program prompts the user to decide (pair-wise) which item is more important to complete. The program presents 3 choices for an unknown pair of items: 

-   Option 1 is more important
-   Option 2 is more important
-   Indifferent

After the user answers enough questions, the program writes the sorted list back to a file.


<a id="org349c0a5"></a>

## Specifications

-   **Input:** New-line separated .txt file of todo items
-   **Output:** New-line separated .txt file of sorted items according to user’s answers
-   **Enough Questions:** Assume that comparisons are transitive, thus if A > B and later B > C, then we should implicitly order A > C to minimize the # of comparisons.


<a id="org4f6026e"></a>

## Feasability

Use a comparison matrix of Int{-1,0,1, Inf}  to represent the currently known ordering

Use file input/output to load strings to print to user

Updates to the matrix can be updated by recursive calls to helper functions (mapping over rows/columns and joining back together)

