# programming-language-concepts
This is my coursework from the module COMP2212 in Year 2 Software Engineering in University of Southampton

This is a simple interpreter for a Domain Specific Language specialised for set theory related problems for example: union, kleene star etc. However, it is extended to solve Integer problems like 'primality test' etc.

It supports - 

1. Integer, Boolean and String manipulation.
2. Conditionals (short and long 'if') and Loop (both 'for' and 'for each' loop)
3. A Set data structure of Strings that supports 'add' and 'remove' of elements
4. Printing of results (primitive and sets).

The coursework directions are available here as well.

To compile: 
'cd' into the Interpreter folder > type 'make'

A 'mysplinterpreter' file will be produced.

To use:

1.Copy in any program from Programs or make your own program.
2.Make an input file containing 

  L1
  L2
  .
  .
  .
  k
  
All in new lines.
  
where L1, L2 are mathematical sets like {a , b, 1, 3} (elements are regarded as strings)
and k is the maximum number of elements to come out as ouputs (if the output is a set)

':' is defined as the symbol for empty string.

for example k=6 for a program to compute kleene star will output {:, a, aa, aaa, aaaa, aaaaa}

Note: multiple k's will only make k = last_occurance_of_integer

Please have a look!
