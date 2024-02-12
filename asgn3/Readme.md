## Assignment 3

Write a tokeniser in OCaml for the toy calculator (over the extended syntax for expressions, with variables). Your tokens should include the following:

- Identifiers: These are alphanumeric strings that can also contain primes (the single quote symbol) and underscores, but no other symbols. Identifiers must start with a lower case letter or an underscore.
- Keywords: These are words reserved for the language and cannot be used as identifiers. You would have keywords corresponding to the operations defined in class (if-then-else and operations on tuples).
- Boolean operators and constants
- Arithmetic operators and integer constants (without leading zeroes) 
- Comparison operators 
- String operators and constants
- Parentheses and commas

Also compute the numbers that you recognize as part of the tokenisation process (as done with digit sequences in the class notes). Feel free to consider a token set that is larger than this, but it must subsume these tokens. Provide enough test cases to demonstrate the correctness of your tokeniser. 


### Build and run
    $ make
    $ ./main.native 