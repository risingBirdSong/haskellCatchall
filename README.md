# catchallHaskell


got stuck with installing a graphing library that I wanted to try ->
the community recommended finding the LTS resolver in stack.yaml
in my case -> resolver: lts-16.18
and then looking for the package on the relevant stackage build
so ->
https://www.stackage.org/lts-16.18
and it turned out it was case issue
I had been doing 
- chart-diagrams
and the correct package downloads correctly ->
- Chart-diagrams

cairo          > Configuring cairo-0.13.8.1...
cairo          > setup.exe: The program 'pkg-config' version >=0.9.0 is required but it could
cairo          > not be found.
cairo          >

--  While building package cairo-0.13.8.1 using:

-- profiling

this command was the one that worked for me for profiling my stack program

``stack exec --profile -- myprojectname-exe  +RTS -p``

and how to insert your own Cost Centres for clearer reading 

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#inserting-cost-centres-by-hand
