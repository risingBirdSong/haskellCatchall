This is a fantastic and educational article on a really powerful tool. 



https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing



I always tended to think that testing was tedious. Good to do but boring and a chore. 



QuickCheck (the original Property Testing library) turned me around, there are ways testing can be very interesting, fun and much more powerful than the naive way I previously thought about testing. 



QuickCheck offers a way for the machine to automate the boring stuff, and let us focusing on writing interesting functions and thinking about working code at a higher level.

 

Read the article for a deeper dive and a very useful example, but let me summarize my takeaway and showed how I applied the strategy to a simple Leetcode. 



First, the motivation, we want to be using Property Checking when possible, because based on the type signatures of our code, QuickCheck can generate massive amounts of random data and feed it through our functions. Probing our code, looking for problems and non obvious edge cases we haven't thought of. 



If our functions can survive the onslaught of randomness, we can have much higher confidence that they work soundly. But also, we don'