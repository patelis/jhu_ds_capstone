Questions to consider

How can you efficiently store an n-gram model (think Markov Chains)?
  
  How can you use the knowledge about word frequencies to make your model smaller and more efficient?
  
  How many parameters do you need (i.e. how big is n in your n-gram model)?
  
  Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?

How do you evaluate whether your model is any good?

How can you use backoff models to estimate the probability of unobserved n-grams?

Size: the amount of memory (physical RAM) required to run the model in R

Runtime: The amount of time the algorithm takes to make a prediction given the acceptable input