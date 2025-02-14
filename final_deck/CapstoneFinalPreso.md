Coursera Data Science Capstone Project
========================================================
author: Steffen Hartleib
date: 2015-08-19

YoNeWo: the app predicting your next word
========================================================

Do you get stuck writing and can't think of the next word?

Are you tired of typing and wish words would just appear?

Do you get anxious when you see that blinking cursor on you screen?

Then *YoNeWo* is for you!

You: enter english text  
YoNeWo: predicts the next word.

Here is how I built it


the data & models
========================================================

Sample data corpus: 150,000 lines of news, twitter, and blog copy 

Removed punctuation, numbers, foreign characters, white spaces, and converted to lower case. (custom regex function)

Replaced words appearing only once with "unk" to reserve probability mass for unknown words

Created 2-Grams, 3-Grams, 4-Grams and 5-Grams (weka package)

Built frequency tables for each n-gram and stored them as a
data tables in RDS format. 

the algorithm - Markov Chain 
========================================================
Calculate probability of next word given the previous 1 to 4 words
The steps are:

* Clean user input 
* Replace input words not found in the models with "unk"  
* Run input through the four n-gram models:
  - last 4 words => 5 gram model => 5th word = prediction
  - last 3 words => 4 gram model => 4th word = prediction etc.

* If user input < 4 words, start with the n + 1 Gram model
* Return data frame of predicitons and their probabilities


the algorithm - stupid backoff smoothing
========================================================

- Weight the probablity of each "backed off" prediction by: 0.4  x (4-n) 


```r
pred4 <- check5Gram(t1,t2,t3,t4)
pred4 <- mutate(pred4, Prob = as.numeric(Prob))
pred3 <- check4Gram(t2,t3,t4)
pred3 <- mutate(pred3, Prob = as.numeric(Prob)*.4)
pred2 <- check3Gram(t3,t4)
pred2 <- mutate(pred2, Prob = as.numeric(Prob)*.4*.4)
pred1 <- check2Gram(t4)
pred1 <- mutate(pred1, Prob = as.numeric(Prob)*.4*.4*.4)
```
- Return the prediction with highest total probability  
the app
========================================================

size: 50MB   
precision: 13%  
loadtime: 1.85 sec  
response time: 2.9 sec        

please try the app here:  
https://steffenhartleib.shinyapps.io/YoNeMo2




