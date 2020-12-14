## Import Clean Data

twitterDF <- read_csv("withoutSW.csv")

## Get 80% of the clean subset to train and 20% to test

set.seed(1551)
linesToTrain <- sample(length(twitterDF$line), length(twitterDF$line)*.8)
trainingDF <- twitterDF[linesToTrain,]
testingDF <- twitterDF[-linesToTrain,]

## Function to make ngrams

ngram_freq <- function(x, n) {
        ngram <- NGramTokenizer(x, Weka_control(min = n, max = n))
        return(ngram)
}


## Ngram in the making

unigram <- ngram_freq(trainingDF$text, 1)
bigram <- ngram_freq(trainingDF$text, 2)
trigram <- ngram_freq(trainingDF$text, 3)
tetragram <- ngram_freq(trainingDF$text, 4)
pentagram <- ngram_freq(trainingDF$text, 5)

## Making the markov fit and saving

markov_unigram <- markovchainFit(unigram, method = "laplace")
save(markov_unigram, file = "markov_unigram.RData")
markov_bigram <- markovchainFit(bigram, method = "laplace")
save(markov_bigram, file = "markov_bigram.RData")
markov_trigram <- markovchainFit(trigram, method = "laplace")
save(markov_trigram, file = "markov_trigram.RData")
markov_tetragram <- markovchainFit(tetragram, method = "laplace")
save(markov_tetragram, file = "markov_tetragram.RData")
markov_pentagram <- markovchainFit(pentagram, method = "laplace")
save(markov_pentagram, file = "markov_pentagram.RData")

## Predict text based on the function that uses the previous markovs

source(paste(getwd(),"/Rcode/ngramModel_fun.R", sep=""))
predictive_text("It would mean the", 1)

