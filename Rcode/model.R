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
#load("markov_unigram.RData")
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

# Preparing the DF to test

for (i in 1:length(testingDF$text)) {
        
        all_words <- unlist(strsplit(testingDF$text[i], " "))
        testingDF$myWord[i] <- tail(all_words, 1)
        testingDF$testing[i] <- paste(all_words[-length(all_words)], collapse = " ")
        
}


# predict words in my testingDF

for (i in 1:length(testingDF$text)) {
        
        inTest <- tryCatch(predictive_text_alternative(testingDF$testing[i], 1), error = function(e) 1) ## change the number of words predicted
        if (is.null(inTest)) inTest <- 0
        testingDF$predictedWords[i] <- paste(inTest, collapse = " ")

}


## how many times it was guessed right?

myCount <- integer()
for (i in 1:length(testingDF$text)) {
        
my_i_Count <- grep(testingDF$myWord[i], testingDF$predictedWords[i])

myCount <- c(myCount, my_i_Count)

}

sum(myCount)/length(testingDF$line)*100 # 5% RFT

sum(testingDF$predictedWords == 1 | testingDF$predictedWords == 0)/length(testingDF$line)*100 # 36% MD







