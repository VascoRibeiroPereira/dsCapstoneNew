# Data wrangling
library(tidyverse)

# Text processing
library(tidytext)
library(textclean)
library(tokenizers) 

# Markov Chain
library(markovchain) 
library(tokenizers) 

library(tm) 

predictive_text <- function(text, num_word) {
        
        text <- text %>%
                replace_contraction() %>%
                removePunctuation() %>%
                replace_money() %>%
                replace_emoticon() %>%
                replace_symbol() %>%
                replace_word_elongation() %>%
                replace_ordinal() %>%
                replace_number() %>%
                tolower() %>%
                removeWords(stopwords()) %>%
                stripWhitespace() %>%
                trimws()
        
        word_count <- count_words(text)
        
        
        
        # Check if it has 5 or more words
        if (word_count >= 5) {
                input_text <- strsplit(text, " ") %>% unlist() %>% tail(5) %>% paste(collapse = " ")
                
                # If cannot find the transition in trigram
                if (isTRUE(tryCatch(markov_pentagram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                        
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(4) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in bigram
                        if (isTRUE(tryCatch(markov_tetragram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(3) %>% paste(collapse = " ")
                                
                                if (isTRUE(tryCatch(markov_trigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                        
                                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                                        
                                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                                
                                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                        
                                        suggest <- markov_unigram$estimate[ tolower(input_text), ] %>%
                                                sort(decreasing = T) %>% 
                                                head(num_word) 
                                        
                                        suggest[suggest > 0] %>% 
                                                names() 
                                        } else {
                                        
                                        # Can find the transition in bigram
                                        suggest <- markov_bigram$estimate[ tolower(input_text), ] %>% # tolower may not be need
                                                sort(decreasing = T) %>% 
                                                head(num_word) 
                                        
                                        suggest[suggest > 0] %>% 
                                                names() %>%
                                                str_extract(pattern = "\\s(.*)") %>% 
                                                str_remove("[ ]")
                                        }
                                } else {
                                
                                # Can find the transition in trigram
                                suggest <- markov_trigram$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]") %>%  
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]") 
                                }
                        } else {
                        
                        # Can find the transition in tetragram
                        suggest <- markov_tetragram$estimate[ tolower(input_text), ] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() %>%
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") %>%  
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") 
                        }
                
                } else {
        
                # Can find the transition in pentagram
                suggest <- markov_pentagram$estimate[ tolower(input_text), ] %>%
                        sort(decreasing = T) %>% 
                        head(num_word) 
                
                suggest[suggest > 0] %>% 
                        names() %>%
                        str_extract(pattern = "\\s(.*)") %>% 
                        str_remove("[ ]") %>%  
                        str_extract(pattern = "\\s(.*)") %>% 
                        str_remove("[ ]") 
                }
                
                } else {
        
        
        # Check if it has 4 or more words
                if (word_count >= 4) {
                input_text <- strsplit(text, " ") %>% unlist() %>% tail(4) %>% paste(collapse = " ")
                
                # If cannot find the transition in trigram
                if (isTRUE(tryCatch(markov_tetragram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                        
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(3) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in bigram
                        if (isTRUE(tryCatch(markov_trigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                                
                                if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                        
                                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                
                                suggest <- markov_unigram$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() 
                                } else {
                                
                                # Can find the transition in bigram
                                suggest <- markov_bigram$estimate[ tolower(input_text), ] %>% # tolower may not be need
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]")
                                }
                        } else {
                        
                        # Can find the transition in trigram
                        suggest <- markov_trigram$estimate[ tolower(input_text), ] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() %>%
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") %>%  
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") 
                        }
                } else {
                        
                        # Can find the transition in tetragram
                        suggest <- markov_tetragram$estimate[ tolower(input_text), ] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() %>%
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") %>%  
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") 
                }
                
        } else {
                
        # Check if it has 3 or more words
        if (word_count >= 3) {
                input_text <- strsplit(text, " ") %>% unlist() %>% tail(3) %>% paste(collapse = " ") #should also clean from stop words first - add that code to beginnig
                
                # If cannot find the transition in trigram
                if (isTRUE(tryCatch(markov_trigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                        
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in bigram
                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                
                                suggest <- markov_unigram$estimate[ tolower(input_text), ] %>% # to lower not need if pre-treated
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() 
                        } else {
                                
                                # Can find the transition in bigram
                                suggest <- markov_bigram$estimate[ tolower(input_text), ] %>% # tolower may not be need
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]")
                        }
                } else {
                        
                        # Can find the transition in trigram
                        suggest <- markov_trigram$estimate[ tolower(input_text), ] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() %>%
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") %>%  
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") 
                }
                
        } else {
                
                # Check if the words is 2 or more
                if (word_count >= 2) {
                        
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in bigram
                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                
                                suggest <- markov_unigram$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() 
                        } else {
                                
                                # Can find the transition in bigram
                                suggest <- markov_bigram$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]")
                        }
                        
                } else {
                        
                        # If only has 1-gram
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                        
                        # exclude punctuation
                        punctuation <- which(markov_unigram$estimate[ tolower(input_text), ] %>% names() %>% str_detect("[:punct:]"))
                        
                        suggest <- markov_unigram$estimate[ tolower(input_text), -punctuation] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() 
                                }
                
                        }
                
                }
        }
}




######## Another version of the same model:


predictive_text_alternative <- function(text, num_word) {
        
        text <- text %>%
                replace_contraction() %>%
                removePunctuation() %>%
                replace_money() %>%
                replace_emoticon() %>%
                replace_symbol() %>%
                replace_word_elongation() %>%
                replace_ordinal() %>%
                replace_number() %>%
                tolower() %>%
                removeWords(stopwords()) %>%
                stripWhitespace() %>%
                trimws()
        
        word_count <- count_words(text)
        
        
        
        # Check if it has 5 or more words
        if (word_count >= 5) {
                input_text <- strsplit(text, " ") %>% unlist() %>% tail(5) %>% paste(collapse = " ")
                
                # If cannot find the transition in trigram
                if (isTRUE(tryCatch(markov_pentagram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                        
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(4) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in bigram
                        if (isTRUE(tryCatch(markov_tetragram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(3) %>% paste(collapse = " ")
                                
                                if (isTRUE(tryCatch(markov_trigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                        
                                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                                        
                                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                                
                                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                                
                                                suggest <- markov_unigram$estimate[ input_text, ] %>%
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() 
                                        } else {
                                                
                                                # Can find the transition in bigram
                                                suggest <- markov_bigram$estimate[ input_text, ] %>% # tolower may not be need
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() %>%
                                                        str_extract(pattern = "\\s(.*)") %>% 
                                                        str_remove("[ ]")
                                        }
                                } else {
                                        
                                        # Can find the transition in trigram
                                        suggest <- markov_trigram$estimate[ input_text, ] %>%
                                                sort(decreasing = T) %>% 
                                                head(num_word) 
                                        
                                        suggest[suggest > 0] %>% 
                                                names() %>%
                                                str_extract(pattern = "\\s(.*)") %>% 
                                                str_remove("[ ]") %>%  
                                                str_extract(pattern = "\\s(.*)") %>% 
                                                str_remove("[ ]") 
                                }
                        } else {
                                
                                # Can find the transition in tetragram
                                suggest <- markov_tetragram$estimate[ input_text, ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]") %>%  
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]") 
                        }
                        
                } else {
                        
                        # Can find the transition in pentagram
                        suggest <- markov_pentagram$estimate[ input_text, ] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() %>%
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") %>%  
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") 
                }
                
        } else {
                
                
                # Check if it has 4 or more words
                if (word_count >= 4) {
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(4) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in trigram
                        if (isTRUE(tryCatch(markov_tetragram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(3) %>% paste(collapse = " ")
                                
                                # If cannot find the transition in bigram
                                if (isTRUE(tryCatch(markov_trigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                        
                                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                                        
                                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                                
                                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                                
                                                suggest <- markov_unigram$estimate[ input_text, ] %>%
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() 
                                        } else {
                                                
                                                # Can find the transition in bigram
                                                suggest <- markov_bigram$estimate[ input_text, ] %>% # tolower may not be need
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() %>%
                                                        str_extract(pattern = "\\s(.*)") %>% 
                                                        str_remove("[ ]")
                                        }
                                } else {
                                        
                                        # Can find the transition in trigram
                                        suggest <- markov_trigram$estimate[ input_text, ] %>%
                                                sort(decreasing = T) %>% 
                                                head(num_word) 
                                        
                                        suggest[suggest > 0] %>% 
                                                names() %>%
                                                str_extract(pattern = "\\s(.*)") %>% 
                                                str_remove("[ ]") %>%  
                                                str_extract(pattern = "\\s(.*)") %>% 
                                                str_remove("[ ]") 
                                }
                        } else {
                                
                                # Can find the transition in tetragram
                                suggest <- markov_tetragram$estimate[ input_text, ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]") %>%  
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]") 
                        }
                        
                } else {
                        
                        # Check if it has 3 or more words
                        if (word_count >= 3) {
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(3) %>% paste(collapse = " ") #should also clean from stop words first - add that code to beginnig
                                
                                # If cannot find the transition in trigram
                                if (isTRUE(tryCatch(markov_trigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                        
                                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                                        
                                        # If cannot find the transition in bigram
                                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                                
                                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                                
                                                suggest <- markov_unigram$estimate[ input_text, ] %>% # to lower not need if pre-treated
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() 
                                        } else {
                                                
                                                # Can find the transition in bigram
                                                suggest <- markov_bigram$estimate[ input_text, ] %>% # tolower may not be need
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() %>%
                                                        str_extract(pattern = "\\s(.*)") %>% 
                                                        str_remove("[ ]")
                                        }
                                } else {
                                        
                                        # Can find the transition in trigram
                                        suggest <- markov_trigram$estimate[ input_text, ] %>%
                                                sort(decreasing = T) %>% 
                                                head(num_word) 
                                        
                                        suggest[suggest > 0] %>% 
                                                names() %>%
                                                str_extract(pattern = "\\s(.*)") %>% 
                                                str_remove("[ ]") %>%  
                                                str_extract(pattern = "\\s(.*)") %>% 
                                                str_remove("[ ]") 
                                }
                                
                        } else {
                                
                                # Check if the words is 2 or more
                                if (word_count >= 2) {
                                        
                                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                                        
                                        # If cannot find the transition in bigram
                                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                                
                                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                                
                                                suggest <- markov_unigram$estimate[ input_text, ] %>%
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() 
                                        } else {
                                                
                                                # Can find the transition in bigram
                                                suggest <- markov_bigram$estimate[ input_text, ] %>%
                                                        sort(decreasing = T) %>% 
                                                        head(num_word) 
                                                
                                                suggest[suggest > 0] %>% 
                                                        names() %>%
                                                        str_extract(pattern = "\\s(.*)") %>% 
                                                        str_remove("[ ]")
                                        }
                                        
                                } else {
                                        
                                        # If only has 1-gram
                                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                        
                                        suggest <- markov_unigram$estimate[ input_text, ] %>%
                                                sort(decreasing = T) %>% 
                                                head(num_word) 
                                        
                                        suggest[suggest > 0] %>% 
                                                names() 
                                }
                                
                        }
                        
                }
        }
}


