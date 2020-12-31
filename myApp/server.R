#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries
library(shiny)
library(textclean)
library(tm)
library(stringr)
library(lexicon)
library(tokenizers)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    observeEvent(input$resetAll, {
        reset("userInput")
        reset("numWords")
        output$text <- renderText({
            
        })
    })
    
    ## Functions
    
    ####################################################################################
    
    ## Expanding the contractions lexicon
    new_con <- data.frame("here's", "here is") 
    names(new_con) <- c("contraction", "expanded")
    key_contractions <- rbind(key_contractions, new_con)
    
    ## Expanding Slang df
    new_slang <- data.frame("RT", "Retweet") 
    names(new_slang) <- c("x", "y")
    hash_internet_slang <- rbind(hash_internet_slang, new_slang)
    
    ## Cleaning and coercing all profanity data sources
    alvarez_alternative <- str_replace_all(profanity_alvarez, "\\*", "\\\\*")
    alvarez_alternative <- str_replace_all(alvarez_alternative, "\\(", "\\\\(")
    anger_alternative <- str_replace_all(profanity_zac_anger, "\\*", "\\\\*")
    anger_alternative <- str_replace_all(anger_alternative, "\\(", "\\\\(")
    profaneWords <- unique(tolower(c(profanity_arr_bad, 
                                     profanity_banned,
                                     profanity_racist,
                                     alvarez_alternative,
                                     anger_alternative)))
    
    ####################################################################################
    
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
    
    
    ####################################################################################
    
    load("markov_unigram.RData")
    load("markov_bigram.RData")
    load("markov_trigram.RData")
    load("markov_tetragram.RData")
    load("markov_pentagram.RData")
    
    ####################################################################################
    
    
    numVals <- eventReactive(input$userRun, {
        input$numWords
        
    })
    
    my_prediction <- eventReactive(input$userRun, {
        tryCatch(predictive_text_alternative(input$userInput, numVals()), error = function(e) "No prediction for this input, try another one")
    })
    
    output$text <- renderText({
        my_prediction()
    })
    

})
