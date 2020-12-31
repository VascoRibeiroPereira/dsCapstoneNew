## Get the data

if (!file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      "Coursera-SwiftKey.zip")
        unzip("Coursera-SwiftKey.zip")
}


## source Libraries and Functions

source(paste(getwd(),"/Rcode/getCleanData_fun.R", sep=""))

## Clean data

### Random selection of data

enDataSubset <- c(subsetBigData(paste(getwd(), "/final/en_US/en_US.twitter.txt", sep=""), 0.1),
                  subsetBigData(paste(getwd(), "/final/en_US/en_US.news.txt", sep=""), 0.2))## original is .2

## Transform data to Corpus and clean with an anonymous function

corp_toClean <- VCorpus(VectorSource(enDataSubset))

## With stop words

#twitterDataClean <- clean_corpus(corp_toClean)

## without Stop Words
twitterDataCleanSW <- clean_corpusSW(corp_toClean)


## Corpus to string
#twitter_str <- as.character(unlist(twitterDataClean))
twitter_str2 <- as.character(unlist(twitterDataCleanSW)) # - run this next

## Remove NA introduced by the cleaning algorithm
#twitter_str <- twitter_str[!is.na(twitter_str)]
twitter_str2 <- twitter_str2[!is.na(twitter_str2)] # - run this next


## Preprocess
#twitter_final <- as.character()
#
#for (i in 1:length(twitter_str)){
#        
#        tmp <- preprocess(twitter_str[i], case = "lower", 
#                          remove.punct = TRUE, 
#                          remove.numbers = TRUE, 
#                          fix.spacing = TRUE) %>% 
#                str_trim()
#        
#        twitter_final <- c(twitter_final, tmp)
#        
#}

#twitter_Subset <- twitter_final[count_words(twitter_final) > 1]

#twitterDF <- tibble(line = 1:length(twitter_Subset), text = twitter_Subset)



#write_csv(twitterDF, "twitter_subset.csv")


## Pre Process for no stop words
twitter_final2 <- as.character()

for (i in 1:length(twitter_str2)){
        
        tmp <- preprocess(twitter_str2[i], case = "lower", 
                          remove.punct = TRUE, 
                          remove.numbers = TRUE, 
                          fix.spacing = TRUE) %>% 
                str_trim()
        
        twitter_final2 <- c(twitter_final2, tmp)
        
}

twitter_Subset2 <- gsub('\\b\\w{1,2}\\b','', twitter_final2)

twitter_Subset2 <- stripWhitespace(trimws(twitter_Subset2))

twitter_Subset2 <- twitter_Subset2[count_words(twitter_Subset2) > 1]

twitterDF2 <- tibble(line = 1:length(twitter_Subset2), text = twitter_Subset2)


write_csv(twitterDF2, "withoutSW.csv")
