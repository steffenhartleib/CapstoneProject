# Create test set:

#  Create Samples, clean, tokenize, replace UNK and make n-grams

options( java.parameters = "-Xmx4g" )
library(RWeka)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape)
library(data.table)
options(scipen=999)


#Create small samples for building models
setwd("/Users/steffenhartleib/Google_Drive/Capstone/final/en_US")



# news
conN <- file("en_US.news.txt","r")
NewsSample <- readLines(conN,60000)
NewsSample <- NewsSample[50001:60000]
write(NewsSample, "/Users/steffenhartleib/Google_Drive/Capstone/data/testSetSample.txt")

# blog
conN <- file("en_US.blogs.txt","r")
BlogSample <- readLines(conN,60000)
BlogSample <- BlogSample[50001:60000]
write(NewsSample, "/Users/steffenhartleib/Google_Drive/Capstone/data/testSetSample.txt", append = TRUE)

#twitter
conT <- file("en_US.twitter.txt" ,"r")
TwitterSample <- readLines(conT,60000)
TwitterSample <- TwitterSample[50001:60000]
write(TwitterSample, "/Users/steffenhartleib/Google_Drive/Capstone/data/testSetSample.txt", append = TRUE)

# read combined sample file into R
setwd("/Users/steffenhartleib/Google_Drive/Capstone/data")
testSetSample <- readLines("testSetSample.txt")


# write function to clean the data
clean <- function(file){
        file <- tolower(file)
        file <- gsub("[^[:alpha:]['’-]", " ", file) # remove no's and punctuation except "-" and "'"
        file <- gsub("['’-](?=\\s)|(?<=\\s)['’-]"," ", file ,perl=TRUE)#remove "-" and "'" with spaces around it # remove no's and punctuation except "-" and "'"
        file <- gsub("[^[a-z]-"," ", file ,perl=TRUE)
        file <- gsub("\\s['’-]"," ",file, perl = TRUE)
        file <- gsub("^['’-]"," ",file, perl = TRUE)
        file <- gsub("[\\[\\]]"," ", file, perl = TRUE)
        file <- gsub("\\su\\ss\\s"," u.s. ", file, perl = TRUE)
        file <- gsub("\\su\\ss\\sa\\s"," u.s.a. ", file, perl = TRUE)
        file <- gsub("\\su\\sk\\s"," u.k. ", file, perl = TRUE)
        file <- gsub("\\s+", " ", str_trim(file))
        return(file)
}

cleanTestSetSample <- clean(testSetSample)
write(cleanTestSetSample,"/Users/steffenhartleib/Google_Drive/Capstone/corpus/testSetSamples/cleanTestSetSamples.txt")

# create corpus

setwd("/Users/steffenhartleib/Google_Drive/Capstone/corpus/testSetSamples")
con = file.path("/Users/steffenhartleib/Google_Drive/Capstone/corpus/testSetSamples")
docs <- Corpus(DirSource(con))


# create n-grams

library(ngrams)

options(mc.cores=1)
fiveGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 5, max = 5, delimiters = " "))
tdm5 <- TermDocumentMatrix(docs, control = list(tokenize = fiveGramTokenizer))
tdm5Matrix <- as.matrix(tdm5)
freq <- rowSums(tdm5Matrix)
df5 <- as.data.frame(freq)
df5$terms <- row.names(df5)
write.csv(df5,"~/Google_Drive/Capstone/data/5GramsTestSet.csv")


# turn ngrams into data frames:
setwd("/Users/steffenhartleib/Google_Drive/Capstone/data")
dir()


fiveGrams <- read.csv("5GramsTestSet.csv", header = TRUE)

wordfun <- function(x) word(as.character(x), -1,-1)
#wordfun(fiveGrams[1:4,1])

phrasefun <- function(x) word(as.character(x),-5,-2)
#phrasefun(fiveGrams[1:4,1])


True <- sapply(fiveGrams[,1], wordfun)
True <- as.data.frame(True)

Phrase <- sapply(fiveGrams[,1], phrasefun)
Phrase <- as.data.frame(Phrase)
TestSet <- cbind(Phrase, True)

saveRDS(TestSet,file="/Users/steffenhartleib/Google_Drive/Capstone/data/TestSet.rds")








