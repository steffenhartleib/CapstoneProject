#script to create random samples of aprox 5% of lines

setwd("/Users/steffenhartleib/Google_Drive/Capstone/final/en_US")
dir()
#newslines: 1010242 
#blog lines: 899288
#twitter ilnes:2360148  

#Blog Sample file:

conN <- file("en_US.blogs.txt","r")
BlogSample <- character()
for (i in 1:899288){
        if(rbinom(1,1,.05) == 1) {
                line <- readLines(conN,1)
                BlogSample <- append(BlogSample,line)
        }
        else{
                line <- readLines(conN,1)
        }
}
write(NewsSample, "/Users/steffenhartleib/Google_Drive/Capstone/corpus/samples/NewsSample.txt")
        

# News Sample file
conN <- file("en_US.news.txt","r")
NewsSample <- character()
for (i in 1:1010242){
        if(rbinom(1,1,.05) == 1) {
                line <- readLines(conN,1)
                NewsSample <- append(NewsSample,line)
        }
        else{
                line <- readLines(conN,1)
        }
}
write(NewsSample, "/Users/steffenhartleib/Google_Drive/Capstone/corpus/samples/NewsSample.txt")

#Twitter Sample File
conT <- file("en_US.twitter.txt" ,"r")
TwitterSample <- character()
for (i in 1:2360148){
        if(rbinom(1,1,.05) == 1) {
                line <- readLines(conT,1)
                TwitterSample <- append(TwitterSample,line)
        }
        else{
                line <- readLines(conT,1)
        }
}
write(TwitterSample, "/Users/steffenhartleib/Google_Drive/Capstone/corpus/samples/TwitterSample.txt")

#-------------------------------------------------------------------------------------------------

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
NewsSample <- readLines(conN,50000)
write(NewsSample, "/Users/steffenhartleib/Google_Drive/Capstone/data/Sample5000.txt")

# blog
conN <- file("en_US.blogs.txt","r")
BlogSample <- readLines(conN,50000)
write(NewsSample, "/Users/steffenhartleib/Google_Drive/Capstone/data/Sample5000.txt", append = TRUE)

#twitter
conT <- file("en_US.twitter.txt" ,"r")
TwitterSample<- readLines(conT,50000)
write(TwitterSample, "/Users/steffenhartleib/Google_Drive/Capstone/data/Sample5000.txt", append = TRUE)


# read combined sample file into R
setwd("/Users/steffenhartleib/Google_Drive/Capstone/data")
sample5000 <- readLines("Sample5000.txt")





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

cleanSample5000 <- clean(sample5000)
write(cleanSample5000,"/Users/steffenhartleib/Google_Drive/Capstone/corpus/sampleSmall/cleanSample5000.txt")



# create corpus

setwd("/Users/steffenhartleib/Google_Drive/Capstone/corpus/sampleSmall")
con = file.path("/Users/steffenhartleib/Google_Drive/Capstone/corpus/sampleSmall")
docs <- Corpus(DirSource(con))


# make unigram
tdm <- TermDocumentMatrix(docs)
tdmMatrix <- as.matrix(tdm)
freq <- rowSums(tdmMatrix)
df.freq = as.data.frame(freq)
df.freq$terms <- row.names(df.freq)

# write unigramms to file
setwd("/Users/steffenhartleib/Google_Drive/Capstone/data")
write.csv(df.freq,"1GramsSmall.csv")


# create list of rare words

setwd("/Users/steffenhartleib/Google_Drive/Capstone/data")
uniGrams <- read.csv('1GramsSmall.csv', header = TRUE)
uniGrams$terms <- as.character(uniGrams$terms)
uniGrams <- uniGrams[,2:3]
dir()

rareWords <- filter(uniGrams, freq == 1)
rareWords <- mutate(rareWords, terms = as.character(terms))
write.csv(rareWords, "/Users/steffenhartleib/Google_Drive/Capstone/data/rareWords.csv")


# write function to loop through rare words, find them in the sample file an dreplace them with "<unk>"
UNK <- function(x){
        for (w in rareWords$terms){
                x <- gsub(paste("\\b", w, "\\b", sep = ""),"<UNK>", x)
        }
        return(x)
}

UNKcleanSample5000 <- UNK(cleanSample5000)

head(UNKcleanSample5000)


write(UNKcleanSample5000,"/Users/steffenhartleib/Google_Drive/Capstone/corpus/sampleSmallUnk/UnkCleanSample5000.txt")

# create n-grams

library(ngrams)

# create corpus with UNK + clean sample file
setwd("/Users/steffenhartleib/Google_Drive/Capstone/corpus/sampleSmallUnk")
con = file.path("/Users/steffenhartleib/Google_Drive/Capstone/corpus/sampleSmallUnk")
docs <- Corpus(DirSource(con))


options(mc.cores=1)
biGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 2, max = 2,delimiters = " "))    
tdm2 <- TermDocumentMatrix(docs, control = list(tokenize = biGramTokenizer))
tdm2Matrix <- as.matrix(tdm2)
freq <- rowSums(tdm2Matrix)
df.biGram = as.data.frame(freq)
df.biGram$terms <- row.names(df.biGram)
write.csv(df.biGram,"~/Google_Drive/Capstone/data/2GramsSmall.csv")




options(mc.cores=1)
triGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3, delimiters = " "))       
tdm3 <- TermDocumentMatrix(docs, control = list(tokenize = triGramTokenizer))
tdm3Matrix <- as.matrix(tdm3)
freq <- rowSums(tdm3Matrix)
df.triGram = as.data.frame(freq)
df.triGram$terms <- row.names(df.triGram)
write.csv(df.triGram,"~/Google_Drive/Capstone/data/3GramsSmall.csv")



options(mc.cores=1)
fourGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 4, max = 4, delimiters = " "))      
tdm4 <- TermDocumentMatrix(docs, control = list(tokenize = fourGramTokenizer))
tdm4Matrix <- as.matrix(tdm4)
freq <- rowSums(tdm4Matrix)
df.4Gram <- as.data.frame(freq)
df.4Gram$terms <- row.names(df.4Gram)
write.csv(df.4Gram,"~/Google_Drive/Capstone/data/4GramsSmall.csv")


options(mc.cores=1)
fiveGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 5, max = 5, delimiters = " "))
tdm5 <- TermDocumentMatrix(docs, control = list(tokenize = fiveGramTokenizer))
tdm5Matrix <- as.matrix(tdm5)
freq <- rowSums(tdm5Matrix)
df5 <- as.data.frame(freq)
df5$terms <- row.names(df5)
write.csv(df5,"~/Google_Drive/Capstone/data/5GramsSmall.csv")


# turn ngrams into data frames:
setwd("/Users/steffenhartleib/Google_Drive/Capstone/data")
dir()

# create data tables:

fiveGrams <- read.csv("5GramsSmall.csv", header = TRUE)
df5 <- colsplit(fiveGrams$terms, split = "\\s", names = c('Term1', 'Term2','Term3','Term4','Term5'))
df5 <- cbind(df5, fiveGrams$freq)
df5$Term1 <- as.character(df5$Term1)
df5$Term2 <- as.character(df5$Term2)
df5$Term3 <- as.character(df5$Term3)
df5$Term4 <- as.character(df5$Term4)
df5$Term5 <- as.character(df5$Term5)
df5 <- filter(df5, Term1 != Term2, Term2 != Term3, Term3 != Term4, Term4 != Term5) # remove repeat words
df5$Term1[grep("<unk>.|.<unk>",df5$Term1)] <- '<unk>'
df5$Term2[grep("<unk>.|.<unk>",df5$Term2)] <- '<unk>'
df5$Term3[grep("<unk>.|.<unk>",df5$Term3)] <- '<unk>'
df5$Term4[grep("<unk>.|.<unk>",df5$Term4)] <- '<unk>'
df5$Term5[grep("<unk>.|.<unk>",df5$Term5)] <- '<unk>'
names(df5)[dim(df5)[2]] <- "freq"
df5$freq <- as.numeric(df5$freq)
dt5 <- data.table(df5)
saveRDS(dt5,file="/Users/steffenhartleib/Google_Drive/Capstone/model/5GramsDTSmall.rds")


fourGrams <- read.csv("4GramsSmall.csv", header = TRUE)
df4 <- colsplit(fourGrams$terms, split = "\\s", names = c('Term1', 'Term2','Term3','Term4'))
df4 <- cbind(df4, fourGrams$freq)
df4$Term1 <- as.character(df4$Term1)
df4$Term2 <- as.character(df4$Term2)
df4$Term3 <- as.character(df4$Term3)
df4$Term4 <- as.character(df4$Term4)
df5 <- filter(df5, Term1 != Term2, Term2 != Term3, Term3 != Term4) 
names(df4)[dim(df4)[2]] <- "freq"
df4$freq <- as.numeric(df4$freq)
df5$Term1[grep("<unk>.|.<unk>",df5$Term1)] <- '<unk>'
df5$Term2[grep("<unk>.|.<unk>",df5$Term2)] <- '<unk>'
df5$Term3[grep("<unk>.|.<unk>",df5$Term3)] <- '<unk>'
df5$Term4[grep("<unk>.|.<unk>",df5$Term4)] <- '<unk>'
dt4 <- data.table(df4)
saveRDS(dt4,file="/Users/steffenhartleib/Google_Drive/Capstone/model/4GramsDTSmall.rds")


triGrams <- read.csv("3GramsSmall.csv", header = TRUE)
df3 <- colsplit(triGrams$terms, split = "\\s", names = c('Term1', 'Term2','Term3'))
df3$Term1 <- as.character(df3$Term1)
df3$Term2 <- as.character(df3$Term2)
df3$Term3 <- as.character(df3$Term3)
df3 <- cbind(df3, triGrams$freq)
df3 <- filter(df3, Term1 != Term2, Term2 != Term3) 
names(df3)[dim(df3)[2]] <- "freq"
df3$freq <- as.numeric(df3$freq)
df3$Term1[grep("<unk>.|.<unk>",df5$Term1)] <- '<unk>'
df3$Term2[grep("<unk>.|.<unk>",df5$Term2)] <- '<unk>'
df3$Term3[grep("<unk>.|.<unk>",df5$Term3)] <- '<unk>'
dt3 <- data.table(df3)
saveRDS(dt3,file="/Users/steffenhartleib/Google_Drive/Capstone/model/3GramsDTSmall.rds")


biGrams <- read.csv("2GramsSmall.csv", header = TRUE)
df2 <- colsplit(biGrams$terms, split = "\\s", names = c('Term1', 'Term2'))
df2 <- cbind(df2, biGrams$freq)
df2$Term1 <- as.character(df2$Term1)
df2$Term2 <- as.character(df2$Term2)
df2 <- filter(df2, Term1 != Term2) 
df2$Term1[grep("<unk>.|.<unk>",df5$Term1)] <- '<unk>'
df2$Term2[grep("<unk>.|.<unk>",df5$Term2)] <- '<unk>'
names(df2)[dim(df2)[2]] <- "freq"
df2$freq <- as.numeric(df2$freq)
dt2 <- data.table(df2)
saveRDS(dt2,file="/Users/steffenhartleib/Google_Drive/Capstone/model/2GramsDTSmall.rds")








