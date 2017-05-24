library(dplyr)
library(reshape)
library(stringr)
library(data.table)

setwd("/Users/steffenhartleib/Google_Drive/Capstone/corpus/clean/tokens")
dir()

# create data tables:

fiveGrams <- read.csv("5Grams.csv", header = TRUE)
df5 <- colsplit(fiveGrams$terms, split = "\\s", names = c('Term1', 'Term2','Term3','Term4','Term5'))
df5 <- cbind(df5, fiveGrams$freq)
df5$Term1 <- as.character(df5$Term1)
df5$Term2 <- as.character(df5$Term2)
df5$Term3 <- as.character(df5$Term3)
df5$Term4 <- as.character(df5$Term4)
df5$Term5 <- as.character(df5$Term5)
df5 <- filter(df5, Term1 != Term2, Term2 != Term3, Term3 != Term4, Term4 != Term5) # remove repeat words
names(df5)[dim(df5)[2]] <- "freq"
df5$freq <- as.numeric(df5$freq)
dt5 <- data.table(df5)
saveRDS(dt5,file="/Users/steffenhartleib/Google_Drive/Capstone/model/5GramsDT.rds")


fourGrams <- read.csv("4Grams.csv", header = TRUE)
df4 <- colsplit(fourGrams$terms, split = "\\s", names = c('Term1', 'Term2','Term3','Term4'))
df4 <- cbind(df4, fourGrams$freq)
df4$Term1 <- as.character(df4$Term1)
df4$Term2 <- as.character(df4$Term2)
df4$Term3 <- as.character(df4$Term3)
df4$Term4 <- as.character(df4$Term4)
names(df4)[dim(df4)[2]] <- "freq"
df4$freq <- as.numeric(df4$freq)
dt4 <- data.table(df4)
saveRDS(dt4,file="/Users/steffenhartleib/Google_Drive/Capstone/model/4GramsDT.rds")


triGrams <- read.csv("triGrams.csv", header = TRUE)
df3 <- colsplit(triGrams$terms, split = "\\s", names = c('Term1', 'Term2','Term3'))
df3$Term1 <- as.character(df3$Term1)
df3$Term2 <- as.character(df3$Term2)
df3$Term3 <- as.character(df3$Term3)
df3 <- cbind(df3, triGrams$freq)
names(df3)[dim(df3)[2]] <- "freq"
df3$freq <- as.numeric(df3$freq)
dt3 <- data.table(df3)
saveRDS(dt3,file="/Users/steffenhartleib/Google_Drive/Capstone/model/3GramsDT.rds")


biGrams <- read.csv("biGrams.csv", header = TRUE)
df2 <- colsplit(biGrams$terms, split = "\\s", names = c('Term1', 'Term2'))
df2 <- cbind(df2, biGrams$freq)
df2$Term1 <- as.character(df2$Term1)
df2$Term2 <- as.character(df2$Term2)
names(df2)[dim(df2)[2]] <- "freq"
df2$freq <- as.numeric(df2$freq)
dt2 <- data.table(df2)
saveRDS(dt2,file="/Users/steffenhartleib/Google_Drive/Capstone/model/2GramsDT.rds")
