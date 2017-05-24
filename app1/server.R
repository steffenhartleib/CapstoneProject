## server.R
library(dplyr)
library(reshape)
library(stringr)
library(data.table)
library(shiny)


# load the data tables
#setwd("/Users/steffenhartleib/Google_Drive/Capstone/model")
dt5 <- readRDS("data/5GramsDTSmall.rds")
dt4 <- readRDS("data/4GramsDTSmall.rds")
dt3 <- readRDS("data/3GramsDTSmall.rds")
dt2 <- readRDS("data/2GramsDTSmall.rds")


# create functions for n-grams

check5Gram <- function(t1,t2,t3,t4) { 
        if (t1 %in% dt5$Term1) t1 = t1 else t1 <- "<unk>"
        if (t2 %in% dt5$Term2) t2 = t2 else t2 <- "<unk>"
        if (t3 %in% dt5$Term3) t3 = t3 else t3 <- "<unk>"
        if (t4 %in% dt5$Term2) t3 = t3 else t3 <- "<unk>"
        dfin4 <- filter(dt5, Term1 == t1 & Term2 == t2 & Term3 == t3, Term4 == t4, Term5 != '<unk>')
        if(dim(dfin4)[1] == 0){
                Term = 'na'
                Prob = 0
                result4 <- as.data.frame(cbind(Term,Prob))
                result4 <- mutate(result4,Prob = as.numeric(as.character(Prob)))
        }
        else {
                dfin4 <- mutate(dfin4, prob = freq/(sum(freq)))
                result4 <- select(dfin4,Term5,prob) 
                result4 <- filter(result4, prob == max(prob))
                result4 <- setnames(result4,c("Term","Prob"))     
        }
        return(result4)
}




check4Gram <- function(t1,t2,t3) { 
        if (t1 %in% dt4$Term1) t1 = t1 else t1 <- "<unk>"
        if (t2 %in% dt4$Term2) t2 = t2 else t2 <- "<unk>"
        if (t3 %in% dt4$Term3) t3 = t3 else t3 <- "<unk>"
        dfin3 <- filter(dt4, Term1 == t1, Term2 == t2, Term3 == t3, Term4 != '<unk>')
        if(dim(dfin3)[1] == 0){
                Term = 'na'
                Prob = 0
                result3 <- as.data.frame(cbind(Term,Prob))
                result3 <- mutate(result3,Prob = as.numeric(as.character(Prob)))
        }
        else {
                dfin3 <- mutate(dfin3, prob = freq/(sum(freq)))
                result3 <- select(dfin3,Term4,prob)
                result3 <- filter(result3, prob == max(prob))
                result3<- setnames(result3,c("Term","Prob"))
        }
        return(result3)
        
}

check3Gram <- function(t1,t2) { 
        if (t1 %in% dt3$Term1) t1 = t1 else t2 <- "<unk>"
        if (t2 %in% dt3$Term2) t2 = t2 else t2 <- "<unk>"
        dfin2 <- filter(dt3, Term1 == t1,  Term2 == t2, Term3 != "<unk>")
        
        if(dim(dfin2)[1] == 0){
                Term = 'na'
                Prob <- 0
                result2 <- data.frame(cbind(Term,Prob))
                result2 <- mutate(result2,Prob = as.numeric(as.character(Prob)))
        }
        else {
                dfin2 <- mutate(dfin2, prob = freq/(sum(freq)))
                result2 <- select(dfin2,Term3,prob)
                result2 <- filter(result2, prob == max(prob))
                results2 <- setnames(result2,c("Term","Prob"))
        }
        return(result2)
        
}


check2Gram <- function(t1) { 
        if (t1 %in% dt2$Term1) t1 <- t1 else t1 <- "<unk>"
        dfin1 <- filter(dt2, Term1 == t1 & Term2 != '<unk>')
        
        if(dim(dfin1)[1] == 0){
                Term = 'na'
                Prob = 0
                result1 <- as.data.frame(cbind(Term,Prob))
                result1 <- mutate(result1,Prob = as.numeric(as.character(Prob)))
        }
        else {
                dfin1 <- mutate(dfin1, prob = freq/(sum(freq)))
                result1 <- select(dfin1,Term2,prob)
                result1 <- filter(result1, prob == max(prob))
                result1 <- setnames(result1,c("Term","Prob"))
        }
        return(result1) 
}


clean <- function(file){
        file <- tolower(file)
        file <- gsub("[^[:alpha:]['’-]", " ", file) # remove no's and punctuation except "-" and "'"
        file <- gsub("['’-](?=\\s)|(?<=\\s)['’-]"," ", file ,perl=TRUE)#remove "-" and "'" with spaces around it # remove no's and punctuation except "-" and "'"
        file <- gsub("[^[a-z]-"," ", file ,perl=TRUE)
        #file <- gsub("\\s['’-]"," ",file, perl = TRUE)
        #file <- gsub("^['’-]"," ",file, perl = TRUE)
        #file <- gsub("[\\[\\]]"," ", file, perl = TRUE)
        #file <- gsub("\\su\\ss\\s"," u.s. ", file, perl = TRUE)
        #file <- gsub("\\su\\ss\\sa\\s"," u.s.a. ", file, perl = TRUE)
        #file <- gsub("\\su\\sk\\s"," u.k. ", file, perl = TRUE)
        file <- gsub("\\s+", " ", str_trim(file), perl=TRUE)
        return(file)
}


# prediction function returning prob table
nextWord <- function(x) {
        i <- clean(x)
        c <- str_count(i," ")
        t1 <- ""
        t2 <- ""
        t3 <- ""
        t4 <- ""
        if(c==0) t4 <- word(i,-1,-1)  #one word input
        
        if(c==1){
                t3 <- word(i,-2,-2)
                t4 <- word(i,-1,-1)   #two word input 
        }
        
        if(c==2){
                t2 <- word(i,-3,-3)
                t3 <- word(i,-2,-2)
                t4 <- word(i,-1,-1)  #three word input
        }      
        
        if(c>=3){
                t1 <- word(i,-4,-4)
                t2 <- word(i,-3,-3)
                t3 <- word(i,-2,-2)
                t4 <- word(i,-1,-1)
        }
        
   
        #one word input
        if(t1 == "" & t2 == "" & t3 == "" & t4 !="") {
                pred1 <- check2Gram(t4)
                pred2 <- data.frame(Term = "na", Prob = as.numeric(0))
                pred3 <- data.frame(Term = "na", Prob = as.numeric(0))
                pred4 <- data.frame(Term = "na", Prob = as.numeric(0))
                results <- rbind(pred1)
                
        }
        #two word input
        else if(t1 == "" & t2 == "") 
        {
                pred1 <- check2Gram(t4)
                pred2 <- check3Gram(t3,t4)
                pred3 <- data.frame(Term = "na", Prob = as.numeric(0))
                pred4 <- data.frame(Term = "na", Prob = as.numeric(0))
                results <- rbind(pred2,pred1)
        }
        #three word input
        
        else if(t1 == "") 
        {
                pred1 <- check2Gram(t4)
                pred2 <- check3Gram(t3,t4)
                pred3 <- check4Gram(t2,t3,t4)
                pred4 <- data.frame(Term = "na", Prob = as.numeric(0))
                results <- rbind(pred3,pred2,pred1)
        }
        
        #four word input
        else
        {
                
                pred4 <- check5Gram(t1,t2,t3,t4)
                pred4 <- mutate(pred4, Prob = as.numeric(Prob))
                pred3 <- check4Gram(t2,t3,t4)
                pred3 <- mutate(pred3, Prob = as.numeric(Prob)*.4)
                pred2 <- check3Gram(t3,t4)
                pred2 <- mutate(pred2, Prob = as.numeric(Prob)*.4*.4)
                pred1 <- check2Gram(t4)
                pred1 <- mutate(pred1, Prob = as.numeric(Prob)*.4*.4*.4)
                
                
        }
        
        results <- rbind(pred4,pred3,pred2,pred1)
        results <- group_by(results,results$Term)
        results <- summarize(results, probSum = sum(Prob))
        #results <- filter(results, probSum == max(probSum))
        #results <- data.frame(results)
        #results <- results$results.Term
        return(results)
}


topResult <- function(x){
        topPred <- filter(x, probSum == max(probSum))
        topPred <- data.frame(topPred)
        topPred <- topPred$results.Term
        return(as.character(topPred[1]))
}


altResult <- function(resultList){
        altPred <- filter(resultList, probSum > 0)
        colnames(altPred) <- c("Word", "Probability")
        altPred$Probability <- round(altPred$Probability,2) 
        return(data.frame(altPred))
}

#resultList <- nextWord("he confessed to the")
#topPred <- topResult(resultList)
#altPred <- altResult(resultList)


shinyServer(function(input, output) {
        output$outputValueWord <- renderPrint({topResult(nextWord(input$text))})
        output$outputValueList <- renderPrint({altResult(nextWord(input$text))})
})