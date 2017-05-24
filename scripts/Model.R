library(dplyr)
library(reshape)
library(stringr)
library(data.table)

# load the data tables
setwd("/Users/steffenhartleib/Google_Drive/Capstone/model")
dt5 <- readRDS("5GramsDT.rds")
dt4 <- readRDS("4GramsDT.rds")
dt3 <- readRDS("3GramsDT.rds")
dt2 <- readRDS("2GramsDT.rds")

# create functions for n-grams

check5Gram <- function(t1,t2,t3,t4) { 
        dfin4 <- filter(dt5, Term1 == t1 & Term2 == t2 & Term3 == t3, Term4 == t4)
        dfin4 <- mutate(dfin4, prob = freq/(sum(freq)))
        result4 <- select(dfin4,Term5,prob)
        result4 <- filter(result4, prob == max(prob))
        result4 <- setnames(result4,c("Term","Prob"))
        return(result4)          
}
check5Gram(t1,t2,t3,t4)

check4Gram <- function(t1,t2,t3) { 
        dfin3 <- filter(dt4, Term1 == t1 & Term2 == t2 & Term3 == t3)
        dfin3 <- mutate(dfin3, prob = freq/(sum(freq)))
        result3 <- select(dfin3,Term4,prob)
        result3 <- filter(result3, prob == max(prob))
        result3<- setnames(result3,c("Term","Prob"))
        return(result3)
        #result3 <- dfin3[dfin3$prob == max(dfin3$prob),"Term4"]
        #prob3 <- dfin3[dfin3$prob == max(dfin3$prob),"prob"]
        #if(length(result3) > 0) output <- result3
        #else output <-"NULL"
        #return(dfin3)           
}

check4Gram(t2,t3,t4)


check3Gram <- function(t2,t3) { 
        # if t2 %in% term1 t2 = t2 Else t2 <- "<unk>"
        # if t3 %in% Term2 t3 = t3 Esle t3 <- "<unk>
        dfin2 <- filter(dt3, Term1 == t2 & Term2 == t3)
        dfin2 <- mutate(dfin2, prob = freq/(sum(freq)))
        result2 <- select(dfin2,Term3,prob)
        result2 <- filter(result2, prob == max(prob))
        results2 <- setnames(result2,c("Term","Prob"))
        return(result2)
        #result2 <- dfin2[dfin2$prob == max(dfin2$prob),"Term3"]
        #if(length(result2) > 0) output <- result2
        #else output <-"NULL"
        #return(dfin2)
        #return(dfin2)
}
check3Gram(t3,t4)


check2Gram <- function(t3) { 
        dfin1 <- filter(dt2, Term1 == t3)
        dfin1 <- mutate(dfin1, prob = freq/(sum(freq)))
        #result1 <- dfin1[dfin1$prob == max(dfin1$prob),"Term2"]
        result1 <- select(dfin1,Term2,prob)
        result1 <- filter(result1, prob == max(prob))
        result1 <- setnames(result1,c("Term","Prob"))
        #if(length(result1) > 0) output <- result1
        #else output <-"no match"
        return(result1) 
}
check2Gram(t4)

# prediction function returning prob table
nextWord <- function(x) {
        i <- tolower(x)
        c <- str_count(i," ")
        t1 <- ""
        t2 <- ""
        t3 <- ""
        t4 <- ""
        if(c==0) t4 <- word(i,-1,-1)
        
        if(c==1){
                t3 <- word(i,-2,-2)
                t4 <- word(i,-1,-1)     
        }
        
        if(c==2){
                t2 <- word(i,-3,-3)
                t3 <- word(i,-2,-2)
                t4 <- word(i,-1,-1)
        }
        
        if(c>=3){
                t1 <- word(i,-4,-4)
                t2 <- word(i,-3,-3)
                t3 <- word(i,-2,-2)
                t4 <- word(i,-1,-1)
        }
        
        #one word input
        if(t1 == "" & t2 == "" & t3 == "") {
                pred1 <- check2Gram(t4)
                results <- rbind(pred1)
                
        }
        #two word input
        else if(t1 == "" & t2 == "") 
                {
                pred2 <- check3Gram(t3,t4)
                pred1 <- check2Gram(t4)
                results <- rbind(pred1,pred2)
        }
        #three word input
        
        else if(t1 == "") 
        {
                pred3 <- check4Gram(t2,t3,t4)
                pred2 <- check3Gram(t3,t4)
                pred1 <- check2Gram(t4)
                results <- rbind(pred1,pred2)
        }
        
        else
                {

                pred4 <- check5Gram(t1,t2,t3,t4)
                pred3 <- check4Gram(t2,t3,t4)
                pred2 <- check3Gram(t3,t4)
                pred1 <- check2Gram(t4)
                results <- rbind(pred1,pred2,pred3,pred4)
        }
        results <- arrange(results, desc(Prob))
        return(results)

}

nextWord("pint of ice cold")



###########