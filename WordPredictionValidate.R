library(tidyverse)
library(tidytext)
library(tictoc)


# Read a file with profanity words
profanity <- readLines("profanity.txt")

unigrams <- readRDS("RDS/unigrams.rds")
bigrams <- readRDS("RDS/bigrams.rds")
trigrams <- readRDS("RDS/trigrams.rds")
quadgrams <- readRDS("RDS/quadgrams.rds")


Qwords <- function(w1, w2, w3, n=5) {
    
    match <- paste(w1,w2,w3,sep=" ")

    Qlist <- quadgrams %>% filter(w123 == match) %>% arrange(desc(prob4)) %>%
        select(w4)
    
    if ( nrow(Qlist) == 0 ){
        return( Twords(w2, w3, n) )
    }
    
    if ( nrow(Qlist) >= n) {
        return( pull(Qlist[1:n,]) )
    }

    Tlist <- Twords(w2, w3, n)[1:(n - nrow(Qlist))]
    return( c(pull(Qlist), Tlist) )
}

Twords <- function(w1, w2, n=5) {
    
    match <- paste(w1,w2,sep=" ")

    Tlist <- trigrams %>% filter(w12 == match) %>% arrange(desc(prob3)) %>%
        select(w3)
    
    if ( nrow(Tlist) == 0 ){
        return( Bwords(w2, n) )
    }
    
    if ( nrow(Tlist) >= n) {
        return( pull(Tlist[1:n,]) )
    }

    Blist <- Bwords(w2, n)[1:(n - nrow(Tlist))]
    return( c(pull(Tlist), Blist) )
}

# function to return highly probable previous word given a word
Bwords <- function(word, n = 5) {

    Blist <- bigrams %>% filter(w1==as.character(word)) %>% arrange(desc(prob2)) %>%
        select(w2)

    if ( nrow(Blist)==0 ) {
        return( Uwords(n) )
    }
    if ( nrow(Blist) >= n ) {
        return( pull(Blist[1:n,]) )
    }

    Ulist <- Uwords(n)[1:(n - nrow(Blist))]
    return( c(pull(Blist),Ulist) )
}

# function to return random words from unigrams
Uwords <- function(n = 5) {

    return( sample( pull(unigrams[1:50,"word"]), size = n ) )
}

PredictWord <- function(text,n=5){
    
    input <- tibble(line =c(1), text=text)
    words <- input %>% unnest_tokens(word,text,token="words") %>%
        # Remove words with non letters
        filter(!grepl("[^a-zA-Z']",word)) %>%
        # Remove profanity words
        filter(!word %in% profanity)
    if (nrow(words)>0) {
        input <- words %>% group_by(line) %>%
            summarize(text = str_c(word, collapse = " "), 
                      text=paste("b0s b0s b0s ",text)) %>% ungroup()
        words <- input %>% unnest_tokens(word,text,token="words")
    } else {
        input <- tibble(line =c(1), text="b0s b0s b0s ")
    }
    words <- input %>% unnest_tokens(word,text,token="words")
    nw <- nrow(words)
    w1 <- words[nw-2,"word"]
    w2 <- words[nw-1,"word"]
    w3 <- words[nw,"word"]
    return( Qwords(w1,w2,w3,n) )
}

CheckWord <- function(text){
    
    input <- tibble(line =c(1), text=text)
    words <- input %>% unnest_tokens(word,text,token="words")
    nw <- nrow(words)
    w1 <- words[nw-3,"word"]
    w2 <- words[nw-2,"word"]
    w3 <- words[nw-1,"word"]
    act <- words[nw,"word"]
    prd <- Qwords(w1,w2,w3,n=1)
    return(c(act==prd))
}

corpusTest <- readRDS("RDS/corpusTest.rds")

words <- corpusTest %>% unnest_tokens(word,text,token="words") %>%
    # Remove words with non letters
    filter(!grepl("[^a-zA-Z']",word)) %>%
    # Remove profanity words
    filter(!word %in% profanity)

corpusClean <- words %>% group_by(source,line) %>%
    summarize(text = str_c(word, collapse = " "), 
              text=paste("b0s b0s b0s ",text)) %>% ungroup()

# Remove e0s from output
unigrams <-  unigrams %>% filter(!word %in% c("e0s"))
unigrams
bigrams <-  bigrams %>% filter(!w2 %in% c("e0s"))
bigrams
trigrams <-  trigrams %>% filter(!w3 %in% c("e0s"))
trigrams
quadgrams <-  quadgrams %>% filter(!w4 %in% c("e0s"))
quadgrams


quadgrams <- quadgrams %>% filter(n4 >= 4)
trigrams <-  trigrams %>% filter(n3>=4)
bigrams <-  bigrams %>% filter(n2>=4)

quadgrams <- quadgrams %>% arrange(w123)
trigrams <- trigrams %>% arrange(w12)
bigrams <- bigrams %>% arrange(w1)

saveRDS(quadgrams,file="quadgrams_min4.rds")
saveRDS(trigrams,file="trigrams_min4.rds")
saveRDS(bigrams,file="bigrams_min4.rds")

sample <- sample(1:nrow(corpusClean),1000)

tic("CheckWord")
test <- corpusClean[sample,] %>% select(text) %>% mutate(out=mapply(CheckWord,text))
toc()
sum(test$out)/nrow(test)


