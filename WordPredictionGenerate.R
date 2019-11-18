library(tidyverse)
library(tidytext)

# Read a file with profanity words
profanity <- readLines("profanity.txt")

readFile <-  function(filepath,fraction,seed) {
    # Open as "rb" to avoid ^Z problem
    con = file(filepath, "rb")
    lines <- readLines(con,encoding = "UTF-8")
    close(con)
    
    set.seed(seed)
    train <- sample(1:length(lines),length(lines)*fraction)
    Ltrain <- lines[train]
    Ltest <- lines[-train]
    Ttrain <- tibble(line=1:length(Ltrain),text=Ltrain)
    Ttest <- tibble(line=1:length(Ltest),text=Ltest)
    return(list(Ttrain,Ttest))
}

# Read 10% of the blogs, news and twitter data
blogs <- readFile("en_US/en_US.blogs.txt",0.9,124)
news <- readFile("en_US/en_US.news.txt",0.9,124)
twitter <- readFile("en_US/en_US.twitter.txt",0.9,124)

# Combine the above into a single corpus
# Mark the source of each of the data
corpus <- bind_rows(list(blogs=blogs[[1]],news=news[[1]],twitter=twitter[[1]]),.id="source")
corpusTest <- bind_rows(list(blogs=blogs[[2]],news=news[[2]],twitter=twitter[[2]]),.id="source")

# Clean up
rm(blogs)
rm(news)
rm(twitter)
gc()

##### Building vocabulary #####

# Tokenize into unigrams using unnest_token
words <- corpus %>% unnest_tokens(word,text,token="words") %>%
    # Remove words with non letters
    filter(!grepl("[^a-zA-Z']",word)) %>%
    # Remove profanity words
    filter(!word %in% profanity)

#Clean up
rm(corpus)
gc()

Wcount <-  words %>% count(word,sort=TRUE)
Wtot <- sum(Wcount$n)
Wcount <- Wcount %>% mutate(frac=n/Wtot,Cumfrac=cumsum(frac)) %>%
    select(word,Cumfrac)

cumfrac <- 0.95
Wcount_k <- Wcount %>% filter(Cumfrac<=cumfrac) %>% mutate(replace=word)
Wcount_r <- Wcount %>% filter(Cumfrac>cumfrac) %>% mutate(replace="d0mmy")
Wcount <- rbind(Wcount_k,Wcount_r)

#Clean up
rm(Wcount_k)
rm(Wcount_r)
gc()

words <- left_join(words,Wcount) %>% mutate(word=replace) %>%
    select(source, line, word)

##### Re-build corpus with chosen vocabulary #####

corpusClean <- words %>% group_by(source,line) %>%
    summarize(text = str_c(word, collapse = " "), 
              text=paste("b0s b0s b0s ",text," e0s")) %>% ungroup()
	

##### Unigram data #####

# Tokenize into unigrams
unigrams <- corpusClean %>% unnest_tokens(word,text,token="words")

# Determine term frequency
unigrams <-  unigrams %>% count(word,sort=TRUE, name="n1") %>%
    filter(!word %in% c("d0mmy","b0s"))
unigrams

##### Bigram data #####

# Tokenize into bigrams
bigrams <- corpusClean %>% unnest_tokens(bigram,text,token="ngrams", n=2)

# Determine term frequency
bigrams <-  bigrams %>% count(bigram,sort=TRUE, name="n2") %>%
    separate(bigram, c("w1","w2"),sep=" ") %>%
    filter(!w2 %in% c("d0mmy","b0s"))
bigrams

# Calculate discount factor
n1 <- sum((bigrams %>% filter(n2==1))$n2)
n2 <- sum((bigrams %>% filter(n2==2))$n2)
d2 <- n1/(n1+2*n2)

# Calculate factors for Kneser-Ney
w2_w1 <- bigrams %>% group_by(w1) %>% count(w2) %>% summarize(w2_w1=sum(n))

w1_w2 <- bigrams %>% group_by(w2) %>% count(w1) %>% summarize(w1_w2=sum(n))
w1_wS2 <- sum(w1_w2$w1_w2)
w1_w2 <- w1_w2 %>% mutate(w1_w2=w1_w2/w1_wS2)

# Store results and clean
bigrams <- left_join(bigrams,w2_w1)
unigrams <- left_join(unigrams,w1_w2,by=c("word"="w2"))
rm(w2_w1)
rm(w1_w2)
rm(w1_wS2)
gc()

##### Calculate unigram probabilities #####
unigrams <- unigrams %>% rename(prob1=w1_w2) %>% select(word,n1,prob1) %>% arrange(desc(prob1))

##### Trigram data #####

# Tokenize into trigrams
trigrams <- corpusClean %>% unnest_tokens(trigram,text,token="ngrams", n=3)

    # Determine term frequency
trigrams <-  trigrams %>% count(trigram,sort=TRUE,name="n3") %>%
    separate(trigram, c("w1","w2","w3"),sep=" ") %>%
    filter(!w3 %in% c("d0mmy","b0s"))
trigrams

# Calculate discount factor
n1 <- sum((trigrams %>% filter(n3==1))$n3)
n2 <- sum((trigrams %>% filter(n3==2))$n3)
d3 <- n1/(n1+2*n2)

# Calculate factors for Kneser-Ney (step 1)
trigrams <- trigrams %>% unite(w12, w1, w2, sep=" ")
w3_w12 <- trigrams %>% group_by(w12) %>% count(w3) %>% summarize(w3_w12=sum(n))

# Store results and clean
trigrams <- left_join(trigrams,w3_w12)
rm(w3_w12)
gc()

# Calculate factors for Kneser-Ney (step 2)
trigrams <- trigrams %>% separate(w12, c("w1","w2"),sep=" ") %>%
    unite(w23, w2, w3, sep=" ")
w1_w23 <- trigrams %>% group_by(w23) %>% count(w1) %>% summarize(w1_w23=sum(n))
w1_w2S3 <- w1_w23 %>% separate(w23, c("w2", "w3"), sep=" ") %>%
    group_by(w2) %>% summarize(w1_w2S3=sum(w1_w23))
	
# Store results and clean
bigrams <- left_join(bigrams,w1_w2S3,by=c("w1"="w2"))
bigrams <- bigrams %>% unite(w12, w1, w2, sep=" ")
bigrams <- left_join(bigrams,w1_w23,by=c("w12"="w23"))
rm(w1_w23)
rm(w1_w2S3)
gc()

##### Calculte bigram probabilities #####
bigrams <- bigrams %>% separate(w12, c("w1","w2"),sep=" ")
bigrams <- left_join(bigrams,unigrams,by=c("w2"="word"))
bigrams <- bigrams %>% mutate( prob2 = (w1_w23-d2)/w1_w2S3+d2*w2_w1/w1_w2S3*prob1 )
bigrams <- bigrams %>% select(w1, w2, n2, prob2 )

##### Quadgram data #####

# Tokenize quadgrams
quadgrams <- corpusClean %>% unnest_tokens(quadgram,text,token="ngrams", n=4)

# Determine term frequency
quadgrams <-  quadgrams %>% count(quadgram,sort=TRUE,name="n4") %>%
    separate(quadgram, c("w1","w2","w3", "w4"),sep=" ") %>%
    filter(!w4 %in% c("d0mmy","b0s"))
quadgrams

# Calculate discount factor
n1 <- sum((quadgrams %>% filter(n4==1))$n4)
n2 <- sum((quadgrams %>% filter(n4==2))$n4)
d4 <- n1/(n1+2*n2)

# Calculate factors for Kneser-Ney (step 1)
quadgrams <- quadgrams %>% unite(w123, w1, w2, w3, sep=" ")
w4_w123 <- quadgrams %>% group_by(w123) %>% count(w4) %>% summarize(w4_w123=sum(n))
w123S4 <- quadgrams %>% group_by(w123) %>% summarize(w123S4=sum(n4))

# Store results and clean
quadgrams <- left_join(quadgrams,w4_w123)
quadgrams <- left_join(quadgrams,w123S4)
rm(w4_w123)
rm(w123S4)
gc()

# Calculate factors for Kneser-Ney (step 2)
quadgrams <- quadgrams %>% separate(w123, c("w1","w2","w3"),sep=" ") %>%
    unite(w234, w2, w3, w4, sep=" ")
w1_w234 <- quadgrams %>% group_by(w234) %>% count(w1) %>% summarize(w1_w234=sum(n))
w1_w23S4 <- w1_w234 %>% separate(w234, c("w2", "w3","w4"), sep=" ") %>%
    unite(w23, w2, w3, sep=" ") %>%
    group_by(w23) %>% summarize(w1_w23S4=sum(w1_w234))
	
# Store results and clean
trigrams <-  trigrams %>% separate(w23, c("w2","w3"),sep=" ") %>%
    unite(w12, w1, w2, sep=" ")
trigrams <- left_join(trigrams,w1_w23S4,by=c("w12"="w23"))
trigrams <- trigrams %>% unite(w123, w12, w3, sep=" ")
trigrams <- left_join(trigrams,w1_w234,by=c("w123"="w234"))
rm(w1_w23S4)
rm(w1_w234)
gc()

##### Calculte trigram probabilities #####
trigrams <- trigrams %>% separate(w123, c("w1","w2","w3"),sep=" ") %>%
    unite(w23, w2, w3, sep=" ")
bigrams <- bigrams %>% unite(w12, w1, w2, sep=" ")
trigrams <- left_join(trigrams,bigrams,by=c("w23"="w12"))
trigrams <- trigrams %>% mutate( prob3 = (w1_w234-d3)/w1_w23S4+d3*w3_w12/w1_w23S4*prob2 )
trigrams <- trigrams %>% select(w1, w23, n3, prob3 )


##### Calculate quadgram probabilities #####
trigrams <- trigrams %>% unite(w123, w1, w23, sep=" ")
quadgrams <- left_join(quadgrams,trigrams,by=c("w234"="w123"))
quadgrams <- quadgrams %>% mutate( prob4=(n4-d4)/w123S4 + d4*w4_w123/w123S4*prob3 )
quadgrams <- quadgrams %>% select(w1, w234, n4, prob4 )


##### Prepare probabilities for prediction #####
bigrams <- bigrams %>% separate(w12, c("w1","w2"),sep=" ")
trigrams <- trigrams %>% separate(w123, c("w1","w2","w3"), sep=" ") %>% 
    unite(w12, w1, w2, sep=" ")
quadgrams <- quadgrams %>% separate(w234, c("w2","w3","w4"),sep=" ") %>%
    unite(w123, w1, w2, w3, sep=" ")

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

saveRDS(unigrams,file="unigrams.rds")
saveRDS(bigrams,file="bigrams.rds")
saveRDS(trigrams,file="trigrams.rds")
saveRDS(quadgrams,file="quadgrams.rds")
