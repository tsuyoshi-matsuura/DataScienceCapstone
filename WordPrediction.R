library(tidyverse)
library(tidytext)

# Read a file with profanity words
profanity <- readLines("profanity.txt")
profanity <- tibble(word=profanity)

readFile <-  function(filepath,fraction,seed) {
    # Open as "rb" to avoid ^Z problem
    con = file(filepath, "rb")
    lines <- readLines(con,encoding = "UTF-8")
    close(con)
    set.seed(seed)
    lines <- sample(lines,length(lines)*fraction)
    output <- tibble(line=1:length(lines),text=lines)
    output
}

# Read 10% of the blogs, news and twitter data
blogs <- readFile("en_US/en_US.blogs.txt",0.05,124)
news <- readFile("en_US/en_US.news.txt",0.05,124)
twitter <- readFile("en_US/en_US.twitter.txt",0.05,124)

# Combine the above into a single corpus
# Mark the source of each of the data
corpus <- bind_rows(list(blogs=blogs,news=news,twitter=twitter),.id="source")

# Tokenize into unigrams using unnest_token
words <- corpus %>% unnest_tokens(word,text,token="words") %>%
    # Remove "snowball" stopwords
    filter(!word %in% stop_words$word[stop_words$lexicon=="snowball"])  %>%
    # Remove words with non letters
    filter(!grepl("[^a-zA-Z']",word)) %>%
    # Remove profanity words
    filter(!word %in% profanity)

corpusClean <- words %>% group_by(source,line) %>%
    summarize(text = str_c(word, collapse = " ")) %>% ungroup()

# Determine term frequency for each source
words <-  words %>% count(word,sort=TRUE)
total_words <- as.numeric(words %>% summarize(total=sum(n)))
words

n1 <- sum((words %>% filter(n==1))$n)
n2 <- sum((words %>% filter(n==2))$n)

discount <- n1/(n1+2*n2)

# Tokenize into bigrams
bigrams <- corpus %>% unnest_tokens(bigram,text,token="ngrams", n=2) %>%
    filter(!is.na(bigram))

# First separate bigrams into two words,
# then filter out non letter words and profanities
# and then unite the separate words into bigrams
bigrams <- bigrams %>% separate(bigram, c("word1","word2"),sep=" ") %>%
    # Remove stopwords
    filter(!word1 %in% stop_words$word[stop_words$lexicon=="snowball"],
           !word2 %in% stop_words$word[stop_words$lexicon=="snowball"]) %>%
    # Remove words with non letters
    filter(!grepl("[^a-zA-Z']",word1),
           !grepl("[^a-zA-Z']",word2)) %>%
    # Remove profanity words
    filter(!word1 %in% profanity,
           !word2 %in% profanity) %>%
    unite(bigram, word1, word2, sep=" ")

# Determine term frequency for each source
bigrams <-  bigrams %>% count(bigram,sort=TRUE)
bigrams

bigrams <- bigrams %>% separate(bigram, c("word1","word2"),sep=" ")

w1Sw2 <- bigrams %>% group_by(word1) %>% summarize(w1Sw2=sum(n))
w2_w1 <- bigrams %>% group_by(word1) %>% count(word2) %>% summarize(w2_w1=sum(n))
w2_w1$w2_w1 <- discount * (w2_w1$w2_w1/w1Sw2$w1Sw2)
w1_w2 <- bigrams %>% group_by(word2) %>% count(word1) %>% summarize(w1_w2=sum(n))
num_uni_big <- sum(w1_w2$w1_w2)
w1_w2 <- w1_w2 %>% mutate(w1_w2=(w1_w2-discount)/num_uni_big)

bigrams <- left_join(bigrams,w2_w1)
words <- left_join(words,w1_w2,by=c("word"="word2"))

# Tokenize into trigrams
trigrams <- corpus %>% unnest_tokens(trigram,text,token="ngrams", n=3) %>%
    filter(!is.na(trigram))

# First separate trigrams into three words, then filter out stopwords,
# non letter words and profanities
# then perform stemming
# and then unite the separate words into trigrams
trigrams <- trigrams %>% separate(trigram, c("word1","word2","word3"),sep=" ") %>%
    # Remove stopwords
    filter(!word1 %in% stop_words$word[stop_words$lexicon=="snowball"],
           !word2 %in% stop_words$word[stop_words$lexicon=="snowball"],
           !word3 %in% stop_words$word[stop_words$lexicon=="snowball"]) %>%
    # Remove words with non letters
    filter(!grepl("[^a-zA-Z']",word1),
           !grepl("[^a-zA-Z']",word2),
           !grepl("[^a-zA-Z']",word3)) %>%
    # Remove profanity words
    filter(!word1 %in% profanity,
           !word2 %in% profanity,
           !word3 %in% profanity) %>%
    unite(trigram, word1, word2, word3, sep=" ")

# Determine term frequency for each source
trigrams <-  trigrams %>% count(trigram,sort=TRUE)

trigrams

trigrams <- trigrams %>% separate(trigram, c("word1","word2","word3"),sep=" ") %>%
    unite(w1w2, word1, word2, sep=" ")

w1w2Sw3 <- trigrams %>% group_by(w1w2) %>% summarize(w1w2Sw3=sum(n))
w3_w1w2 <- trigrams %>% group_by(w1w2) %>% count(word3) %>% summarize(w3_w1w2=sum(n))
w3_w1w2$w3_w1w2 <- discount * (w3_w1w2$w3_w1w2/w1w2Sw3$w1w2Sw3)

trigrams <- left_join(trigrams,w1w2Sw3)
trigrams <- left_join(trigrams,w3_w1w2)

trigrams <- trigrams %>% separate(w1w2, c("word1","word2"),sep=" ") %>%
    unite(w2w3, word2, word3, sep=" ")

w1_w2w3 <- trigrams %>% group_by(w2w3) %>% count(word1) %>% summarize(w1_w2w3=sum(n))
w1_w2Sw3 <- w1_w2w3 %>% separate(w2w3, c("word2", "word3"), sep=" ") %>%
    group_by(word2) %>% summarize(w1_w2Sw3=sum(w1_w2w3))

bigrams <- left_join(bigrams,w1_w2Sw3,by=c("word1"="word2"))
bigrams <- bigrams %>% unite(w1w2, word1, word2, sep=" ")
bigrams <- left_join(bigrams,w1_w2w3,by=c("w1w2"="w2w3"))



words[is.na(words$w1_w2),"w1_w2"]=0
words <-  words %>% mutate( prob=w1_w2+(discount/total_words) )
words <- words %>% select(word,prob)
bigrams <- bigrams %>% separate(w1w2, c("word1","word2"),sep=" ")
bigrams <- left_join(bigrams,words,by=c("word2"="word"))
bigrams[is.na(bigrams$w1_w2w3),"w1_w2w3"]=0
bigrams[is.na(bigrams$w1_w2Sw3),"w1_w2Sw3"]=1
bigrams[bigrams$w1_w2w3==0,"w1_w2w3"]=discount
bigrams <- bigrams %>% mutate( prob = (w1_w2w3-discount)/w1_w2Sw3+w2_w1*prob )
bigrams <- bigrams %>% select(word1, word2, prob)

bigrams <- bigrams %>% unite(w2w3,word1,word2,sep=" ")
trigrams <-  left_join(trigrams,bigrams)
trigrams <- trigrams %>% mutate( prob=(n-discount)/w1w2Sw3 + w3_w1w2*prob )
trigrams <-  trigrams %>% separate(w2w3, c("word2", "word3"), sep=" ") %>%
    unite(w1w2, word1, word2, sep=" ")
trigrams <- trigrams %>% select(w1w2, word3, prob)

bigrams <- bigrams %>% separate(w2w3, c("word1","word2"),sep=" ")

trigrams <- trigrams %>% arrange(w1w2)

words <- words %>% arrange(desc(prob))


Twords <- function(w1, w2, n=5) {
    
    match <- paste(w1,w2,sep=" ")
    
    Tlist <- trigrams %>% filter(w1w2 == match) %>% arrange(desc(prob)) %>%
        select(word3)
    
    if ( nrow(Tlist) == 0 )
        return( Bwords(w2, n) )
    
    if ( nrow(Tlist) >= n)
        return( pull(Tlist[1:n,]) )
    
    Blist <- Bwords(w2, n)[1:(n - nrow(Tlist))]
    return( c(pull(Tlist), Blist) )
}

# function to return highly probable previous word given a word
Bwords <- function(w1, n = 5) {
    
    Blist <- bigrams %>% filter(word1==as.character(w1)) %>% arrange(desc(prob)) %>%
        select(word2)
    
    if ( nrow(Blist)==0 )
        return( Uwords(n) )
    if ( nrow(Blist) >= n )
        return( pull(Blist[1:n,]) )

    Ulist <- Uwords(n)[1:(n - nrow(Blist))]
    return( c(pull(Blist),Ulist) )
}

# function to return random words from unigrams
Uwords <- function(n = 5) {  
    return( sample( pull(words[1:50,"word"]), size = n ) )
}

PredictWord <- function(text){
    
    input <- tibble(text=text)
    words <- input %>% unnest_tokens(word,text,token="words") %>%
        # Remove "snowball" stopwords
        filter(!word %in% stop_words$word[stop_words$lexicon=="snowball"])  %>%
        # Remove words with non letters
        filter(!grepl("[^a-zA-Z']",word)) %>%
        # Remove profanity words
        filter(!word %in% profanity)
    nw <- nrow(words)
    if (nw==0) {
        return( Uwords())
    } else if (nw == 1){
        word <- words[1,"word"]
        return( Bwords() )
    } else {
        w1 <- words[nw-1,"word"]
        w2 <- words[nw,"word"]
        return( Twords(w1,w2))
    }
}