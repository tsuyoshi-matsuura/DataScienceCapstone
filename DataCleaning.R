library(tidyverse)
library(tidytext)
library(textclean)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Read a file with profanity words
profanity <- readLines("profanity.txt")
profanity <- tibble(word=profanity)

readFileClean <-  function(filepath,fraction,seed) {
    # Open as "rb" to avoid ^Z problem
    con = file(filepath, "rb")
    lines <- readLines(con,encoding = "UTF-8")
    close(con)
    set.seed(seed)
    lines <- sample(lines,length(lines)*fraction)
    # Replace contrations with their multi-word form, e,g I'll -> I will
    #lines <- replace_contraction(lines)
    output <- tibble(line=1:length(lines),text=lines)
    output
}

# Read 10% of the blogs, news and twitter data
blogs <- readFileClean("en_US/en_US.blogs.txt",0.1,124)
news <- readFileClean("en_US/en_US.news.txt",0.1,124)
twitter <- readFileClean("en_US/en_US.twitter.txt",0.1,124)

write.csv(blogs,"blogs.csv",row.names=FALSE)
write.csv(news,"news.csv",row.names=FALSE)
write.csv(twitter,"twitter.csv",row.names=FALSE)

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
    filter(!word %in% profanity) %>%
    # Stem the words
    #mutate(word = wordStem(word)) %>%
    # Remove empty words
    filter(word != "")


# Determine term frequency for each source
source_words <-  words %>% count(source,word,sort=TRUE)
total_words <- source_words %>% group_by(source) %>% 
                summarize(total=sum(n))
source_words <- left_join(source_words,total_words)
source_words <- source_words %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), tf = n/total)
source_words

# There are many words that only occur very rarely
source_words %>% ggplot(aes(tf,fill=source))+
    geom_histogram(show.legend=FALSE)+
    xlim(NA,0.001) +
    scale_y_log10() +
    facet_wrap(~source)

# Visualize the top twenty words for each source
pd_words <-  source_words %>% 
    group_by(source) %>% 
    top_n(20, tf) %>% 
    ungroup() %>%
    arrange(source,desc(tf)) %>%
    mutate(order=row_number())

pd_words %>% ggplot(aes(x=-order, y=tf, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol=3,scales="free_y") +
    labs(x = NULL, y = "term frequency") +
    scale_x_continuous(
        breaks = -pd_words$order,
        labels = pd_words$word,
        expand = c(0,0)
    ) +
    coord_flip()

# Many meaningless words with low frequency
source_words[source_words$n==1,]

# unique words in sample
length(unique(source_words$word)) #  127746

# unique words in sample, if n=1 is left out
length(unique(source_words$word[source_words$n>1])) # 54349

# Tokenize into bigrams
bigrams <- corpus %>% unnest_tokens(bigram,text,token="ngrams", n=2) %>%
    filter(!is.na(bigram)) # 9926761 

# First separate bigrams into two words, then filter out stopwords,
# non letter words and profanities
# then perform stemming
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
    # Stem the words
    #mutate(word1 = wordStem(word1),
    #       word2 = wordStem(word2)) %>%
    # Filter empty words
    filter(word1 != "",
           word2 != "") %>%
    unite(bigram, word1, word2, sep=" ") # 2470965

# Determine term frequency for each source
source_bigrams <-  bigrams %>% count(source,bigram,sort=TRUE)
total_bigrams <- source_bigrams %>% group_by(source) %>% 
    summarize(total=sum(n))
source_bigrams <- left_join(source_bigrams,total_bigrams)
source_bigrams <- source_bigrams %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), tf = n/total)
source_bigrams

# There are many bigrams that only occur very rarely
source_bigrams %>% ggplot(aes(tf,fill=source))+
    geom_histogram(show.legend=FALSE)+
    xlim(NA,0.0001) +
    scale_y_log10() +
    facet_wrap(~source)

# Visualize the top twenty bigramss for each source
pd_bigrams <- source_bigrams %>% 
    group_by(source) %>% 
    top_n(20, tf) %>% 
    ungroup() %>%
    arrange(source,desc(tf)) %>%
    mutate(order=row_number())

pd_bigrams %>% ggplot(aes(x=-order, y=tf, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol=3,scales="free_y") +
    labs(x = NULL, y = "term frequency") +
    scale_x_continuous(
        breaks = -pd_bigrams$order,
        labels = pd_bigrams$bigram,
        expand = c(0,0)
    ) +
    coord_flip()

# Many meaningless words with low frequency
source_bigrams[source_bigrams$n==1,]

# unique bigrams in sample
length(unique(source_bigrams$bigram)) #  1335350

# unique words in sample, if n=1 is left out
length(unique(source_bigrams$bigram[source_bigrams$n>1])) # 205975

# Tokenize into trigrams
trigrams <- corpus %>% unnest_tokens(trigram,text,token="ngrams", n=3) %>%
    filter(!is.na(trigram)) #9501283

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
    # Stem the words
    #mutate(word1 = wordStem(word1),
    #       word2 = wordStem(word2),
    #       word3 = wordStem(word3)) %>%
    # Filter empty words
    filter(word1 != "",
           word2 != "",
           word3 != "") %>%
    unite(trigram, word1, word2, word3, sep=" ") #1077987

# Determine term frequency for each source
source_trigrams <-  trigrams %>% count(source,trigram,sort=TRUE)
total_trigrams <- source_trigrams %>% group_by(source) %>% 
    summarize(total=sum(n))
source_trigrams <- left_join(source_trigrams,total_trigrams)
source_trigrams <- source_trigrams %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), tf = n/total)
source_trigrams

# There are many trigrams that only occur very rarely
source_trigrams %>% ggplot(aes(tf,fill=source))+
    geom_histogram(show.legend=FALSE)+
    xlim(NA,0.0001) +
    scale_y_log10() +
    facet_wrap(~source)

# Visualize the top twenty trigrams for each source
pd_trigrams <- source_trigrams %>% 
    group_by(source) %>% 
    top_n(20, tf) %>% 
    ungroup() %>%
    arrange(source,desc(tf)) %>%
    mutate(order=row_number())

pd_trigrams %>% ggplot(aes(x=-order, y=tf, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol=3,scales="free_y") +
    labs(x = NULL, y = "term frequency", title="Top twenty trigrams") +
    scale_x_continuous(
        breaks = -pd_trigrams$order,
        labels = pd_trigrams$trigram,
        expand = c(0,0)
    ) +
    coord_flip()


stats <- corpus %>% group_by(source) %>% summarize(lines=max(line))
stats2 <- corpus %>% unnest_tokens(word,text,token="words") %>%
    count(source,word) %>% group_by(source) %>%
    summarize(words=sum(n))



wordsT <- corpus %>% unnest_tokens(word,text,token="words") %>%
    # Remove words with non letters
    filter(!grepl("[^a-zA-Z']",word))

source_wordsT <-  wordsT %>% count(source,word,sort=TRUE)
total_wordsT <- source_wordsT %>% group_by(source) %>% 
    summarize(total=sum(n))
source_wordsT <- left_join(source_wordsT,total_wordsT)
source_wordsT <- source_wordsT %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), tf = n/total)
source_wordsT

source_wordsT %>% ggplot(aes(x=rank, y=n, colour=source)) +
    geom_line()+
    scale_x_log10(limits=c(1,10000)) +
    scale_y_log10()