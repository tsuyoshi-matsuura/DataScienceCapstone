library(tidyverse)
library(tidytext)
library(textclean)

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
    # Cast to lower case
    lines <- tolower(lines)
    # Replace contrations with their multi-word form, e,g I'll -> I will
    lines <- replace_contraction(lines)
    # Remove profanity
    lines <- mgsub(lines,profanity,"")
    # Remove all non letters and non spaces
    lines <- gsub("[^a-zA-Z ]","",lines)
    output <- tibble(line=1:length(lines),text=lines)
    output
}

# Read 10% of the blogs, news and twitter data
blogs <- readFileClean("en_US/en_US.blogs.txt",0.1,124)
news <- readFileClean("en_US/en_US.news.txt",0.1,124)
twitter <- readFileClean("en_US/en_US.twitter.txt",0.1,124)

# Combine the above into a single corpus
# Mark the source of each of the data
corpus <- bind_rows(list(blogs=blogs,news=news,twitter=twitter),.id="source")

# Tokenize into unigrams using unnest_token
words <- corpus %>% unnest_tokens(word,text,token="words") # 10090397
# Remove "snowball" stopwords
words <- words %>% anti_join(stop_words[stop_words$lexicon=="snowball",]) # 5555541 

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

# Visualize the top ten words for each source
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

# unique words in sample
length(unique(source_words$word)) #  214980

# Tokenize into bigrams
bigrams <- corpus %>% unnest_tokens(bigram,text,token="ngrams", n=2)

# Remove stopwords
# First separate bigrams into two words, then filter out stopwords
# and then unite the separate words into bigrams
bigrams <- bigrams %>% separate(bigram, c("word1","word2"),sep=" ") %>%
    filter(!word1 %in% stop_words[stop_words$lexicon=="snowball",1]) %>%
    filter(!word2 %in% stop_words[stop_words$lexicon=="snowball",1]) %>%
    unite(bigram, word1, word2, sep=" ")

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
    xlim(NA,0.0005) +
    scale_y_log10() +
    facet_wrap(~source)

# Visualize the top ten bigramss for each source
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

# Tokenize into trigrams
trigrams <- corpus %>% unnest_tokens(trigram,text,token="ngrams", n=3) %>%
    filter(is.na(trigram))

# Remove stopwords
# First separate trigrams into three words, then filter out stopwords
# and then unite the separate words into trigrams
trigrams <- trigrams %>% separate(trigram, c("word1","word2","word3"),sep=" ") %>%
    filter(!word1 %in% stop_words[stop_words$lexicon=="snowball",1]) %>%
    filter(!word2 %in% stop_words[stop_words$lexicon=="snowball",1]) %>%
    filter(!word3 %in% stop_words[stop_words$lexicon=="snowball",1]) %>%
    unite(trigram, word1, word2, word3, sep=" ")

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

# Visualize the top ten trigrams for each source
pd_trigrams <- source_trigrams %>% 
    group_by(source) %>% 
    top_n(20, tf) %>% 
    ungroup() %>%
    arrange(source,desc(tf)) %>%
    mutate(order=row_number())

pd_trigrams %>% ggplot(aes(x=-order, y=tf, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol=3,scales="free_y") +
    labs(x = NULL, y = "term frequency") +
    scale_x_continuous(
        breaks = -pd_trigrams$order,
        labels = pd_trigrams$trigram,
        expand = c(0,0)
    ) +
    coord_flip()