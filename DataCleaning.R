library(tidyverse)
library(tidytext)
library(SnowballC)

# Function to read a random fraction of the input file
# All numbers are replaced by blanks
readFile = function(filepath,fraction,seed) {
    # Open as "rb" to avoid ^Z problem
    con = file(filepath, "rb")
    lines <- readLines(con,encoding = "UTF-8")
    close(con)
    set.seed(seed)
    lines <- sample(lines,length(lines)*fraction)
    lines <- gsub("\\b[0-9][0-9.,eE+-]*\\b","",lines)
    output <- tibble(line=1:length(lines),text=lines)
    output
}

# Read 10% of the blogs, news and twitter data
blogs <- readFile("en_US/en_US.blogs.txt",0.1,124)
news <- readFile("en_US/en_US.news.txt",0.1,124)
twitter <- readFile("en_US/en_US.twitter.txt",0.1,124)

# Combine the above into a single corpus
# Mark the source of each of the data
corpus <- bind_rows(list(blogs=blogs,news=news,twitter=twitter),.id="source")

# Read a file with profanity words
profanity <- readLines("en_US/profanity.txt")
profanity <- tibble(word=profanity)

# Tokenize into unigrame using unnest_token
words <- corpus %>% unnest_tokens(word,text,token="words") # 10054741
# Remove profanities and "snowball" stopwords
words <- words %>% anti_join(profanity) # 10042330 
words <- words %>% anti_join(stop_words[stop_words$lexicon=="snowball",]) # 5597926
# Count number of words
words %>% count(word,sort=TRUE)

# Determine term frequency for each source
source_words <-  words %>% count(source,word,sort=TRUE)
total_words <- source_words %>% group_by(source) %>% 
                summarize(total=sum(n))
source_words <- left_join(source_words,total_words)
source_words <- source_words %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), tf = n/total)
source_words

# There are manu words that only occure very rarely
source_words %>% ggplot(aes(tf,fill=source))+
    geom_histogram(show.legend=FALSE)+
    xlim(NA,0.0001) +
    facet_wrap(~source)

pd <-  source_words %>% 
    group_by(source) %>% 
    top_n(10, tf) %>% 
    ungroup() %>%
    arrange(source,desc(tf)) %>%
    mutate(order=row_number())

pd %>% ggplot(aes(x=-order, y=tf, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol=1,scales="free_y") +
    labs(x = NULL, y = "term frequency") +
    scale_x_continuous(
        breaks = -pd$order,
        labels = pd$word,
        expand = c(0,0)
    ) +
    coord_flip()


# Impact of stemming
tokstem <- tokens %>% mutate(word = wordStem(word))

source_words_Stem <-  tokstem %>% count(source,word,sort=TRUE)
total_words_Stem <- source_words_Stem %>% group_by(source) %>% 
    summarize(total=sum(n))

source_words_Stem <- left_join(source_words_Stem,total_words_Stem)

source_words_Stem

# unique words in sample
length(unique(source_words$word)) # 183224
length(unique(source_words_Stem$word)) # 147052

bigrams <- corpus %>% unnest_tokens(bigram,text,token="ngrams", n=2)

bigrams %>% count(bigram,sort=TRUE) # 2,642,460

bigrams_sep <- bigrams %>% separate(bigram, c("word1","word2"),sep=" ")

# Remove stopwords and profanity
bigram_clean <- bigrams_sep %>%
    filter(!word1 %in% stop_words[stop_words$lexicon=="snowball",1]) %>%
    filter(!word2 %in% stop_words[stop_words$lexicon=="snowball",1]) %>%
    filter(!word1 %in% profanity$word) %>%
    filter(!word2 %in% profanity$word)

bigrams_counts <- bigram_clean %>% count(word1,word2,sort=TRUE)
bigrams_counts

bigram_united <- bigram_clean %>% unite(bigram, word1, word2, sep=" ")
bigram_united

bigram_united %>% count(bigram,sort=TRUE)

source_bigrams <-  bigram_united %>% count(source,bigram,sort=TRUE)
total_bigrams <- source_bigrams %>% group_by(source) %>% 
    summarize(total=sum(n))

source_bigramss <- left_join(source_bigrams,total_bigrams)

source_bigrams

freq_by_rank_bigrams <- source_bigramss %>% 
    group_by(source) %>% 
    mutate(rank = row_number(), 
           tf = n/total)

freq_by_rank_bigrams

pd_bigram <- freq_by_rank_bigrams %>% 
    group_by(source) %>% 
    top_n(10, tf) %>% 
    ungroup() %>%
    arrange(source,desc(tf)) %>%
    mutate(order=row_number())

ggplot(pd_bigram,aes(x=-order, y=tf, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, ncol=1,scales="free_y") +
    labs(x = NULL, y = "term frequency") +
    scale_x_continuous(
        breaks = -pd_bigram$order,
        labels = pd_bigram$bigram,
        expand = c(0,0)
    ) +
    coord_flip()