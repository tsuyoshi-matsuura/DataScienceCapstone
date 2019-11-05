---
title: "Milestone Report 1"
output:
  html_document:
    keep_md: yes
  pdf_document: default
editor_options:
  chunk_output_type: inline
---



## Summary

The objective of this project is to develop a text prediction application. For this
purpose three data files are made available to train the application.

This report gives the current status of the project. In particular, the status
of data reading, data cleaning and data exploration will be discussed.

## Data input

Three large data files containing blog, news and twitter data are available
to train the text prediction application. Text mining was performed on these
data sets to develop a feel for the data and to start developing ideas on how
to build the text prediction application.

For the text mining the infrastructure offered by 'tidytext' was used (an
introduction can be found on this [website](https://www.tidytextmining.com/)). A
nice feature of 'tidytext' is that it is very well integrated with the 'tidyverse'
packages.



To read the three data files a function 'readFileClean' was written that can read
in the files and write a random fraction of the file into a 'tibble'. For this
report we used 10% of the data sets.


```r
# Read 10% of the blogs, news and twitter data
blogs <- readFileClean("en_US/en_US.blogs.txt", fraction=0.1, seed=124)
news <- readFileClean("en_US/en_US.news.txt", fraction=0.1, seed=124)
twitter <- readFileClean("en_US/en_US.twitter.txt", fraction=0.1, seed=124)
```
The three data sets were combined into a 'corpus' for further cleaning and
analysis.

```r
corpus <- bind_rows(list(blogs=blogs,news=news,twitter=twitter),.id="source")
```

The content of the corpus is shown below. It contains the source, the line number and the line text.

```
## # A tibble: 5 x 3
##   source  line text                                                        
##   <chr>  <int> <chr>                                                       
## 1 blogs      1 From there, newspapers sometimes complicate matters by refe~
## 2 blogs      2 But just as babies are no longer content to wobble when the~
## 3 blogs      3 Unfortunately, I was having too much fun to take pictures.  
## 4 blogs      4 When you understand the MESSAGE OF GRACE, you walk in the K~
## 5 blogs      5 More retirement savings through CPP looks, to those who are~
```
In the table below the number of lines and words in the corpus is summarized
for each source.

```
## # A tibble: 3 x 3
##   source   lines   words
##   <chr>    <int>   <int>
## 1 blogs    89928 3761615
## 2 news    101024 3458077
## 3 twitter 236014 3008376
```

## Data extraction and cleaning

### Tokenization

For the text prediction application I intend to use unigram (single word), 
bigram (two consecutive words) and trigram (three consecutive words)
frequencies in the data sets. Therefore, some analysis of these frequencies are
presented here.

To obtain unigram, bigram and trigram data from the corpus the 'tidytext' function
'unnest_tokens' is used. By default this function casts all text to lower case.

### Cleaning

After tokenization the following cleaning steps were taken

* Removal of 'stop words' using the 'snowball' lexicon. This
removes frequently occuring words like I, to, the, ....
    
* Removal of 'words' containing characters other than a-z, A-Z and ' (apostrophe).
    
* Removal of profanity words. For this process a list with profanities from Google was used.

Stemming of the words is also a possibility and easy to implement, but this is
to be investigated at a later stage.

### Implementation

Below the implementation and cleaning is demonstrated for unigrams. For bigrams
and trigrams the process is very similar.


```r
# Tokenize into unigrams using unnest_tokens, followed by cleaning
words <- corpus %>% unnest_tokens(word,text,token="words") %>%
    # Remove "snowball" stopwords
    filter(!word %in% stop_words$word[stop_words$lexicon=="snowball"])  %>%
    # Remove words with non letters
    filter(!grepl("[^a-zA-Z']",word)) %>%
    # Remove profanity words
    filter(!word %in% profanity)
```

## Data analysis

### Unigrams

As a first step the term frequency for each unigram was calculated. The term freqency
is the number of times an unigram occurs in a source divided by the total number of
unigrams in the source. The higher the term frequency the more often an unigram is used.

Below is a table with statistics for unigrams in the three data sets. In the table
'source' is the source of the word, 'word' is the unigram, 'n' is the number of
times an unigram occurs in the source, 'rank' is the ranking of the unigran in the
source and 'tf' is the term frequency. The table is sorted on 'n'.


```
## # A tibble: 255,587 x 6
## # Groups:   source [3]
##    source  word      n   total  rank      tf
##    <chr>   <chr> <int>   <int> <int>   <dbl>
##  1 news    said  25036 1940949     1 0.0129 
##  2 twitter just  15040 1681833     1 0.00894
##  3 blogs   one   12634 1898237     1 0.00666
##  4 twitter like  12073 1681833     2 0.00718
##  5 blogs   will  11574 1898237     2 0.00610
##  6 twitter get   11317 1681833     3 0.00673
##  7 news    will  10760 1940949     2 0.00554
##  8 twitter love  10558 1681833     4 0.00628
##  9 twitter good  10177 1681833     5 0.00605
## 10 blogs   can   10120 1898237     3 0.00533
## # ... with 255,577 more rows
```

In the figure below unigram frequencies in the three data sets are
shown.
![](MileStoneReport1_files/figure-html/uniPlot-1.png)<!-- -->

### Bigrams and trigrams

In a similar way to the unigrams, bigrams and trigrams can be obtained
and analyzed. Below the bi- and trigram distributions are shown for the three
data sets.



![](MileStoneReport1_files/figure-html/biPlot-1.png)<!-- -->

![](MileStoneReport1_files/figure-html/triPlot-1.png)<!-- -->

## Observations and thoughts

Here are some observations and thoughts from text mining through the three 
data sets:

* The data sets contain many 'words' with non alphabetic characters. I assumed that for
setting up the text predection these would be not useful, therefore I removed
all words with non alphabetic characters from the cleaning data sets.

* A very large fraction of the unigrams are meaningless words, like 'aaaaaaaarrrrrghhhhhhh'.
Although there are many meaningless words, they all have a very low frequency. This means
that they could be easily removed by deleting all unigrams which only
occur once in the data set. This process would also remove rare, valid words, but
this is likely something one could live with.

* At the moment I am assuming that it is sufficient to go up to trigrams for
text prediction. This is something that needs testing.

* In this report I did not apply word stemming as part of the cleaning process.
I need to investigate whether word stemming will be required. It will bring
down the number on ngrams.



