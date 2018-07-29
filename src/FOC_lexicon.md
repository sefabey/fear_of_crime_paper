---
title: "Lexicon of Crime and Police Words"
author: "Sefa Ozalp"
date: "26/07/2018"
output:
      html_document:
        keep_md: true
---


```r
library(tidyverse)
```

```
## ── Attaching packages ──────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.0.0     ✔ purrr   0.2.4
## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
## ✔ tidyr   0.8.0     ✔ stringr 1.3.1
## ✔ readr   1.1.1     ✔ forcats 0.3.0
```

```
## ── Conflicts ─────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(rhymer)
```

# Lexicon Paragraph
We compiled a list of 51 seeding words which includes words used when expressing fear of crime, different crime types and British slangs referring to police and criminal activity (find this list online [https://github.com/sefabey/fear_of_crime_paper/blob/master/data/FOC_seed_words.csv]). The seed list was used to create the lexicon by querying the Datamuse API -a word-finding engine which allows for querying rhyming words, similar spellings and semantically similar or contextually related words using multiple contraints such as synonyms, perfect and approximate rhymes, homophones, frequent followers, direct holonyms. Using the rhymer package (Landesberg 2017) in R, we queried Datamuse API for the first 100 words that 'means like' -i.e. words or sequence of words that are conceptually, semantically and lexically related to- the words in the seed list. After removing duplicates and manually removing clearly out of context results (such as query:coppers, Datamuse result:atomic number 29), we collated a lexicon consisting of 3096 words (find this file online [https://github.com/sefabey/fear_of_crime_paper/blob/master/data/FOC_lexicon_001_manual_edited.csv]). 

This lexicon was used to filter the 20m tweets in the dataset... (amir)


```r
seed_list <- read_csv("../data/FOC_seed_words.csv")
```

```
## Parsed with column specification:
## cols(
##   id = col_integer(),
##   words = col_character(),
##   context = col_character(),
##   explanation = col_character()
## )
```

```r
seed_list %>% distinct(words) #51 distinct words
```

```
## # A tibble: 51 x 1
##    words   
##    <chr>   
##  1 afraid  
##  2 alone   
##  3 assault 
##  4 avoid   
##  5 burglary
##  6 CCTV    
##  7 coppers 
##  8 whore   
##  9 crime   
## 10 criminal
## # ... with 41 more rows
```


```r
query_datamuse <- function(x, ...){
    rhymer::get_means_like(word = x, ...)
}

crime_fear_lexicon <- seed_list %>% 
    select(words) %>%
    pull() %>%  
    map_df(query_datamuse, limit=100, .id = "id") %>% #5030 results
    mutate(id=as.integer(id)) %>% 
    distinct(word, .keep_all = T) %>% # drops to 3687
    left_join( select(seed_list, c(id, query_word=words)), by="id") %>% 
    select(id, query_word, everything()) %>% 
    mutate(tags=as.character(tags)) %>% 
    separate(tags, sep = ",", into = c("tag1", "tag2", "tag3", "tag4")) %>% 
    mutate_at(vars(tag1,tag2,tag3,tag4), .funs = function(x){
              str_extract(string=x, pattern =  regex("(?<=\")[[:alnum:]]+(?=\")"))})
```

```
## Warning: Expected 4 pieces. Missing pieces filled with `NA` in 3686
## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
## 20, ...].
```

```r
# write_csv(crime_fear_lexicon,"../data/FOC_lexicon_001.csv")

# read locke data some web api package development lessons 
```


```r
lexicon_filtered <- read_csv("../data/FOC_lexicon_001_manual.csv") %>% 
    filter(remove==0) # rows to be removed was labelled as 1, rest was 0
```

```
## Parsed with column specification:
## cols(
##   id = col_integer(),
##   query_word = col_character(),
##   word = col_character(),
##   score = col_integer(),
##   tag1 = col_character(),
##   tag2 = col_character(),
##   tag3 = col_character(),
##   tag4 = col_character(),
##   remove = col_integer()
## )
```

```r
lexicon_filtered
```

```
## # A tibble: 3,096 x 9
##       id query_word word         score tag1  tag2  tag3  tag4  remove
##    <int> <chr>      <chr>        <int> <chr> <chr> <chr> <chr>  <int>
##  1     1 afraid     scared       50038 syn   adj   <NA>  <NA>       0
##  2     1 afraid     fearful      47652 syn   adj   <NA>  <NA>       0
##  3     1 afraid     frightened   45532 syn   adj   <NA>  <NA>       0
##  4     1 afraid     terrified    44729 syn   adj   <NA>  <NA>       0
##  5     1 afraid     petrified    42958 syn   adj   <NA>  <NA>       0
##  6     1 afraid     intimidated  42315 syn   adj   <NA>  <NA>       0
##  7     1 afraid     apprehensive 41740 syn   adj   <NA>  <NA>       0
##  8     1 afraid     concerned    40893 syn   adj   <NA>  <NA>       0
##  9     1 afraid     alarmed      39542 syn   adj   <NA>  <NA>       0
## 10     1 afraid     cowed        38611 syn   adj   <NA>  <NA>       0
## # ... with 3,086 more rows
```

```r
write_csv(lexicon_filtered, "../data/FOC_lexicon_001_manual_edited.csv")
```
