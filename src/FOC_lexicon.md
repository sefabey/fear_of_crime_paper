---
title: "Lexicon of Crime and Police Words"
author: "Sefa Ozalp"
date: "26/07/2018"
output:
      html_document:
        keep_md: true
---

```r
knitr::opts_chunk$set(warning = F)
```




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
## ✔ readr   1.1.1     ✔ forcats 0.3.0
```

```
## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(rhymer)
# install.packages('rhymer')
```

# Lexicon Paragraph
We compiled a list of 44 seeding words which includes words used when expressing fear of crime, different crime types and British slangs referring to police and criminal activity (find this list online [https://github.com/sefabey/fear_of_crime_paper/blob/master/data/FOC_seed_words_002.csv]). The seed list was used to create the lexicon by querying the Datamuse API -a word-finding engine which allows for querying rhyming words, similar spellings and semantically similar or contextually related words using multiple contraints such as synonyms, perfect and approximate rhymes, homophones, frequent followers, direct holonyms. Using the rhymer package (Landesberg 2017) in R, we queried Datamuse API for the first 100 words that 'means like' -i.e. words or sequence of words that are conceptually, semantically and lexically related to- the words in the seed list. After removing duplicates and manually removing clearly out of context results (such as query:coppers, Datamuse result:atomic number 29), we collated a lexicon consisting of 2538 words (find this file online [https://github.com/sefabey/fear_of_crime_paper/blob/master/data/FOC_lexicon_002_manual_edited.csv]). 

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


# write_csv(crime_fear_lexicon,"../data/FOC_lexicon_001.csv")

# 
```
Not memoising above because no need as wrote to csv file once and using that.


Below removing words to remove identified manually. Again wrote once and commented oit write function.

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
# write_csv(lexicon_filtered, "../data/FOC_lexicon_001_manual_edited.csv")
```

## Take 2

New comments from the PI, need to do the following.

1) Remove fear related words from the seed list and lexicon
2) Revisit the final version of the lexicon and remove irrelevant words (such as drugs ~ prescription, charlatan)


### 2.1. Removing fear related words from the seed list and then from the lexicon.

```r
read_csv("../data/FOC_seed_words.csv") %>% 
    filter(context %in% c("fear")) # print fear related words
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

```
## # A tibble: 6 x 4
##      id words   context explanation                                       
##   <int> <chr>   <chr>   <chr>                                             
## 1     1 afraid  fear    http://criminology.oxfordre.com/view/10.1093/acre…
## 2     2 alone   fear    http://criminology.oxfordre.com/view/10.1093/acre…
## 3     4 avoid   fear    http://criminology.oxfordre.com/view/10.1093/acre…
## 4    15 fear    fear    http://criminology.oxfordre.com/view/10.1093/acre…
## 5    37 scary   fear    http://criminology.oxfordre.com/view/10.1093/acre…
## 6    49 worried fear    https://www.met.police.uk/sd/stats-and-data/met/c…
```

```r
seed_list_002 <- read_csv("../data/FOC_seed_words.csv") %>% 
    filter(!context %in% c("fear")) %>% # glad that i added context var previously
    filter(!words=="shank") %>% # not that related
    mutate(id=seq.int(nrow(.)))
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
# write_csv(seed_list_002, "../data/FOC_seed_words_002.csv")
```


```r
lexicon_2 <- read_csv("../data/FOC_lexicon_001_manual_edited.csv") %>% 
    filter(!query_word %in% c("afraid","alone", "avoid", "fear", "scary", "worried")) %>% 
    select(-id) %>% 
    left_join(seed_list_002, by = c("query_word"="words")) %>% 
    select(id, everything(),-context, -explanation) # decrease to 
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
# write_csv(lexicon_2, "../data/FOC_lexicon_002.csv")
```

### 2.1. Revisit and remove irrelevant words

This is going to be done manually and outside R. 

Removing manually identified words


```r
lexicon_filtered_02 <- read_csv("../data/FOC_lexicon_002_manual.csv") %>% 
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
lexicon_filtered_02
```

```
## # A tibble: 2,538 x 9
##       id query_word word           score tag1  tag2  tag3  tag4  remove
##    <int> <chr>      <chr>          <int> <chr> <chr> <chr> <chr>  <int>
##  1     1 assault    attack         93835 syn   n     <NA>  <NA>       0
##  2     1 assault    sexual assault 93573 syn   n     <NA>  <NA>       0
##  3     1 assault    rape           92281 syn   n     <NA>  <NA>       0
##  4     1 assault    battery        85324 syn   n     <NA>  <NA>       0
##  5     1 assault    violation      85044 syn   n     <NA>  <NA>       0
##  6     1 assault    offensive      84266 syn   n     <NA>  <NA>       0
##  7     1 assault    ravishment     80402 syn   n     <NA>  <NA>       0
##  8     1 assault    assail         79614 syn   v     <NA>  <NA>       0
##  9     1 assault    round          79435 syn   n     <NA>  <NA>       0
## 10     1 assault    snipe          79399 syn   n     <NA>  <NA>       0
## # ... with 2,528 more rows
```

```r
write_csv(lexicon_filtered_02, "../data/FOC_lexicon_002_manual_edited.csv")
```


As words in English can take many forms, suffixes and prefixes, I'll add lemmas and stems of words in seperate columns. 


```r
lexicon_filtered_02 <- lexicon_filtered_02 %>% 
    mutate(lemmas=textstem::lemmatize_strings(word)) %>% 
    mutate(stems= textstem::stem_strings(word)) %>% 
    select(everything(),-remove, remove)

lexicon_filtered_02
```

```
## # A tibble: 2,538 x 11
##       id query_word word     score tag1  tag2  tag3  tag4  lemmas  stems  
##    <int> <chr>      <chr>    <int> <chr> <chr> <chr> <chr> <chr>   <chr>  
##  1     1 assault    attack   93835 syn   n     <NA>  <NA>  attack  attack 
##  2     1 assault    sexual … 93573 syn   n     <NA>  <NA>  sexual… sexual…
##  3     1 assault    rape     92281 syn   n     <NA>  <NA>  rape    rape   
##  4     1 assault    battery  85324 syn   n     <NA>  <NA>  battery batteri
##  5     1 assault    violati… 85044 syn   n     <NA>  <NA>  violat… violat 
##  6     1 assault    offensi… 84266 syn   n     <NA>  <NA>  offens… offens 
##  7     1 assault    ravishm… 80402 syn   n     <NA>  <NA>  ravish… ravish 
##  8     1 assault    assail   79614 syn   v     <NA>  <NA>  assail  assail 
##  9     1 assault    round    79435 syn   n     <NA>  <NA>  round   round  
## 10     1 assault    snipe    79399 syn   n     <NA>  <NA>  snipe   snipe  
## # ... with 2,528 more rows, and 1 more variable: remove <int>
```

```r
write_csv(lexicon_filtered_02, "../data/FOC_lexicon_002_manual_edited.csv")
```

