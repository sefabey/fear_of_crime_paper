---
title: "London gang slangs on Twitter"
author: "Sefa Ozalp"
date: "30/07/2018"
output:
      html_document:
        keep_md: true
---



# Intro

A quick markdown file to search London gang slang on Twitter. 


Rationale: 


Involves:
    1. Data scraping from multiple sources (one html one pdf)
    2. Data wrangling (duh)
    3. Querying from Twitter search API
    4. Memoise API call

Sources:    https://bura.brunel.ac.uk/bitstream/2438/14817/1/FulltextThesis.pdf

https://www.shinobilifeonline.com/index.php?topic=2973.0
    
# 1. Data Scraping


First, scaping data from here: https://www.shinobilifeonline.com/index.php?topic=2973.0


```r
html01 <- "https://www.shinobilifeonline.com/index.php?topic=2973.0"


slangs <- rvest::html(html01) %>% 
    rvest::html_nodes( xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "inner", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "bbc_size", " " ))]') %>% 
    rvest::html_text() %>% 
    as.tibble() %>% 
    mutate(slang_term=value) %>% 
    mutate(letter_count=str_count(slang_term)) %>% 
    filter(letter_count>1) %>% 
    # mutate(double_words= case_when( str_detect(slang_term, "/") ~ "yes",TRUE ~ "no")) %>% #no need to do this, separate_rows works as expected
    separate_rows(slang_term, sep = "/") %>% 
    distinct(slang_term,.keep_all = T) %>% 
    filter(!row_number()==1) %>% #drop the first row which was a header 
    mutate(slang_term_lower= str_to_lower(slang_term)) %>% 
    select(slang_term,slang_term_lower)
```



```r
slangs <- slangs %>% 
    mutate(slang_term_query= paste0('\"',slangs$slang_term_lower,'\"'))

slang_tweets <- purrr::map_df(.x = slangs$slang_term_query, .f =  rtweet::search_tweets, 
                              n = 50, include_rts=F)
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```
## Searching for tweets...
```

```
## Finished collecting tweets!
```

```r
rate_limit() %>% 
    arrange(reset)
```

```
## # A tibble: 140 x 7
##    query    limit remaining reset  reset_at            timestamp          
##    <chr>    <int>     <int> <time> <dttm>              <dttm>             
##  1 search/…   180        60 13.91… 2018-08-01 20:31:54 2018-08-01 20:18:00
##  2 lists/l…    15        15 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
##  3 lists/m…    75        75 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
##  4 lists/s…    15        15 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
##  5 lists/m…   900       900 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
##  6 lists/s…    15        15 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
##  7 lists/s…    75        75 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
##  8 lists/o…    15        15 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
##  9 lists/s…   180       180 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
## 10 lists/m…    15        15 15.01… 2018-08-01 20:33:00 2018-08-01 20:18:00
## # ... with 130 more rows, and 1 more variable: app <chr>
```

```r
slang_tweets %>% rtweet::write_as_csv("slang_tweets.csv")
```


read slang tweets

```r
slang_tweets <- rtweet::read_twitter_csv("../data/slang_tweets.csv")
```



