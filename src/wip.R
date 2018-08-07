library(tidyverse)
library(parallel)
library(ndjson)

input_path <- "../local_folder/json/input/"
output_path <- "../local_folder/json/output/"

files <- list.files(input_path)

read_json <- function(x) {
    data <- ndjson::stream_in(paste0(input_path,x))
    data <- data %>% select()
    readr::write_csv(data, path = paste0(output_path, x,".csv"))}


data <- ndjson::stream_in(paste0(input_path,files[10]))


data %>%  group_by(id_str) %>% 
    summarise(n=n()) %>% 
    arrange(desc(n)) %>% 
    pull(n) %>% 
    table() # 44 duplicate tweet IDs. it should not happen. Possibly an API error.

data %>%  group_by(id_str) %>% 
    summarise(n=n()) %>% 
    arrange(desc(n))

data %>% 
    mutate(rowid= rownames(.)) %>% filter(id_str=="996332115875610625") %>%select(rowid, everything()) %>% select(text)

data %>% 
    mutate(rowid= rownames(.)) %>% filter(is.na(id_str)) %>%select(rowid, everything()) %>% select(text)


data %>%
    distinct(id_str,.keep_all = T) %>% 
    filter(str_detect(string = text, pattern = regex('muslim|islam|refugee|migrant|migrat',ignore_case = T)))


## here fit topic models to text and see why above search is returning 26k tweets rather than 74k
# in other words, whats the remaining tweets talking about

library(topicmodels)
data %>% select(contains("text")) %>% colnames()

data %>% select(text, display_text_range.0, display_text_range.1, extended_tweet.full_text) %>% View()

data %>% 
    distinct(id_str,.keep_all = T) %>% 
    
    mutate(text_real= ifelse(is.na(extended_tweet.full_text), text,extended_tweet.full_text )) %>% 
    select(text, display_text_range.0, display_text_range.1, extended_tweet.full_text, text_real, id_str) %>% 
    # View() %>% 
    select(text_real, id_str)-> tweets_new


library(tm)
myCorpus <- Corpus(VectorSource(tweets_new))  
dtm_tweets <- DocumentTermMatrix(myCorpus)

tweets_lda <- LDA(dtm_tweets, k=20, control = list(seed=1234))

library(tidytext)
tweet_topics <- tidy(tweets_lda, matrix="beta")
tweet_topics

tweet_top_terms <- tweet_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

tweet_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()


# ok, I have observed many arabic tweets in the graph. maybe it makes sense to look at the languages
data %>% 
    group_by(lang) %>% 
    summarise(n=n()) %>% 
    arrange(desc(n))

data %>% filter(lang=="en") %>% 
    distinct(id_str,.keep_all = T) %>%
    filter(str_detect(string = text, pattern = regex('muslim|islam',ignore_case = T))) %>% 
    select(text, display_text_range.0, display_text_range.1, extended_tweet.full_text, id_str, matches("full")) %>%  #%>% 
    mutate(text_real= ifelse(is.na(extended_tweet.full_text), text,extended_tweet.full_text )) %>%
    mutate(text_real= rtweet::plain_tweets(text_real)) %>% 
    select(text_real,id_str) ->tweets_cleaned


myCorpus <- Corpus(VectorSource(tweets_cleaned$text_real))  
myCorpus <- tm_map(myCorpus,removeWords, stopwords("english"))
dtm_tweets <- DocumentTermMatrix(myCorpus)

tweets_lda <- LDA(dtm_tweets, k=10, control = list(seed=1234),  method= "VEM")
tweet_topics <- tidy(tweets_lda, matrix="beta")
tweet_topics

tweet_top_terms <- tweet_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)


tweet_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

#take a look at the graph


tweets_cleaned %>% 
    # add_rownames() %>% 
    unnest_tokens(word,text_real) %>% 
    anti_join(stop_words, copy = T) %>% 
    as_data_frame()->a

a %>% count(word, sort = T) %>% View



data %>%
    filter(lang=="en") %>% 
    distinct(id_str,.keep_all = T) %>%
    filter(str_detect(string = text, pattern = regex('muslim|islam',ignore_case = T))) %>%
    mutate(  
        quoted_status.coordinates.coordinates.0= ifelse(is.null(.$quoted_status.coordinates.coordinates.0), NA, .$quoted_status.coordinates.coordinates.0),
        quoted_status.coordinates.coordinates.1= ifelse(is.null(.$quoted_status.coordinates.coordinates.1), NA, .$quoted_status.coordinates.coordinates.1),
        retweeted_status.coordinates.coordinates.0= ifelse(is.null(.$retweeted_status.coordinates.coordinates.0), NA, .$retweeted_status.coordinates.coordinates.0),
        retweeted_status.coordinates.coordinates.1= ifelse(is.null(.$retweeted_status.coordinates.coordinates.1), NA, .$retweeted_status.coordinates.coordinates.1),
        coordinates.coordinates.0= ifelse(is.null(.$coordinates.coordinates.0), NA, .$coordinates.coordinates.0),
        coordinates.coordinates.1= ifelse(is.null(.$coordinates.coordinates.1), NA, .$coordinates.coordinates.1),
        withheld_in_countries.0= ifelse(is.null(.$withheld_in_countries.0), NA, .$withheld_in_countries.0)) %>%
    select(text,
           created_at,
           id,
           id_str,
           lang,
           possibly_sensitive,
           source,
           timestamp_ms,
           coord_longitude=coordinates.coordinates.0,
           coord_latitude=coordinates.coordinates.1,
           coordinates.type,
           place.name,
           place.country,
           place.country_code,
           entities.urls.0.expanded_url,
           withheld_in_countries.0,
           user.created_at,
           user.description,
           user.followers_count,
           user.friends_count,
           user.id,
           user.id_str,
           user.location,
           user.name,
           user.protected,
           user.screen_name,
           user.statuses_count,
           user.verified,
           user.location,
           user.time_zone,
           is_quote_status,
           quoted_status.created_at,
           quoted_status.id,
           quoted_status.id_str,
           quoted_status.text,
           quoted_status.lang,
           quoted_status.possibly_sensitive,
           quoted_status.coord_longitude= quoted_status.coordinates.coordinates.0,
           quoted_status.coord_latitude= quoted_status.coordinates.coordinates.1,
           quoted_status.place.name,
           quoted_status.place.country,
           quoted_status.place.country_code,
           quoted_status.entities.urls.0.expanded_url,
           quoted_status.user.created_at,
           quoted_status.user.description,
           quoted_status.user.friends_count,
           quoted_status.user.followers_count,
           quoted_status.user.id,
           quoted_status.user.id_str,
           quoted_status.user.screen_name,
           quoted_status.user.name,
           quoted_status.user.statuses_count,
           quoted_status.user.verified,
           quoted_status.user.protected,
           quoted_status.user.location,
           quoted_status.user.time_zone,
           retweeted_status.created_at,
           retweeted_status.id,
           retweeted_status.id_str,
           retweeted_status.text,
           retweeted_status.lang,
           retweeted_status.possibly_sensitive,
           retweeted_status.coord_longitude=retweeted_status.coordinates.coordinates.0,
           retweeted_status.coord_latitude=retweeted_status.coordinates.coordinates.1,
           retweeted_status.place.name,
           retweeted_status.place.country,
           retweeted_status.place.country_code,
           retweeted_status.entities.urls.0.expanded_url,
           retweeted_status.user.created_at,
           retweeted_status.user.description,
           retweeted_status.user.followers_count,
           retweeted_status.user.friends_count,
           retweeted_status.user.id,
           retweeted_status.user.id_str,
           retweeted_status.user.screen_name,
           retweeted_status.user.name,
           retweeted_status.user.statuses_count,
           retweeted_status.user.verified,
           retweeted_status.user.location,
           retweeted_status.user.protected,
           retweeted_status.user.time_zone,
           in_reply_to_screen_name,
           in_reply_to_status_id,
           in_reply_to_status_id_str,
           in_reply_to_user_id,
           in_reply_to_user_id_str,
           quoted_status.extended_tweet.full_text,
           retweeted_status.extended_tweet.full_text,
           extended_tweet.full_text) %>% 
    mutate(text_real= case_when(
        !is.na(extended_tweet.full_text) ~ extended_tweet.full_text ,
        !is.na(retweeted_status.extended_tweet.full_text) ~ retweeted_status.extended_tweet.full_text,
        !is.na(quoted_status.extended_tweet.full_text) ~ quoted_status.extended_tweet.full_text,
        TRUE ~ text)
        ) %>% 
    select(text_real, id_str, everything()) ->b
    





data %>% colnames

parallel::detectCores(logical = T)
mclapply(X = files, FUN = read_json, mc.cores=3)



library(tidyverse)

input_path <- "../local_folder/json/input/"
output_path <- "../local_folder/json/output/"
files <- list.files(input_path)

parse_function <- function (x){
    gc()
    json <- ndjson::stream_in(paste0(input_path, x)) %>% 
        filter(lang=="en") %>% 
        distinct(id_str,.keep_all = T) %>%
        filter(str_detect(string = text, pattern = regex('muslim|islam',ignore_case = T))) %>%
        mutate(  
            quoted_status.coordinates.coordinates.0= ifelse(is.null(.$quoted_status.coordinates.coordinates.0), NA, .$quoted_status.coordinates.coordinates.0),
            quoted_status.coordinates.coordinates.1= ifelse(is.null(.$quoted_status.coordinates.coordinates.1), NA, .$quoted_status.coordinates.coordinates.1),
            retweeted_status.coordinates.coordinates.0= ifelse(is.null(.$retweeted_status.coordinates.coordinates.0), NA, .$retweeted_status.coordinates.coordinates.0),
            retweeted_status.coordinates.coordinates.1= ifelse(is.null(.$retweeted_status.coordinates.coordinates.1), NA, .$retweeted_status.coordinates.coordinates.1),
            coordinates.coordinates.0= ifelse(is.null(.$coordinates.coordinates.0), NA, .$coordinates.coordinates.0),
            coordinates.coordinates.1= ifelse(is.null(.$coordinates.coordinates.1), NA, .$coordinates.coordinates.1),
            withheld_in_countries.0= ifelse(is.null(.$withheld_in_countries.0), NA, .$withheld_in_countries.0)) %>%
        select(text,
               created_at,
               id,
               id_str,
               lang,
               possibly_sensitive,
               source,
               timestamp_ms,
               coord_longitude=coordinates.coordinates.0,
               coord_latitude=coordinates.coordinates.1,
               coordinates.type,
               place.name,
               place.country,
               place.country_code,
               entities.urls.0.expanded_url,
               withheld_in_countries.0,
               user.created_at,
               user.description,
               user.followers_count,
               user.friends_count,
               user.id,
               user.id_str,
               user.location,
               user.name,
               user.protected,
               user.screen_name,
               user.statuses_count,
               user.verified,
               user.location,
               user.time_zone,
               is_quote_status,
               quoted_status.created_at,
               quoted_status.id,
               quoted_status.id_str,
               quoted_status.text,
               quoted_status.lang,
               quoted_status.possibly_sensitive,
               quoted_status.coord_longitude= quoted_status.coordinates.coordinates.0,
               quoted_status.coord_latitude= quoted_status.coordinates.coordinates.1,
               quoted_status.place.name,
               quoted_status.place.country,
               quoted_status.place.country_code,
               quoted_status.entities.urls.0.expanded_url,
               quoted_status.user.created_at,
               quoted_status.user.description,
               quoted_status.user.friends_count,
               quoted_status.user.followers_count,
               quoted_status.user.id,
               quoted_status.user.id_str,
               quoted_status.user.screen_name,
               quoted_status.user.name,
               quoted_status.user.statuses_count,
               quoted_status.user.verified,
               quoted_status.user.protected,
               quoted_status.user.location,
               quoted_status.user.time_zone,
               retweeted_status.created_at,
               retweeted_status.id,
               retweeted_status.id_str,
               retweeted_status.text,
               retweeted_status.lang,
               retweeted_status.possibly_sensitive,
               retweeted_status.coord_longitude=retweeted_status.coordinates.coordinates.0,
               retweeted_status.coord_latitude=retweeted_status.coordinates.coordinates.1,
               retweeted_status.place.name,
               retweeted_status.place.country,
               retweeted_status.place.country_code,
               retweeted_status.entities.urls.0.expanded_url,
               retweeted_status.user.created_at,
               retweeted_status.user.description,
               retweeted_status.user.followers_count,
               retweeted_status.user.friends_count,
               retweeted_status.user.id,
               retweeted_status.user.id_str,
               retweeted_status.user.screen_name,
               retweeted_status.user.name,
               retweeted_status.user.statuses_count,
               retweeted_status.user.verified,
               retweeted_status.user.location,
               retweeted_status.user.protected,
               retweeted_status.user.time_zone,
               in_reply_to_screen_name,
               in_reply_to_status_id,
               in_reply_to_status_id_str,
               in_reply_to_user_id,
               in_reply_to_user_id_str,
               quoted_status.extended_tweet.full_text,
               retweeted_status.extended_tweet.full_text,
               extended_tweet.full_text) %>% 
        mutate(text_real= case_when(
            !is.na(extended_tweet.full_text) ~ extended_tweet.full_text ,
            !is.na(retweeted_status.extended_tweet.full_text) ~ retweeted_status.extended_tweet.full_text,
            !is.na(quoted_status.extended_tweet.full_text) ~ quoted_status.extended_tweet.full_text,
            TRUE ~ text)) %>% 
        select(text_real, id_str, everything())
    write_csv(json, path = paste0(output_path, str_sub(x,end = -7), ".csv"),append = T, col_names =T )
    gc()
    }


#benchmarking 1 core vs 3 cores ====
microbenchmark::microbenchmark(
map(files, parse_function), times = 3
) #637.8224 seconds 


microbenchmark::microbenchmark(
    parallel::mclapply(X = files, FUN = parse_function, mc.cores=3), times = 3
) #310.2522 seconds, more than 2x

parse_jsonl <- compiler::cmpfun(parse_function)

microbenchmark::microbenchmark(
    parallel::mclapply(X = files, FUN = parse_function_c, mc.cores=3), times = 3
)


parse_jsonl(files[10])

