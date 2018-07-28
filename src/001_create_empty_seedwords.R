library(tidyverse)
seed_words <- tibble() %>% 
    mutate(id=NA, words=NA, context=NA, explanation=NA) %>% 
    add_row(id=1:50)
write_csv(seed_words, "data/FOC_seed_words.csv")