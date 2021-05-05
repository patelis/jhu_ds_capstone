quadgram <- corpora %>% 
  unnest_tokens(quadgram, value, token = "ngrams", n = 4) %>% 
  dt_separate(col = quadgram, into = c("word1", "word2", "word3", "word4"), sep = " ", remove = FALSE)

quadgram <- quadgram %>% 
  lazy_dt() %>%
  filter(str_length(word1) < 19, 
         str_detect(word1, "^[a-zA-Z]+$"), 
         !str_detect(word1, "([a-zA-Z]+)\\1{3,}"), 
         str_length(word2) < 19, 
         str_detect(word2, "^[a-zA-Z]+$"), 
         !str_detect(word2, "([a-zA-Z]+)\\1{3,}"), 
         str_length(word3) < 19, 
         str_detect(word3, "^[a-zA-Z]+$"), 
         !str_detect(word3, "([a-zA-Z]+)\\1{3,}"), 
         str_length(word4) < 19, 
         str_detect(word4, "^[a-zA-Z]+$"), 
         !str_detect(word4, "([a-zA-Z]+)\\1{3,}")) %>% 
  as_tibble() %>% 
  anti_join(profanity, by = c("word1" = "word")) %>% # joins currently don't work very well with dtplyr
  anti_join(profanity, by = c("word2" = "word")) %>% 
  anti_join(profanity, by = c("word3" = "word")) %>% 
  anti_join(profanity, by = c("word4" = "word")) 

quadgram_count <- quadgram %>% 
  lazy_dt() %>% 
  group_by(quadgram, word1, word2, word3, word4) %>% 
  count() %>% 
  ungroup() %>% 
  as_tibble()

quadgram_count %>% filter(word3 == "going", word2 == "am") %>% arrange(desc(n)) %>% slice_head(n = 1) %>% pull(word4)
# as.data.table(quadgram_count)[word3 == "going" & word2 == "am"][order(n, decreasing = TRUE)][1, .(word4)]

library(tictoc)
library(vroom)
library(lobstr)

tic()

test <- vroom("testtxt.txt", delim = "\t", 
              col_types = cols(
                word1 = col_character(),
                word2 = col_character(),
                word3 = col_character(),
                word4 = col_character(),
                n = col_double()
              )
)

toc()

tic()

test2 <- readRDS("testrds.rds")

toc()