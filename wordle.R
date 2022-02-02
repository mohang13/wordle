library(tidyverse)

# Downloding word list----
download.file("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt", "words.txt")


# Filter for 5 letter words and remove words with characters and numbers----
here::here("words.txt") %>% 
  read_tsv() -> df

df %>%
  janitor::clean_names() %>% 
  filter(str_length(x2) == 5) %>% 
  filter(parse_number(x2) %>% is.na()) %>% 
  # mutate(uni_len = map_dbl(x2, unique_length)) %>% 
  # filter(uni_len == 5) %>% 
  select(x2) %>% 
  filter(!str_detect(x2, "\\.|-|'|\\/")) %>% 
  rename(word = x2) %>% 
  write_csv(here::here("wordle","wordle_words.csv"))

here::here("wordle","wordle_words.csv") %>% 
  read_csv() -> wordle_list


# Split the word into individual letters in column for further analysis----
wordle_list %>%
  mutate(word = word %>% tolower()) %>%
  separate(col = word, into = paste0("x", 0:5), sep = "") %>%
  select(-x0) %>%
  mutate(word = (wordle_list$word) %>% tolower(), .before = x1) -> wordle_extension


# Ranking the letters in each position and their scores----
wordle_extension %>% 
  pivot_longer(cols = -1) %>% 
  mutate(name = name %>% parse_number()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  arrange(name, n) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(value = value %>% as.factor(),
         rank = row_number()) %>% 
  write_csv(here::here("wordle", "wordle_ranking.csv"))

here::here("wordle", "wordle_ranking.csv") %>% 
  read_csv() -> wordle_ranking

# Checking whether the scores uses random distribution or not----
wordle_ranking %>%
  filter(name == 1) %>%
  pull(n) -> vv

dnorm(vv, mean(vv), sd(vv))

plot(vv,dnorm(vv, mean(vv), sd(vv)))

# They are seems to be randomly distributed so I went ahead and used the scores directly rather than the ranks for sorting the words

# Ranking the words based on score, the filtered words will be sorted based on this ranking----
which_score <- function(letter, position, df){
  df %>% 
    filter(name == position & value == letter) %>% 
    pull(n)
}

wordle_extension %>% 
  mutate(x1 = map2_dbl(x1, 1, which_score, wordle_ranking),
         x2 = map2_dbl(x2, 2, which_score, wordle_ranking),
         x3 = map2_dbl(x3, 3, which_score, wordle_ranking),
         x4 = map2_dbl(x4, 4, which_score, wordle_ranking),
         x5 = map2_dbl(x5, 5, which_score, wordle_ranking),
         score = x1+x2+x3+x4+x5) %>%
  arrange(desc(score)) %>% 
  mutate(uni_len = map_dbl(word, unique_length)) %>% 
  arrange(desc(uni_len), desc(score)) %>% 
  write_csv(here::here("wordle", "wordle_scores.csv"))


here::here("wordle", "wordle_scores.csv") %>% 
  read_csv() -> wordle_scores

