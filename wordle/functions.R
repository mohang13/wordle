"wordle_words.csv" %>% 
  read_csv() -> wordle_list

"wordle_scores.csv" %>% 
  read_csv() -> wordle_scores

wordle_list %>%
  mutate(word = word %>% tolower()) %>%
  separate(col = word, into = paste0("x", 0:5), sep = "") %>%
  select(-x0) %>%
  mutate(word = (wordle_list$word) %>% tolower(), .before = x1) -> wordle_extension


# The magical function which does the filtration
wordle_crack <- function(df, cw, mw, nt){
  if(nt == ""){
    df -> ttf
  } else {
    setdiff(nt %>% str_split("") %>% unlist(), cw %>% names()) %>% paste(collapse = "") -> nt
    setdiff(nt %>% str_split("") %>% unlist(), mw %>% names()) %>% paste(collapse = "") -> nt
    df %>% 
      filter(!str_detect(word, nt %>% str_split("") %>% unlist() %>% paste(collapse = "|"))) -> ttf
  }
  
  if(!is.null(mw)){
    for(i in 1:length(mw)){
      ttf %>% 
        filter(str_detect(word, (mw %>% names())[i])) -> ttf
    }
  }
  
  if(is.null(cw) & !is.null(mw)){
    for(i in 1:length(mw)){
      ttf %>% 
        filter(.data[[paste0("x", mw[i])]] != (mw[i] %>% names())) -> ttf
    }
  } else if(!is.null(cw) & is.null(mw)){
    for(i in 1:length(cw)){
      ttf %>% 
        filter(.data[[paste0("x", cw[i])]] == (cw[i] %>% names())) -> ttf
    }
  } else if(!is.null(cw) & !is.null(mw)){
    for(i in 1:length(mw)){
      ttf %>% 
        filter(.data[[paste0("x", mw[i])]] != (mw[i] %>% names())) -> ttf
    }
    
    for(i in 1:length(cw)){
      ttf %>% 
        filter(.data[[paste0("x", cw[i])]] == (cw[i] %>% names())) -> ttf
    }
  }
  ttf 
}



# A function to find unique length in a string
unique_length <- function(string){
  string %>% 
    tolower() %>% 
    str_split(pattern = "") %>% 
    unlist() %>% 
    unique() %>% 
    length() 
}