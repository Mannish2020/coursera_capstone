
library(tidyverse)

blogs <- read_lines('~/Datasets/coursera/en_US.blogs.txt')

ind <- sample(seq_along(blogs), 20000)

blogs.samp <- blogs[ind]

library(quanteda)

corp <- corpus(blogs.samp)

toks <- tokens(corp, remove_punct = TRUE,remove_symbols = TRUE,remove_numbers = TRUE, remove_separators = TRUE,
               remove_url = TRUE)

toks_ngrams <- tokens_ngrams(toks,n=1:5)



toks_containing_text <- function(.words){
  .d <- 
    do.call(rbind,
          lapply(
            toks_ngrams,function(x) as.integer(any(x %in% .words))
          )
      )
  names_nonzero_texts <- names(.d[which(l2 != 0),])
  toks_ngrams[names_nonzero_texts]
}


s.func <- function(.word){
  
  len <- length(str_split(.word,"_")[[1]])
  
  if(len == 2){
    setdiff(
      toks_containing_text(.words = .word) %>% str_subset(paste0("^",.word)) %>%
        str_subset("([:punct:](.)+){2}"),
      toks_containing_text(.words = .word) %>% str_subset(paste0("^",.word)) %>%
        str_subset("([:punct:](.)+){3}")
    )
  }
  else if(len == 3){
      setdiff(
        toks_containing_text(.words = .word) %>% str_subset(paste0("^",.word)) %>%
          str_subset("([:punct:](.)+){3}"),
        toks_containing_text(.words = .word) %>% str_subset(paste0("^",.word)) %>%
          str_subset("([:punct:](.)+){4}")
      ) 
    } 
  else if(len == 4){
      setdiff(
        toks_containing_text(.words = .word) %>% str_subset(paste0("^",.word)) %>%
          str_subset("([:punct:](.)+){4}"),
        toks_containing_text(.words = .word) %>% str_subset(paste0("^",.word)) %>%
          str_subset("([:punct:](.)+){5}")
      ) 
    }

};s.func("that_brings_clarity_to")

