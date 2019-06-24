# WAV conversion ----------------------------------------------------------
## Convert string to WAV object
library(tuneR)
library(purrr)
library(magrittr)
library(dplyr)

str_to_wav <- function(str, f) {
  str %>% 
    strsplit('') %>% 
    .[[1]] %>% 
    tibble(music = .) %>% 
    left_join(f, by = c('music' = 'code')) %>% 
    mutate(wav = map(frequency, sine, duration = 10000)) %>% 
    pull(wav) %>% 
    reduce(bind) %>%
    return()
}
