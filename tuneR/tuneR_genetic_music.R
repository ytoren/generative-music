## From https://fronkonstin.com/2017/04/27/genetic-music-from-schoenberg-to-bach/

## Setup
library(tuneR)
library(stringdist)
library(dplyr)
library(purrr)
library(ggplot2)


# Definitions -------------------------------------------------------------
popsize <- 500 # Population size
maxiter <- 500 # Max number of iterations
crossover_rate <- 0.25 # Probability of parent crossover
mutation_rate <- 0.01 # Probability of mutation per locus
seed <- 1234

## From: https://stackoverflow.com/a/27135335
setWavPlayer('/usr/bin/afplay')

## Note for Bach's prelude
prelude="tAJHJAJAtAJHJAJAtCKJKCKCtCKJKCKCtEKJKEKEtEKJKEKEtFJHJFJFtFJHJFJF"
n <- nchar(prelude)

## freq <- function(n) {440*(2^(1/12))^n}

## cello notes
frequencies <- tibble(
  n=-33:14,
  notes = c("C2", "C#2/Db2", "D2", "D#2/Eb2", "E2", "F2", "F#2/Gb2", "G2", "G#2/Ab2", "A2", "A#2/Bb2", "B2", "C3", "C#3/Db3", "D3", "D#3/Eb3", "E3", "F3", "F#3/Gb3", "G3", "G#3/Ab3", "A3", "A#3/Bb3", "B3", "C4", "C#4/Db4", "D4", "D#4/Eb4", "E4", "F4", "F#4/Gb4", "G4", "G#4/Ab4", "A4", "A#4/Bb4", "B4", "C5", "C#5/Db5", "D5", "D#5/Eb5", "E5", "F5", "F#5/Gb5", "G5", "G#5/Ab5", "A5", "A#5/Bb5", "B5"),
  code=c(letters, toupper(letters))[1:48]
) %>% 
  mutate(frequency = round(440*(2^(1/12))^n,4))


# Aux functions -----------------------------------------------------------
source('tuneR/aux_functions.R')
source('tuneR/genetic_algo.R')


# RUN ---------------------------------------------------------------------
## Initial ppulation
population <- generate_initial_population(popsize = popsize, seed = seed)

## Comapre
## Bach's melody 
prelude %>% str_to_wav() %>% play()
## Best our of random
population[min(which.max(fitness))] %>% str_to_wav() %>% play()

## Run genetic algo
results <- run_evolution(population, crossover_rate, mutation_rate, maxiter)


# Analysis ----------------------------------------------------------------
results %>%
  ggplot(aes(x = iteration, y = fitness/ n)) %>%
  +geom_line() +ylab('% correct notes')

correct_frac <- 0.70

## Enjoy! :D
m <- which.min(abs(results$fitness / n - correct_frac))[1]
results[m,] %>%
  pull(best_melody) %>%
  as.character() %>%
  str_to_wav() %>%
  play()


