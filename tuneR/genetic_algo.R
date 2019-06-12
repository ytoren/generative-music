# Fitness Scoring ---------------------------------------------------------
score_fitness <- function(x) {
  # 2^(1 - (stringdist(x, prelude, method="hamming") - n))
  n - stringdist(x, prelude, method="hamming")
}


# Genetic crossover -------------------------------------------------------
cross_over <- function(p1, p2, crossover_point) {
  paste0(
    substr(p1, 1, crossover_point), 
    substr(p2, crossover_point + 1, nchar(p2))
  )
}


# Mutation ----------------------------------------------------------------
mutate_genes_str <- function(x, genes = frequencies$code) {
  for (j in 1:n) if(runif(1) < mutation_rate) substr(x,j,j) = sample(genes,1)
  return(x)
}

mutate_genes_vec <- function(x, genes = frequencies$code) {
  new_genes <- sample(x = genes, size = nchar(x), replace = TRUE)
  replace_filter <- runif(n = n) < mutation_rate
  
  x <- strsplit(x, '')
  ## Replace values 
  x[replace_filter] <- new_genes[replace_filter]
  
  return(paste(x, collapse = ''))
}


# Initiate population ----------------------------------------------------------------
generate_initial_population <- function(popsize = 500, seed = NULL) {
  ## Set seed for innitial population
  if(!is.null(seed) & is.integer(seed)) {set.seed(seed)}
  
  population <- replicate(
    popsize, 
    sample(frequencies$code, n, replace = TRUE)
  ) %>% 
    apply(2, function(x) paste(x,collapse=''))
  
  return(tibble(
    codes = population,
    fitness = sapply(population, score_fitness, USE.NAMES=FALSE)
  ))
  
}


# Pair / crossover & mutate -----------------------------------------------
evolution_step <- function(
  population, ## DF with codes & fitness
  crossover_rate = 0.25,
  mutation_rate = 0.01,
  timing = FALSE
){
  ## empty vector for new population
  n <- nchar(population$codes[1])
  new_codes <- rep(NA, popsize)
  
  ## Select parents
  tic = Sys.time()
  for (i in 1:(nrow(population)/2)) {
    parents <- sample(
      x = 1:nrow(population),
      size = 2,
      prob = population$fitness / sum(population$fitness),
      replace = FALSE
    )
    p1 <- population[parents[1], 'codes'] %>% unlist()
    p2 <- population[parents[2], 'codes'] %>% unlist()
    
    ## Crossover
    crossover_point <- sample(1:(n - 1), 1)
    if (runif(1) > crossover_rate) {
      q1 <- cross_over(p1, p2, crossover_point)
      q2 <- cross_over(p2, p1, crossover_point)
    } else {
      q1 <- p1
      q2 <- p2
    }
    
    ## Mutatation
    # microbenchmark::microbenchmark(
    #   mutate_genes_vec(q1),
    #   mutate_genes_str(q1),
    #   {for (j in 1:n) if(runif(1) < mutation_rate) substr(q1,j,j) = sample(frequencies$code,1)}
    # )
    q1 <- mutate_genes_str(q1)
    q2 <- mutate_genes_str(q2)
    
    ## Store
    # population2 <- c(population2, c(p1, p2))
    new_codes[(2*(i-1) + 1):(2*i)] <- c(q1, q2)
  }
  if (timing) {message(Sys.time() - tic)}
  
  return(tibble(
    codes = new_codes %>% unlist(),
    fitness = sapply(new_codes, score_fitness, USE.NAMES=FALSE)
  ))
}


# Run algorithm -----------------------------------------------------------
run_evolution <- function(
  population, 
  crossover_rate = 0.25, 
  mutation_rate = 0.01,
  maxiter = 200,
  pb = TRUE, 
  verbose = FALSE
) {
  #fitness <- rep(1/popsize, popsize)
  results <- NULL
  maxfitenss_iter <- 0 
  iter <- 1
  
  if (pb) {pbar <- txtProgressBar(min = 1, max = maxiter, width = 50)}

  while(maxfitenss_iter < nchar(population$codes[1]) & iter < maxiter) {
    if (pb) {setTxtProgressBar(pbar, value = iter)}
    
    ## New population
    population2 <- evolution_step(
      population = population, 
      crossover_rate = crossover_rate, 
      mutation_rate = mutation_rate, 
      timing = verbose
    )
    population <- population2
    bestfit <- population$codes[min(which.max(population$fitness))]
    maxfitenss_iter <- max(population$fitness)
    if (verbose) {message(paste0("Iteration ",iter, ": ", bestfit, ', fitness = ', maxfitenss_iter))}
    
    ## Move counter
    iter <- iter + 1
    
    ## Collect results
    results <- rbind(
      results,
      tibble(
        iteration = iter,
        fitness = maxfitenss_iter,
        best_melody = bestfit
        # correct_notes = log(maxfitenss_iter, base = 2)-1
      )
    )
  }
  
  return(results)
}
