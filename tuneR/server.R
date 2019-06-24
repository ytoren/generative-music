#!/user/bin/R
# Setup -------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringdist)

## cello notes
frequencies <- tibble(
  n=-33:14,
  notes = c("C2", "C#2/Db2", "D2", "D#2/Eb2", "E2", "F2", "F#2/Gb2", "G2", "G#2/Ab2", "A2", "A#2/Bb2", "B2", "C3", "C#3/Db3", "D3", "D#3/Eb3", "E3", "F3", "F#3/Gb3", "G3", "G#3/Ab3", "A3", "A#3/Bb3", "B3", "C4", "C#4/Db4", "D4", "D#4/Eb4", "E4", "F4", "F#4/Gb4", "G4", "G#4/Ab4", "A4", "A#4/Bb4", "B4", "C5", "C#5/Db5", "D5", "D#5/Eb5", "E5", "F5", "F#5/Gb5", "G5", "G#5/Ab5", "A5", "A#5/Bb5", "B5"),
  code=c(letters, toupper(letters))[1:48]
) %>% 
  mutate(frequency = round(440*(2^(1/12))^n,4))

source('R/aux_functions.R')
source('R/genetic_algo.R')
source('R/config.R')

# Cleanup old files and generate prelude
if (file.exists(paste0(media_path, prelude_filename))) {
  file.remove(paste0(media_path, prelude_filename))
}
if (file.exists(paste0(media_path, selected_melody_filename))) {
  file.remove(paste0(media_path, selected_melody_filename))
}

prelude %>% 
  str_to_wav(f = frequencies) %>% 
  writeWave(paste0(media_path, prelude_filename))


# Server ------------------------------------------------------------------
server <- shinyServer(
  function(input, output, session) { 
    
    shinyjs::disable('regen')
    
    v <- reactiveValues(
      results = tibble(iteration = 1:50, fitness = 0),
      selected_point = list(iteration = 0, y = 0.01)
    )
    
    ## Initial display
    
    
    # Re-run algo
    observeEvent(
      input$recalc, 
      {
        shinyjs::disable('recalc')
        shinyjs::disable('regen')
        
        req(input$popsize)
        
        population <- generate_initial_population(
          target_notes = prelude,
          popsize = as.integer(input$popsize),
          genes = frequencies$code,
          seed = as.integer(input$seed)
        )
        
        v$results <- run_evolution(
          population = population,
          target_notes = prelude,
          genes = frequencies$code,
          crossover_rate = as.numeric(input$crossover_rate),
          mutation_rate = as.numeric(input$mutation_rate),
          maxiter = as.integer(input$maxiter),
          pb = FALSE
        )
        
        shinyjs::enable('recalc')
        shinyjs::enable('regen')
      }
    )
    #output$results <- renderTable({v$results})
    
    ## Reset cutoff point
    observeEvent(
      input$cutoff_point,
      {
        req(v$results)
        
        y <- v$results$fitness / str_length(prelude)
        point_y <- as.numeric(input$cutoff_point)
        y_gaps <- abs(y - point_y)
        point_x <- v$results$iteration[y_gaps == min(y_gaps)][1]
        v$selected_point <- list(iteration = point_x, y = point_y)
        # message(paste0('Point = (', point_x, ',', point_y, '), diff = ', y_gaps[point_x]))
        
      }
    )
    
    ## Generate new music
    observeEvent(
      input$regen,
      {
        req(v$results)
        req(v$selected_point)
        
        selected_melody <- v$results$best_melody[v$selected_point[['iteration']]][1]
        
        output$selected_melody <- renderText({selected_melody})
        
        selected_melody %>% 
          str_to_wav(f = frequencies) %>% 
          writeWave(paste0(media_path, selected_melody_filename))
      }
    )
    
    ## Plot
    output$plot_progress <- renderPlot(
      {
        req(v$results)
        req(v$selected_point)
        
        v$results %>%
          ggplot(aes(
            x = v$results$iteration, 
            y = v$results$fitness / str_length(prelude)
          )) %>%
          +geom_line(stat = 'identity') %>%
          +ylab('Fraction of correct notes') %>%
          +xlab('Iteration') %>% 
          # +ggtitle('Algorithm Progress') %>% 
          +ylim(0,1) %>%
          +geom_hline(yintercept = v$selected_point[['y']]) %>%
          +geom_vline(xintercept = v$selected_point[['iteration']])
      }
    )
  }
)
