library(shiny)
library(shinymaterial)
library(shinyjs)

source('R/config.R')

ui <- fluidPage(
  useShinyjs(),
  ## Title
  titlePanel("Randomizing Bach"),
  
  sidebarLayout(
    
    ## Sidebar
    sidebarPanel = sidebarPanel(
      
      tags$h3('Configuration'), 
      
      sliderInput(
        inputId = 'crossover_rate', 
        label = 'Probability of parent-crossover',
        value = 0.25,
        min = .01, max = 1, step = .01
      ),
      
      sliderInput(
        inputId = 'mutation_rate', 
        label = 'Probability of single locus mutation',
        value = 0.01,
        min = .01, max = 1, step = .01
      ),
      
      sliderInput(
        inputId = 'popsize',
        label = 'Population size',
        value = 200,
        min = 10, max = 1000, step = 10
      ),
      
      sliderInput(
        inputId = 'maxiter',
        label = 'Maximun number if iterations',
        value = 150,
        min = 10, max = 300, step = 10
      ),
      
      numericInput(
        inputId = 'seed',
        label = 'Seed for initial random music',
        value = 1234
      ),
      
      actionButton(inputId = 'recalc', label = 'Run Genetic Algorithm')
    ),
    
    ## Main panel
    mainPanel = mainPanel(
      tags$h3('Algorithm Progress'),
      plotOutput('plot_progress', height = 200),
      ## Select music
      tags$hr(),
      
      tags$h3('Select tune'),
      tags$p('After running the algorithm once, you can play with the selection the fraction of correct notes, generate music and find what gets your groove on! (for me it\'s around 65-70% match).'),
      tags$p('If the algorithms did not score high enough for the choosen fraction, the higest match will be used instead. If you still want more, change the algorithm\'s settings and see what happens. Higher population / iterations mean longer running times, so please be patient :)'),
      tags$br(),
      
      sliderInput(
        inputId = 'cutoff_point', 
        label = 'Fraction of correct notes',
        value = 0.00,
        min = .00, max = 1, step = .01,
        width = '100%'
      ),
      actionButton(inputId = 'regen', label = 'Generate Music'),
      
      tags$hr(),
      h4('Original Prelude'),
      tags$p(
        tags$a(href = prelude_filename, 'Play', target = '_blank'),
        span(prelude, style = 'font-family: courier new')
      ),
      
      h4('Generated Music'),
      tags$p(
        tags$a(href = selected_melody_filename, 'Play', target = '_blank'),
        span(textOutput('selected_melody', inline = TRUE), style = 'font-family: courier new')
      ),
      
      tags$hr(),
      tags$p(
        tags$b('Credits: '),
        'Based on this ', 
        tags$a('wonderful post', href = 'https://fronkonstin.com/2017/04/27/genetic-music-from-schoenberg-to-bach/'),
        ' by ', 
        tags$a('@aschinchon', href = 'https://twitter.com/intent/follow?screen_name=aschinchon'), 
        ' + improvements to speed and a Shiny app'
      ),
      tags$br(),
      tags$p(
        tags$a('GitHub', href = 'https://www.github.com/ytoren/generative-music/'), 
        ' / ',
        tags$a('Twitter', href = 'https://www.twitter.com/BigEndianB'),
        ' / ',
        tags$a('LinkedIn', href = 'https://www.linkedin.com/in/ytoren/'), 
        align = 'center'
      )
      
      # fluidRow(
      #   tags$h3('Original Prelude'),
      #   tags$audio(src = prelude_filename, type = 'audio/wav', autoplay = FALSE, controls = NA)
      # ),
      # fluidRow(
      #   tags$h3('Algorithm output'),
      #   tags$audio(src = selected_melody_filename, type = 'audio/wav', autoplay = FALSE, controls = NA)
      # )
    )
  )
  
)