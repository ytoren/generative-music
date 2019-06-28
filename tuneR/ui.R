library(shiny)
library(shinymaterial)
library(shinyjs)

source('R/config.R')

ui <- material_page(
  useShinyjs(),
  ## Title
  titlePanel("Randomizing Bach"),
  include_fonts = TRUE,
  nav_bar_color = "teal",
  
  material_row(
    ## Sidebar
    material_column(
      width = 3,
      background_color = 'purple lighten-5',
      material_row(
        material_card(
          title = 'Configuration',
          divider = TRUE,
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
          )
        ),
        actionButton(inputId = 'recalc', label = 'Run Algorithm')
      )
    ),
    
    ## Main panel
    material_column(
      width = 9,
      material_card(
        title = '',
        tags$p('This Shiny app starts from a completely random set of music notes, and tries to get "as close as possible" to the first few notes from Bach\'s Prelude from Suite No. 1 for Cello solo (BWV 1007) using genetic algorithms. Think of it as an exercise in trying to mediate between Bach and Schöneberg...'),
        tags$p('After running the algorithm once, you can play with the fraction of correct notes you want to hear, generate music and discover what gets your groove on! (for me it\'s around 65-70% match). If the algorithms did not score high enough for the choosen level, the higest match will be used instead. If you still want more, change the algorithm\'s settings and see what happens. Higher population / iterations mean longer running times, so please be patient :)')
      ),
      
      material_card(
        title = 'Algorithm Progress',
        plotOutput('plot_progress', height = 300),
        ## Select music
        sliderInput(
          inputId = 'cutoff_point', 
          label = 'Select fraction of correct notes',
          value = 0.00,
          min = .00, max = 1, step = .01,
          width = '100%'
        ),
        actionButton(inputId = 'regen', label = 'Generate Music')
      ),
      
      material_card(
        title = NULL,
        tags$p(tags$b('Original Prelude:')),
        tags$p(
          tags$a(href = prelude_filename, 'Play', target = '_blank'),
          ' / Code representation: ',
          span(prelude, style = 'font-family: courier new')
        ),
        #tags$audio(src = prelude_filename, type = 'audio/wav', autoplay = FALSE, controls = NA),
        tags$br(),
        tags$p(tags$b('Generated Music:')),
        tags$p(
          tags$a(href = selected_melody_filename, 'Play', target = '_blank'),
          ' / Code representation: ',
          span(textOutput('selected_melody', inline = TRUE), style = 'font-family: courier new')
        )
        # tags$audio(src = selected_melody_filename, type = 'audio/wav', autoplay = FALSE, controls = NA),
      ),
      
      material_card(
        title = 'Credits',
        tags$p(
          'Based on this ', 
          tags$a('wonderful post', href = 'https://fronkonstin.com/2017/04/27/genetic-music-from-schoenberg-to-bach/'),
          ' by ', 
          tags$a('@aschinchon', href = 'https://twitter.com/intent/follow?screen_name=aschinchon'), 
          ' + improvements to speed and a Shiny app'
        )   
      ),

      ## Contact
      material_card(
        title = NULL,
        tags$p(
          tags$a('GitHub', href = 'https://www.github.com/ytoren/generative-music/'), 
          ' / ',
          tags$a('Twitter', href = 'https://www.twitter.com/BigEndianB'),
          ' / ',
          tags$a('LinkedIn', href = 'https://www.linkedin.com/in/ytoren/'), 
          align = 'center'
        )
      )
   
    )    
  )
)