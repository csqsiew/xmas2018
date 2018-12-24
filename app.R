library(shiny)
library(tidyverse)

# user interface
ui <- fluidPage(
    plotOutput("myImage"), # show image 
    tags$audio(src = "silver.wav", type = "audio/wav", autoplay = 1, controls = 1), # play audio
    tags$p('Christmas tune proudly created in R and free to download!'),
    tags$a(href="http://github.com/csqsiew/xmas2018", "R shiny code here") # github link for code 
)

# backend processes to create the image 
server <- function(input, output, session) {
  output$myImage <- renderPlot({
    n <- 120 # number of flakes 
    tibble(x = runif(n),  
                y = runif(n),  
                s = runif(n, min = 0.5, max = 4)) %>% # 4 vs 20 for pch 42 (stars)
      ggplot(aes(x, y, size = s)) + # use the default round points for plot 
      # geom_point(color = "grey", pch = 42) + # snowflakes
        geom_point(color = "grey90") + # snowballs?
      scale_size_identity() +
      coord_cartesian(c(0,1), c(0,1)) +
      theme_void() +
      theme(panel.background = element_rect("red2")) +
      annotate(geom = 'text', x=0.5, y=0.5, label="Merry Christmas and a\n Happy New Year!\nWishing everyone \nan AWESOME 2019!", 
               color = 'white', size = 12) +
      annotate(geom = 'text', x=0.5, y=0, label='A R Shiny Christmas card created with code adapted from @paulvanderlaken', 
               color = 'grey', size = 4)
    
  })
  
  }
  
shinyApp(ui = ui, server = server)
