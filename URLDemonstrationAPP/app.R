library(shiny)
library(tidyverse)
library(wesanderson)
library(urltools)




# Define UI ----
ui <- navbarPage(
  titlePanel('Showcase of Mediestream API'),
  tabsetPanel(
    tabPanel("URL",
             sidebarLayout(
               sidebarPanel( 
                 textInput("url", 
                           p("Url:"), 
                           value = "")),
               mainPanel(
                 tableOutput('familyIdTable'),
                 tableOutput('lplaceTable'),
                 plotOutput("plot", click = "plot_click"),
                 verbatimTextOutput("info"),
                 textOutput("result"))))
  )
)

server <- function(input, output) {
  output$familyIdTable <- renderTable(
    read_csv(paste0(input$url)) %>% 
      count(familyId, sort = TRUE) %>% 
      top_n(10)
  )
  
  output$lplaceTable <- renderTable(
    read_csv(paste0(input$url)) %>% 
      count(lplace, sort = TRUE) %>% 
      top_n(10)
  )
  
  output$plot <- renderPlot({
    read_csv(paste0(input$url)) %>% 
      count(familyId, sort = TRUE) %>% 
      top_n(20) %>% 
      ggplot(aes(x = familyId, y = n))+ 
      geom_col(fill = "#0B775E") +
      coord_flip() +
      labs(title = paste0('Amount of articles mentioning \'revolution\''), 
           subtitle = "dispersed on familyId from 1840 to 1880",
           x = "familyId")
  }, res = 96) 
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
