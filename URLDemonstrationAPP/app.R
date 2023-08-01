library(shiny)
library(tidyverse)
library(wesanderson)
library(urltools)
library(shinyWidgets)




# Define UI ----
ui <- fluidPage(
  setBackgroundColor(
    color = "#b28906",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),

  titlePanel('Showcase of Mediestream API'),
  tabsetPanel(
    tabPanel("URL",
             sidebarLayout(
               sidebarPanel( 
                 tags$style(".well {background-color:#b98f07;}"),
                 fluidRow(textInput("url", 
                           p("Url:"), 
                           value = "")
                 ),
                img(src='DKB_logo_expanded_black_small_RGB.png', align = "bottomleft", width = "50%")),
               mainPanel(
                 tableOutput('familyIdTable'),
                 tableOutput('lplaceTable'),
                 plotOutput("plot", click = "plot_click"),
                 verbatimTextOutput("info"),
                 textOutput("result"))),
           )
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
      slice_max(n, n = 20) %>% 
      mutate(familyId = reorder(familyId, n)) %>% 
      ggplot(aes(x = familyId, y = n))+ 
      geom_col(fill = "#0B775E") +
      coord_flip() +
      labs(title = paste0('Amount of articles mentioning search term'), 
           subtitle = "dispersed on familyId from 1840 to 1880",
           x = "familyId")
  }, res = 96)
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
