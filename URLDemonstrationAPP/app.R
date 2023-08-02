library(shiny)
library(tidyverse)
library(wesanderson)
library(urltools)
library(shinyWidgets)

revolutionValue = "http://labs.statsbiblioteket.dk/labsapi/api/aviser/export/fields?query=revolution%20AND%20py%3A%5B1840%20TO%201880%5D&fields=link&fields=recordID&fields=timestamp&fields=pwa&fields=cer&fields=fulltext_org&fields=pageUUID&fields=editionUUID&fields=titleUUID&fields=editionId&fields=familyId&fields=newspaper_page&fields=newspaper_edition&fields=lplace&fields=location_name&fields=location_coordinates&max=1&structure=header&structure=content&format=CSV"

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
                           value = "", 
                           placeholder = "copy URL here"),
                           actionButton("load", "Load"),
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
  
  data <- eventReactive(input$load, {
    data <- read_csv(paste0(input$url))
  })
  
  output$familyIdTable <- renderTable(
    data() %>% 
      count(familyId, sort = TRUE) %>% 
      top_n(10)
  )
  
  output$lplaceTable <- renderTable(
    data() %>% 
      count(lplace, sort = TRUE) %>% 
      top_n(10)
  )
  
  output$plot <- renderPlot({
    data() %>% 
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
