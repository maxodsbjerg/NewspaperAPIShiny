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
                           value = "http://labs.statsbiblioteket.dk/labsapi/api/aviser/export/fields?query=revolution%20AND%20py%3A%5B1840%20TO%201880%5D&fields=link&fields=recordID&fields=timestamp&fields=pwa&fields=cer&fields=fulltext_org&fields=pageUUID&fields=editionUUID&fields=titleUUID&fields=editionId&fields=familyId&fields=newspaper_page&fields=newspaper_edition&fields=lplace&fields=location_name&fields=location_coordinates&max=1&structure=header&structure=content&format=CSV",
                           placeholder = "copy URL here")),
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
