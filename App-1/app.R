library(shiny)
library(tidyverse)
library(wesanderson)
library(urltools)




# Define UI ----
ui <- navbarPage(
  titlePanel('Faceting hits from queries to Mediestream'),
  tabsetPanel(
    tabPanel("familyId",
  sidebarLayout(
    sidebarPanel( 
      textInput("text", 
                p("Query:"), 
                value = "revolution"),
      numericInput("num", 
                               p("How many newspapers should be shown?"), 
                               value = 10)),
    mainPanel(
      plotOutput("plot", click = "plot_click"),
      verbatimTextOutput("info"),
      textOutput("selected_facet")))),
  tabPanel("py",
           sidebarLayout(
             sidebarPanel( 
               textInput("text1", 
                         p("Query:"), 
                         value = "revolution"),
               selectInput("sort", 
                           p("Sort order of the content"),
                           choices = c("count", "index"),
                           selected = "count")),
             mainPanel())
              )
  )
)

server <- function(input, output) {
  
    output$plot <- renderPlot({
      read_csv(paste0('http://labs.statsbiblioteket.dk/labsapi/api/aviser/query/facet?query=', url_encode(input$text),'&field=familyId&sort=count&limit=400&format=CSV'), skip = 1, col_names = c("facet", "count")) %>% 
        mutate(facet = reorder(facet, count)) %>% 
        top_n(input$num) %>% 
        ggplot(aes(x = facet, y = count))+
        geom_col(fill = "#0B775E") +
        coord_flip() +
        labs(title = paste0('Amount of articles mentioning ', input$text), 
             subtitle = "dispersed on familyId from 1666 to 1881",
             x = "familyId")
    }, res = 96) 
  
  }

# Run the app ----
shinyApp(ui = ui, server = server)