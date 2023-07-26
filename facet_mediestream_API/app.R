#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(urltools)
library(wesanderson)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Mediestream Metadata\n Visualiser",   
    tabPanel("Welcome", h1("Welcome to the Mediestream Metadata Visualiser"),
                            h2("Visualise Danish newspaper data from 1666 to 1880"),
                           p("Explore the panels above to gain interesting insights regarding the Danish newspaper collection"),
                           p("This paragraph needs to tell the user at the visualiser asumes basic knowledge of searching Mediestream and how the lowest entity is articles")),
    navbarMenu("Disperse hits on metadata", 
               tabPanel("familyId",
                        sidebarLayout(
                            sidebarPanel( 
                                textInput("text", 
                                          p("Query:"), 
                                          value = "revolution"),
                                dateRangeInput("familyIddate", 
                                               p("Date range"),
                                               start = "1666-01-01",
                                               end = "1880-12-31"),
                                numericInput("num", 
                                             p("How many newspapers should be shown?"), 
                                             value = 10)),
                            mainPanel(
                                plotOutput("familyIdplot", click = "plot_click"),
                                verbatimTextOutput("info"),
                                textOutput("selected_facet")))
                        ),
               tabPanel("publication year(py)", 
                        sidebarLayout(
                            sidebarPanel( 
                                textInput("pytext", 
                                          p("Query:"), 
                                          value = "revolution"),
                                sliderInput("pyslider", 
                                            label = "Years of interest",
                                            min = 1666, 
                                            max = 1881, 
                                            value = c(1666, 1881),
                                            sep = "",
                                            ticks = TRUE)),
                            mainPanel(plotOutput("pyplot", click = "plot_click")
                                      ))),
               tabPanel("lplace", "four-c")),
    tabPanel("panel 2", "two"),
    tabPanel("panel 3", "three")
) 


server <- function(input, output) {
    
    output$familyIdplot <- renderPlot({
        familyId_total <- read_csv('http://labs.statsbiblioteket.dk/labsapi/api/aviser/query/facet?query=%2A&field=familyId&sort=count&limit=400&format=CSV', skip = 1, col_names = c("familyId", "total"))
        
        read_csv(paste0('http://labs.statsbiblioteket.dk/labsapi/api/aviser/query/facet?query=', url_encode(input$text),'&field=familyId&sort=count&limit=400&format=CSV')) %>% 
            left_join(familyId_total, by = "familyId") %>% 
            mutate(freq = count / total) %>% 
            mutate(familyId = reorder(familyId, freq)) %>%
            top_n(input$num) %>% 
            ggplot(aes(x = familyId, y = freq))+
            geom_col(fill = "#0B775E") +
            coord_flip() +
            labs(title = paste0('Amount of articles mentioning ', input$text), 
                 subtitle = "dispersed on familyId from 1666 to 1881",
                 x = "familyId")
    }, res = 96) 
    
    
# Visualising py
# Loading data with total amount of artciles pr. year
    py_total <- read_csv("http://labs.statsbiblioteket.dk/labsapi/api/aviser/query/facet?query=%2A&field=py&sort=count&limit=400&format=CSV", skip = 1, col_names = c("py", "total"))
# Rounding function:
    round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
    output$pyplot <- renderPlot({
        read_csv(paste0('http://labs.statsbiblioteket.dk/labsapi/api/aviser/query/facet?query=', url_encode(input$pytext), '&field=py&sort=index&limit=400&format=CSV')) %>%
            left_join(py_total) %>% 
            mutate(freq = (count / total)*100) %>% 
            filter(py >= input$pyslider[1], py <= input$pyslider[2]) %>% 
            ggplot(aes(x = py, y = freq))+
            geom_col(fill = "#E6A0C4") +
            coord_flip()+
            scale_x_continuous(breaks = 
                                   seq(round_any(input$pyslider[1], 10),
                                       round_any(input$pyslider[2], 10),
                                       10))+
            labs(title = paste0('Yearly percentage of articles mentioning ', input$pytext), 
                 subtitle = paste0('Showing years from ', input$pyslider[1], ' to ', input$pyslider[2]),
                 x = "Year",
                 y = "Percentage")
    }, res = 96) 
}

# Run the application 
shinyApp(ui = ui, server = server)
