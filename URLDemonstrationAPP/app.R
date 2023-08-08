library(shiny)
library(tidyverse)
library(wesanderson)
library(urltools)
library(shinyWidgets)

revolutionValue = "http://labs.statsbiblioteket.dk/labsapi/api/aviser/export/fields?query=revolution%20AND%20py%3A%5B1840%20TO%201880%5D&fields=link&fields=recordID&fields=timestamp&fields=pwa&fields=cer&fields=fulltext_org&fields=pageUUID&fields=editionUUID&fields=titleUUID&fields=editionId&fields=familyId&fields=newspaper_page&fields=newspaper_edition&fields=lplace&fields=location_name&fields=location_coordinates&max=1&structure=header&structure=content&format=CSV"

# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dhm_poster.css")
  ),
  
  titlePanel('Showcase of Mediestream API'),
  tabsetPanel(
    tabPanel("URL",
             sidebarLayout(
               sidebarPanel( 
                 fluidRow(textInput("url", 
                           p("Url:"), 
                           value = "", 
                           placeholder = "copy URL here"),
                           actionButton("load", "Load"),
                 ),
               ),
               mainPanel(
                 plotOutput("plot", click = "plot_click"),
                 fluidRow(
                   column(width = 6,
                          tableOutput('familyIdTable')),
                   column(width = 6,
                          tableOutput('lplaceTable'))))
               ),
           )
  ),
  img(src='DKB_logo_expanded_white_small_RGB.png', align = "bottomleft", width = "15%")
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
      geom_col(fill = "#FBFAF5") +
      coord_flip() +
      labs(title = paste0('Amount of articles matching search query "', input$url %>% str_remove("&fields=.*") %>% str_remove(".*query=") %>% url_decode(), '"'), 
           subtitle = "dispersed on familyId",
           x = "familyId") +
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.minor = element_blank(), #remove minor gridlines
        axis.title.x = element_text(size=10, 
                                          family = "Arial",
                                          color="#FBFAF5", 
                                          face="bold",
                                          angle=0),
        axis.title.y = element_text(size=10, 
                                    family = "Arial",
                                    color="#FBFAF5", 
                                    face="bold",
                                    angle=90),
        axis.text.x = element_text(size=8, 
                                    family = "Arial",
                                    color="#FBFAF5", 
                                    face="bold",
                                    angle=0),
        axis.text.y = element_text(size=8, 
                                    family = "Arial",
                                    color="#FBFAF5", 
                                    face="bold",
                                    angle=0),
        plot.title = element_text(size=14, 
                                    family = "Arial",
                                    color="#FBFAF5", 
                                    face="bold",
                                    angle=0),
        plot.subtitle = element_text(size=14, 
                                  family = "Arial",
                                  color="#FBFAF5", 
                                  face="bold",
                                  angle=0)
        )
  }, res = 96, bg="transparent")
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
