library(shiny)
library(tidyverse)
library(wesanderson)
library(urltools)
library(shinyWidgets)
library(ggraph)
library(igraph)
library(tidytext)
library(ggwordcloud)

revolutionValue = "http://labs.statsbiblioteket.dk/labsapi/api/aviser/export/fields?query=revolution%20AND%20py%3A%5B1840%20TO%201880%5D&fields=link&fields=recordID&fields=timestamp&fields=pwa&fields=cer&fields=fulltext_org&fields=pageUUID&fields=editionUUID&fields=titleUUID&fields=editionId&fields=familyId&fields=newspaper_page&fields=newspaper_edition&fields=lplace&fields=location_name&fields=location_coordinates&max=1&structure=header&structure=content&format=CSV"

# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dhm_poster.css")
  ),
  
  titlePanel('Demonstration af Mediestream API'),
  tabsetPanel(
    tabPanel("URL",
             sidebarLayout(
               sidebarPanel( 
                 fluidRow(textInput("url", 
                           p("URL fra Swagger API (findes på labs.kb.dk):"), 
                           value = "", 
                           placeholder = "Indsæt URL her"),
                           actionButton("load", "Indlæs"),
                 ),
               ),
               mainPanel(
                 plotOutput("col_plot", click = "plot_click"),
                 fluidRow(
                   column(width = 4,
                          tableOutput('lplaceTable')),
                   column(width = 6,
                          plotOutput("wordcloud", click = "plot_click"), offset = 0)))
               ),
           ),
    tabPanel("Ordnetværk",
             sliderInput("n_slider", "Bagatelgrænse for antal ordforbindelser", min = 2, max = 20, value = 8), 
             plotOutput("networkgraph", click = "plot_click")
  )),
  img(src='DKB_logo_expanded_white_small_RGB.png', align = "bottomleft", width = "15%")
)

server <- function(input, output) {
  stopord <- read_csv("https://gist.githubusercontent.com/maxodsbjerg/4d1e3b1081ebba53a8d2c3aae2a1a070/raw/e1f63b4c81c15bb58a54a2f94673c97d75fe6a74/stopord_18.csv")
  stopord2 <- tibble(word = c("os", "vore", "bleven", "stal", "vel", "vor", "mig", "sine", "gjore", "sit", "hos", "skal", "skulde", "thi", "lige", "anden", "gjort", "deel"))
  data <- eventReactive(input$load, {
  data <- read_csv(paste0(input$url))
  })
  

  
  output$lplaceTable <- renderTable(
    data() %>% 
      count(lplace, sort = TRUE) %>% 
      rename(Udgivelsessted = lplace) %>% 
      top_n(10)
  )
  
  output$col_plot <- renderPlot({
    data() %>% 
      count(familyId, sort = TRUE) %>% 
      slice_max(n, n = 20) %>% 
      mutate(familyId = reorder(familyId, n)) %>% 
      ggplot(aes(x = familyId, y = n))+ 
      geom_col(fill = "#FFFFF0") +
      coord_flip() +
      labs(title = paste0('Antal artikler der matcher søgningen "', input$url %>% str_remove("&fields=.*") %>% str_remove(".*query=") %>% url_decode(), '"'), 
           subtitle = "Fordelt på avisniveau",
           x = "Avis") +
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
        axis.text.y = element_text(size=9, 
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

  slider_input <- reactive({
    input$n_slider
  }) %>% debounce(1000)
  
output$networkgraph <- renderPlot({
  data() %>% 
    unnest_tokens(bigram, fulltext_org, token = "ngrams", n = 2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stopord$word) %>%
    filter(!word2 %in% stopord$word) %>% 
    count(word1, word2, sort = TRUE) -> bigrams_count
  
  bigram_graph <- bigrams_count %>%
    filter(n > slider_input()) %>%
    graph_from_data_frame()
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "#FBFAF5", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}, res = 96, bg="transparent")  
  output$wordcloud <- renderPlot({
    data() %>%
      unnest_tokens(word, fulltext_org) %>% 
      anti_join(stopord, by = "word") %>% 
      anti_join(stopord2, by = "word") %>% 
      anti_join(stop_words, by = "word") %>% 
      count(word, sort = TRUE) %>% 
      slice_max(n, n = 20) %>% 
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud_area(rm_outside = T, area_corr_power = 1) +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient(low = "#FFFFF0", high = "#FFFFF0") +
      labs(title = paste0('Hyppigst forekommende ord i artikler, der matcher  søgningen\n"', input$url %>% str_remove("&fields=.*") %>% str_remove(".*query=") %>% url_decode(), '"'), 
           subtitle = "efter frasortering af stopord")+ 
      theme(plot.title = element_text(size=14, 
                                      family = "Arial",
                                      color="#FBFAF5", 
                                      face="bold",
                                      angle=0),
            plot.subtitle = element_text(size=12, 
                                         family = "Arial",
                                         color="#FBFAF5", 
                                         face="bold",
                                         angle=0))
  }, res = 96, bg="transparent")
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
