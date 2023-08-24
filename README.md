# Newspaper API  Shiny
[Shiny](https://shiny.posit.co) web app for demonstrating text and data mining potential of the [kb-labs-api](http://labs.statsbiblioteket.dk/labsapi/api//api-docs?url=/labsapi/api/openapi.yaml)-endpoint that exports newspaper data as raw data(/aviser/export/fields). 

# What it does 

The web app only needed input is a request url from the /aviser/export/fields-endpoint, which is based on queries to [Mediestream](https://www2.statsbiblioteket.dk/mediestream/avis/search/cykel%20AND%20lplace%3AKøbenhavn). For more info on constructing queries and request urls visit expand the /aviser/export/fields-panel on the [kb-labs-api](http://labs.statsbiblioteket.dk/labsapi/api//api-docs?url=/labsapi/api/openapi.yaml)-page.  

Based on this request url with the raw data of the query the web app creates a column diagram of the dispersion of the results on newspaper, a table of publication place and a word cloud of the most frequent words in the articles (without stopwords(Danish + English) being counted). Lastly it also does network graph based on the most frequent word to word connections. 

# Requirements
* R

# Running the web app
If you're not familiar with cloning gituhb repositories can be ran from R with the following command: 

```
install.packages(c("shiny", "tidyverse", "wesanderson", "urltools", "shinyWidgets", "ggraph", "igraph", "tidytext", "ggwordcloud"))
shiny::runGitHub('NewspaperAPIShiny', 'maxodsbjerg', subdir = 'URLDemonstrationAPP/')
```

