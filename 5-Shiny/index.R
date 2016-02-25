## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8,
  fig.align = "center",
  cache = FALSE
)

## ------------------------------------------------------------------------
library(rvest)
src <- read_html("http://en.wikipedia.org/wiki/Table_(information)")
node <- html_node(src, css = ".wikitable")

## ------------------------------------------------------------------------
html_table(node)

## ------------------------------------------------------------------------
read_html("http://en.wikipedia.org/wiki/Table_(information)") %>%
  html_node(".wikitable") %>% html_table()

## ------------------------------------------------------------------------
domain <- "http://www.sec.gov"
susp <- paste0(domain, "/litigation/suspensions.shtml")
hrefs <- read_html(susp) %>% html_nodes("p+ table a") %>% html_attr(name = "href")
tail(hrefs)

## ---- eval = FALSE-------------------------------------------------------
## # download all the pdfs!
## hrefs <- hrefs[!is.na(hrefs)]
## pdfs <- paste0(domain, hrefs)
## mapply(download.file, pdfs, basename(pdfs))

## ---- error = TRUE-------------------------------------------------------
read_html("http://bl.ocks.org/cpsievert/raw/2a9fb8f504cd56e9e8e3/") %>%
  html_node("table")

## ------------------------------------------------------------------------
read_html("http://bl.ocks.org/cpsievert/raw/2a9fb8f504cd56e9e8e3/") %>%
  html_node("body") %>% as.character() %>% cat()

## ---- eval = FALSE-------------------------------------------------------
## library(rdom)
## rdom("http://bl.ocks.org/cpsievert/raw/2a9fb8f504cd56e9e8e3/") %>%
##   html_node("table") %>% html_table()

## ---- eval = FALSE-------------------------------------------------------
## rdom("http://www.techstars.com/companies/stats/", "table") %>%
##   html_table()

## ------------------------------------------------------------------------
library(httr)
response <- GET("https://api.github.com/users/hadley")
content(response)[c("name", "company")]

## ------------------------------------------------------------------------
response$header[1:3]

## ---- eval = FALSE-------------------------------------------------------
## library(XML2R)
## obs <- XML2Obs("https://gist.githubusercontent.com/cpsievert/85e340814cb855a60dc4/raw/651b7626e34751c7485cff2d7ea3ea66413609b8/mariokart.xml")
## table(names(obs))

## ---- echo = FALSE-------------------------------------------------------
library(XML2R)
obs <- XML2Obs("https://gist.githubusercontent.com/cpsievert/85e340814cb855a60dc4/raw/651b7626e34751c7485cff2d7ea3ea66413609b8/mariokart.xml", quiet = TRUE)
obs <- lapply(obs, function(x) x[, !colnames(x) %in% "url", drop = FALSE])
table(names(obs))

## ------------------------------------------------------------------------
obs 

## ------------------------------------------------------------------------
collapse_obs(obs) # group into table(s) by observational name/unit

## ------------------------------------------------------------------------
obs <- add_key(obs, parent = "mariokart//driver", recycle = "name")
collapse_obs(obs)

## ------------------------------------------------------------------------
tabs <- collapse_obs(obs)
merge(tabs[[1]], tabs[[2]], by = "name")

## ------------------------------------------------------------------------
library(jsonlite)
mario <- fromJSON("http://bit.ly/mario-json")
str(mario) # nested data.frames?!? 

## ------------------------------------------------------------------------
mario$driver
mario$vehicles

## ------------------------------------------------------------------------
# this mapply statement is essentially equivalent to add_key
vehicles <- Map(function(x, y) cbind(x, driver = y), 
                   mario$vehicles, mario$driver)
Reduce(rbind, vehicles)
mario[!grepl("vehicle", names(mario))]

## ---- eval = FALSE-------------------------------------------------------
## # install dependencies and run first example (press ESC to quit)
## if (!require("shiny")) install.packages("shiny")
## if (!require("leaflet")) install.packages("leaflet")
## runGitHub("rstudio/shiny-examples", subdir = "063-superzip-example")

## ---- eval = FALSE-------------------------------------------------------
## library(shiny)
## library(ggplot2)
## ui <- fluidPage(
##   numericInput(
##     inputId = "size",
##     label = "Choose a point size",
##     value = 3, min = 1, max = 10
##   ),
##   plotOutput("plotId")
## )
## server <- function(input, output) {
##   output$plotId <- renderPlot({
##     ggplot(mtcars, aes(wt, mpg)) +
##       geom_point(size = input$size)
##   })
## }
## shinyApp(ui, server)

## ---- eval = FALSE-------------------------------------------------------
## ui <- fluidPage(
##   sidebarPanel(
##     selectInput(
##       inputId = "x", label = "Choose an x variable", choices = names(mtcars)
##     ),
##     selectInput(
##       inputId = "y", label = "Choose an y variable", choices = names(mtcars)
##     )
##   ),
##   mainPanel(
##     plotOutput("plotId")
##   )
## )
## server <- function(input, output) {
##   output$plotId <- renderPlot({
##     ggplot(mtcars, aes_string(input$x, input$y)) +
##       geom_point()
##   })
## }
## shinyApp(ui, server)

