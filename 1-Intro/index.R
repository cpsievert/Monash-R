## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  cache = FALSE,
  fig.height = 2,
  fig.width = 5,
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE, echo = FALSE-----------------------------------------
## # devtools::install_github("metacran/crandb")
## # pkgs <- crandb::list_packages(limit = 999999)
## # length(pkgs)
## # [1] 7330

## ------------------------------------------------------------------------
c(0, 1)

## ------------------------------------------------------------------------
x <- c(0, 1)

## ------------------------------------------------------------------------
sum(x + 10)

## ------------------------------------------------------------------------
c("a", "b")[1]
c("a", "b")[c(T, F)]

## ------------------------------------------------------------------------
x <- list(
  a = 10,
  b = c(1, "2")
)
x$a
x[["a"]]
x["a"]

## ------------------------------------------------------------------------
str(x)

## ---- eval = FALSE-------------------------------------------------------
## browseVignettes("dplyr")

## ------------------------------------------------------------------------
`+`

## ------------------------------------------------------------------------
"+" <- function(x, y) "I forgot how to add"
1 + 2

## ------------------------------------------------------------------------
rm("+")

## ------------------------------------------------------------------------
data(economics, package = "ggplot2")
# data frames are essentially a list of vectors
str(economics)

## ---- fig.height = 3, fig.width = 10-------------------------------------
library(ggplot2)
p <- ggplot(economics, aes(date, unemploy / pop)) + 
  geom_line()
p

## ---- fig.show = 'hold'--------------------------------------------------
p
p + geom_smooth(method = "lm", se = F)
p + geom_smooth(method = "loess", se = F)
p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), se = F)

## ------------------------------------------------------------------------
m <- lm((unemploy / pop) ~ date, data = economics)
str(m)

## ---- fig.height = 3, fig.width = 10-------------------------------------
economics$yhat <- m$fitted.values
ggplot(economics) + 
  geom_line(aes(date, unemploy / pop)) +
  geom_line(aes(date, yhat), color = "blue")

## ------------------------------------------------------------------------
r <- ts(economics$unemploy / economics$pop)
str(r)
class(r)
ar1 <- arima(r, order = c(1, 0, 0))
class(ar1)

## ------------------------------------------------------------------------
str(predict(ar1))

## ------------------------------------------------------------------------
methods(predict)

## ------------------------------------------------------------------------
library(forecast)
summary(ar1)

## ------------------------------------------------------------------------
methods(summary)

## ------------------------------------------------------------------------
forecast(ar1, level = 95)

## ---- fig.height = 5.5, fig.width = 10-----------------------------------
plot(forecast(ar1, level = 95))

## ------------------------------------------------------------------------
# to run the your turn solution...
devtools::source_gist("62d6f5ddcdb923324e2a")

## ---- cache = FALSE------------------------------------------------------
library(plotly)
ggplotly(p)

## ---- echo = FALSE-------------------------------------------------------
data(economics)

## ------------------------------------------------------------------------
head(economics)

## ------------------------------------------------------------------------
library(tidyr)
e <- gather(economics, variable, value, -date)
head(e)

## ---- fig.height = 5.5, fig.width = 10-----------------------------------
ggplot(e, aes(date, value)) + geom_line() +
  facet_wrap(~ variable, scales = "free_y")

## ------------------------------------------------------------------------
subset(
  transform(
    economics,
    year = format(date, "%Y")
  ),
  year == 2000
)

## ---- eval = FALSE-------------------------------------------------------
## library(magrittr)
## economics %>%
##   transform(year = format(date, "%Y")) %>%
##   subset(year == 2000)

## ---- eval = FALSE-------------------------------------------------------
## # in general, this:
## f(g(x), y)
## # equal this
## x %>% g() %>% f(y)

## ------------------------------------------------------------------------
library(dplyr)
economics %>%
  mutate(year = format(date, "%Y")) %>%
  filter(year == 2000)

## ------------------------------------------------------------------------
economics %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(mpce = mean(pce))

## ------------------------------------------------------------------------
economics %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(mpce = mean(psavert))

## ------------------------------------------------------------------------
library(babynames)
head(babynames)
dim(babynames)

## ---- cache = TRUE-------------------------------------------------------
library(readr)
bb_path <- tempfile(fileext = ".csv")
write_csv(babynames, bb_path)
read_csv(bb_path)

## ---- eval = FALSE-------------------------------------------------------
## library(readxl)
## read_excel("my-spreadsheet.xls", sheet = "data")
## read_excel("my-spreadsheet.xls", sheet = 2)

## ---- eval = FALSE-------------------------------------------------------
## library(haven)
## # SAS files
## read_sas("path/to/file")
## # SPSS files
## read_por("path/to/file")
## read_sav("path/to/file")
## # Stata files
## read_dta("path/to/file")

## ---- eval = FALSE-------------------------------------------------------
## db <- src_sqlite("intro/babynames.sqlite3", create = TRUE)
## copy_to(db, babynames, temporary = FALSE)

## ---- echo = FALSE, cache = FALSE----------------------------------------
db <- src_sqlite("babynames.sqlite3")

## ------------------------------------------------------------------------
db
tbl(db, "babynames")

## ------------------------------------------------------------------------
h <- db %>% 
  tbl("babynames") %>%
  filter(name == "Hilary")

## ------------------------------------------------------------------------
class(h)
h$query
# execute SQL query and bring into R
hc <- collect(h)
class(hc)
hc

## ---- fig.height = 3, fig.width = 10, cache = FALSE, warning = FALSE-----
plot_ly(hc, x = year, y = prop, color = sex, colors = c("blue", "hotpink"))

## ------------------------------------------------------------------------
popular <- babynames %>%
  group_by(name) %>%
  summarise(N = sum(n)) %>%
  arrange(desc(N))
popular

## ------------------------------------------------------------------------
top <- top_n(popular, 1000)
topnames <- subset(babynames, name %in% top[["name"]])
topnames

## ---- eval = FALSE-------------------------------------------------------
## library(shiny)
## library(ggplot2)
## ui <- bootstrapPage(
##   selectizeInput(
##     inputId = 'name',
##     label = 'Enter a name',
##     choices = unique(topnames$name),
##     selected = "James",
##     multiple = TRUE
##   ),
##   plotOutput('plot')
## )
## server <- function(input, output) {
##   output$plot <- renderPlot({
##     dat <- subset(topnames, name %in% input$name)
##     ggplot(dat, aes(year, prop, colour = sex)) +
##       geom_line() + facet_wrap(~ name)
##   })
## }
## runApp(shinyApp(ui, server))

