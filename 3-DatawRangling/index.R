## ---- echo = FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8,
  fig.align = "center",
  cache = FALSE
)
options(digits = 2)

## ---- echo = FALSE-------------------------------------------------------
library(tidyr)
library(dplyr)
library(readr)
data(french_fries, package = "reshape2")
kable(head(french_fries, 4), format = "markdown", row.names = F)

## ---- echo=FALSE, results='asis'-----------------------------------------
genes <- read_csv("http://dicook.github.io/Monash-R/data/genes.csv")
kable(genes)

## ------------------------------------------------------------------------
library(XML)
src ="http://www.realclearpolitics.com/epolls/2012/president/us/republican_presidential_nomination-1452.html"
tables <- readHTMLTable(src)
polls <- tables[[1]]
head(polls)

## ---- eval = FALSE-------------------------------------------------------
#> library(maptools)
#> xx <- readShapeSpatial("http://dicook.github.io/Monash-R/data/australia/region.shp")
#> object.size(as(xx, "SpatialPolygons"))
#> xxx <- thinnedSpatialPoly(as(xx, "SpatialPolygons"),
#>   tolerance=0.5, minarea=0.001, topologyPreserve=TRUE)
#> object.size(as(xxx, "SpatialPolygons"))
#> qplot(long, lat, data=xx, group=group) + geom_path() + coord_map()

## ---- echo=FALSE, results='asis'-----------------------------------------
kable(head(french_fries), format = "markdown", row.names = FALSE)

## ------------------------------------------------------------------------
library(tidyr)
ff_long <- gather(french_fries, key = variable, value = rating, potato:painty)
head(ff_long)

## ------------------------------------------------------------------------
french_fries_wide <- spread(ff_long, key = variable, value = rating)

head(french_fries_wide)

## ---- fig.height=2, fig.width=8------------------------------------------
library(ggplot2)
ff.m <- french_fries %>% 
  gather(type, rating, -subject, -time, -treatment, -rep)
head(ff.m)
qplot(rating, data=ff.m, binwidth=2) + 
  facet_wrap(~type, ncol=5) 

## ------------------------------------------------------------------------
qplot(type, rating, data = ff.m, fill = type, geom = "boxplot")

## ---- fig.show='hold', fig.align='default', fig.height=4, fig.width=4----
head(ff.m)
ff.s <- ff.m %>% spread(rep, rating)
head(ff.s)

## ---- fig.show='hold', fig.align='default', fig.height=3, fig.width=3----
qplot(`1`, `2`, data=ff.s) + theme(aspect.ratio=1) + 
  xlab("Rep 1") + ylab("Rep 2")
qplot(`1`, `2`, data=ff.s) + theme(aspect.ratio=1) + 
  xlab("Rep 1") + ylab("Rep 2") + 
  scale_x_log10() + scale_y_log10()

## ------------------------------------------------------------------------
billboard <- read.csv("http://dicook.github.io/Monash-R/data/billboard.csv")

## ---- echo=FALSE, fig.height=3-------------------------------------------
long_billboard <- gather(billboard, key = week, value = rank, X1:X76)
long_billboard$week <- as.numeric(gsub("X", "", long_billboard$week))

qplot(week, rank, data = long_billboard, geom = "line", colour = artist, group = track)

## ------------------------------------------------------------------------
library(dplyr)
french_fries_split <- group_by(ff_long, variable) # SPLIT
french_fries_apply <- summarise(french_fries_split, rating = mean(rating, na.rm = TRUE)) # APPLY + COMBINE
french_fries_apply

## ------------------------------------------------------------------------
french_fries %>%
    gather(key = variable, value = rating, potato:painty) %>%
    group_by(variable) %>%
    summarise(rating = mean(rating, na.rm = TRUE))

## ------------------------------------------------------------------------
french_fries %>%
    filter(subject == 3, time == 1)

## ------------------------------------------------------------------------
french_fries %>%
    arrange(desc(rancid)) %>%
    head

## ------------------------------------------------------------------------
french_fries %>%
    select(time, treatment, subject, rep, potato) %>%
    head

## ------------------------------------------------------------------------
french_fries %>%
    group_by(time, treatment) %>%
    summarise(mean_rancid = mean(rancid), sd_rancid = sd(rancid))

## ------------------------------------------------------------------------
french_fries %>% 
  select(subject, time, treatment) %>% 
  tbl_df() %>% 
  count(subject, time) %>%
  spread(time, n)

## ------------------------------------------------------------------------
french_fries %>% 
  gather(type, rating, -subject, -time, -treatment, -rep) %>%
  select(subject, time, treatment, type) %>% 
  tbl_df() %>% 
  count(subject, time) %>%
  spread(time, n)

## ------------------------------------------------------------------------
qplot(time, rating, data=ff.m, colour=treatment) + 
  facet_grid(subject~type) 

## ------------------------------------------------------------------------
ff.m.av <- ff.m %>% 
  group_by(subject, time, type, treatment) %>%
  summarise(rating=mean(rating))
qplot(time, rating, data=ff.m, colour=treatment) + 
  facet_grid(subject~type) +
  geom_line(data=ff.m.av, aes(group=treatment))

## ------------------------------------------------------------------------
library(nycflights13)
flights

## ---- results='hold'-----------------------------------------------------
library(lubridate)

now()
today()
now() + hours(4)
today() - days(2)

## ---- results='hold'-----------------------------------------------------
ymd("2013-05-14")
mdy("05/14/2013")
dmy("14052013")
ymd_hms("2013:05:14 14:50:30", tz = "America/Chicago")

## ---- echo = FALSE, fig.height = 3---------------------------------------
flights$date <- ymd(paste(flights$year, flights$month, flights$day, sep = "-"))
delay.dat <- flights %>% group_by(date) %>% summarise(dep_delay = mean(dep_delay, na.rm = TRUE))

qplot(date, dep_delay, geom = "line", data = delay.dat)

## ---- echo=FALSE, results='asis'-----------------------------------------
genes <- read_csv("http://dicook.github.io/Monash-R/data/genes.csv")
knitr::kable(genes)

## ---- echo=FALSE, results='asis'-----------------------------------------
genes.m <- gather(genes, treatment, expr, -id)
knitr::kable(head(genes.m))

## ------------------------------------------------------------------------
genes.m %>%
  separate(treatment, c("trt", "leftover"), "-")

## ------------------------------------------------------------------------
genes.m2 <- genes.m %>%
  separate(treatment, c("trt", "leftover"), "-") %>%
  separate(leftover, c("time", "rep"), "\\.") %>%
  mutate(trt = sub("W", "", trt))
genes.m2

## ---- fig.height=3-------------------------------------------------------
genes.m.av <- genes.m2 %>% 
  group_by(id, trt, time) %>% 
  summarise(expr = mean(expr))
qplot(trt, expr, data = genes.m2, colour = time) + 
  xlab("Type of modification") + ylab("Expression") + 
  facet_wrap(~id) +
  geom_line(data = genes.m.av, aes(group = time))

