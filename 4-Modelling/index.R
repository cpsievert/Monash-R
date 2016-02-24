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
load("data/realestate.rda")
str(realestate)

## ------------------------------------------------------------------------
lm(price ~ sqft, data=realestate)

## ---- echo=FALSE---------------------------------------------------------
library(ggplot2)
qplot(sqft, price, data=realestate) + geom_smooth(method="lm") + theme(aspect.ratio=1)

## ---- echo=FALSE---------------------------------------------------------
qplot(sqft, log(price), data=realestate) + geom_smooth(method="lm") + theme(aspect.ratio=1)

## ------------------------------------------------------------------------
lm(log(price)~sqft, data=realestate)

## ------------------------------------------------------------------------
m1 <- lm(log(price)~sqft, data=realestate)
summary(m1)

## ------------------------------------------------------------------------
m2 <- update(m1, .~.+ac)

## ------------------------------------------------------------------------
summary(m2)

## ------------------------------------------------------------------------
options()$contrasts

## ---- eval=FALSE---------------------------------------------------------
## ?contr.treatment

## ------------------------------------------------------------------------
anova(m1, m2)

## ------------------------------------------------------------------------
qplot(sqft, log(price), colour=ac, data=realestate) + geom_smooth(method="lm")

## ------------------------------------------------------------------------
m3 <- lm(log(price) ~ poly(sqft, 2) + ac, data=realestate)
anova(m2, m3)

## ------------------------------------------------------------------------
realestate$fitted.m3 <- fitted(m3)
ggplot(data=realestate, aes(y=log(price), x=sqft, colour=ac)) +
  geom_point() +
  geom_line(aes(y=fitted.m3))

## ---- echo=FALSE, results='hide', fig.show='hide'------------------------
m4 <- update(m3, .~.+quality+log(lot))
anova(m3, m4)
realestate$fitted.m4 <- fitted(m4)

ggplot(data=realestate, aes(y=log(price), x=sqft, colour=ac)) +
  geom_point() +
  geom_line(aes(y=fitted.m4)) + 
  facet_wrap(~quality)

# pretty hard to visualize the second continuous variable ... idea, make it discrete
# 
realestate$lot.discrete <- cut(realestate$lot, breaks=c(0,35000, 100000), labels=c("large", "huge"))
ggplot(data=realestate, aes(y=log(price), x=lot, colour=ac)) +
  geom_point() +
  geom_line(aes(y=fitted.m4)) + 
  facet_wrap(lot.discrete~quality)


qplot(bed, log(price), data=realestate, geom="boxplot", group=bed)
qplot(bed, log(price), data=realestate, geom="boxplot", group=bed) + facet_wrap(~quality)

m4b <- update(m4, .~.+bed+I(bed==0) + built)

## ------------------------------------------------------------------------
m5 <- lm(log(price) ~ sqft + bath + built + log(lot), data = realestate)

## ------------------------------------------------------------------------
new_house <- data.frame(sqft = 2123, bath = 2, built = 1963, lot = 20590)

predict(m5, newdata = new_house, interval = "prediction", level = 0.95) # log(price)!
exp(predict(m5, newdata = new_house, interval = "prediction", level = 0.95))/1000

## ------------------------------------------------------------------------
m5 <- lm(log(price) ~ sqft + bath + built + log(lot), data = realestate)

## ------------------------------------------------------------------------
realestate$resid.m5 <- residuals(m5)
realestate$fitted.m5 <- fitted(m5)
ggplot(aes(sample = resid.m5), data=realestate) + geom_qq() + theme(aspect.ratio=1)

## ------------------------------------------------------------------------
m5b <- lm(price ~ sqft + bath + built + log(lot), data = realestate)

realestate$resid.m5b <- residuals(m5b)
ggplot(aes(sample = resid.m5b), data=realestate) + geom_qq() + theme(aspect.ratio=1)

## ------------------------------------------------------------------------
qplot(data=realestate, x=fitted.m5, y=resid.m5) + xlab("Fitted Values") + ylab("Residuals") + 
  geom_hline(yintercept=0) + geom_smooth()

## ------------------------------------------------------------------------
library(lmtest)
bptest(m5)

## ------------------------------------------------------------------------
library(MASS)
bc <- boxcox(m5b)

## ------------------------------------------------------------------------
str(bc)
lambda <- bc$x[which.max(bc$y)]

## ------------------------------------------------------------------------
m5c <- lm(price^lambda ~ sqft + bath + built + log(lot), data=realestate)
bptest(m5c) # better but still not non-significant

## ------------------------------------------------------------------------
qplot(fitted(m5c), residuals(m5c)) + geom_hline(yintercept=0) + geom_smooth()

## ------------------------------------------------------------------------
library(gapminder)
qplot(data=gapminder, year, lifeExp, geom="line", group=country)


## ---- echo=FALSE---------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

gapminder2 <- gapminder %>% mutate(year = year-1950)
by_country <- gapminder2 %>% 
  group_by(continent, country) %>% 
  nest()

by_country <- by_country %>% mutate(
    model = purrr::map(data, ~ lm(lifeExp ~ year, data = .))
)

by_country <- by_country %>% unnest(model %>% purrr::map(broom::tidy))
country_coefs <- by_country %>% select(continent, country, term, estimate) %>% spread(term, estimate)

qplot(`(Intercept)`, year, data=country_coefs, colour=continent) + scale_colour_brewer(palette="Dark2") + 
  xlab("Average Life Expectancy in 1950") +
  ylab("Average Gain in Life Expectancy per Year")

## ---- eval=FALSE---------------------------------------------------------
## install.packages("tidyr")
## install.packages("purrr")
## 
## library(tidyr)
## library(purrr)

## ------------------------------------------------------------------------
oz <- subset(gapminder, country=="Australia")
head(oz)

## ------------------------------------------------------------------------
qplot(data=oz, year, lifeExp) + geom_smooth(method="lm")

## ------------------------------------------------------------------------
oz.lm <- lm(lifeExp~year, data=oz)
oz.lm

## ------------------------------------------------------------------------
gapminder <- gapminder %>% mutate(year = year-1950)

## ------------------------------------------------------------------------
by_country <- gapminder %>% group_by(continent, country) %>% nest()
head(by_country)

## ------------------------------------------------------------------------
head(by_country$data[[1]])
lm(lifeExp~year, data=by_country$data[[1]])

## ------------------------------------------------------------------------
by_country$model <- by_country$data %>% purrr::map(~lm(lifeExp~year, data=.))
head(by_country)

## ------------------------------------------------------------------------
library(broom)
broom::glance(by_country$model[[1]])
broom::tidy(by_country$model[[1]])

## ------------------------------------------------------------------------
broom::augment(by_country$model[[1]])

## ------------------------------------------------------------------------
by_country_coefs <- by_country %>% unnest(model %>% purrr::map(broom::tidy))
coefs <- by_country_coefs %>% select(country, continent, term, estimate) %>% spread(term, estimate)

## ------------------------------------------------------------------------
qplot(`(Intercept)`, year, colour=continent, data=coefs)

## ------------------------------------------------------------------------
by_country <- by_country %>% unnest(model %>% purrr::map(broom::glance))
by_country$country <- reorder(by_country$country, by_country$r.squared)
qplot(country, r.squared, colour=continent, data=by_country) +
  theme(axis.text.x=element_text(angle=-90, hjust=0)) +
  scale_colour_brewer(palette="Dark2")

## ------------------------------------------------------------------------
country_all <- by_country %>% unnest(data)
qplot(year+1950, lifeExp,  data=subset(country_all, r.squared <= 0.45), geom="line") +
  facet_wrap(~country)

## ------------------------------------------------------------------------
qplot(year+1950, lifeExp,  data=subset(country_all, between(r.squared, 0.45, 0.75)), geom="line") +
  facet_wrap(~country)

