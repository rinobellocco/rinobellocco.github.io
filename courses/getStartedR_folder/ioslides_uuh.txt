## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(tidyverse)
knitr::opts_chunk$set(echo = TRUE, fig.width = 4.5, message = F, warning = F,
                      fig.align = "center", comment = "")
options(width = 120)
# purl("ioslides.Rmd")
# reduce the space between code and output
knit_hooks$set(document = function(x){ gsub("```\n+```\n", "", x) })

## ---- eval=FALSE---------------------------------------------------------
## # install the package
## install.packages("tidyverse")
## # load the package
## library(tidyverse)

## ----echo = FALSE, out.width = "100%"------------------------------------
knitr::include_graphics("data-science.png")

## ---- results= 'hide'----------------------------------------------------
orca = read.table("http://www.stats4life.se/data/oralca.txt")
orca = as_data_frame(orca)

## ---- eval=FALSE---------------------------------------------------------
## # reads a file in table format (.csv) into data.frame
## orca = read.csv("http://www.stats4life.se/data/oralca.csv")
## # read txt into a data.frame
## orca = read.table("http://www.stats4life.se/data/oralca.txt")
## # reads a delimited file (.csv and .tsc) into tibble
## library(readr)
## orca = read_csv("http://www.stats4life.se/data/oralca.csv")
## library(haven)
## # read a Stata data set into tibble
## orca = read_dta("http://www.stats4life.se/data/oralca.dta")
## # read a SPSS data set into tibble
## orca = read_sav("http://www.stats4life.se/data/oralca.sav")
## # read a SAS data set into tibble
## orca = read_sas("http://www.stats4life.se/data/oralca.sas7bdat")
## library(readxl)
## # download and read an Excel sheet into tibble
## download.file("http://www.stats4life.se/data/oralca.xlsx", destfile = "oralca.xlsx")
## orca = read_excel("oralca.xlsx")
## # load data in R format
## load(url("http://www.stats4life.se/data/orcalca.rda"))

## ------------------------------------------------------------------------
# print data in the console
orca
# dim = number of row anxd number of columns
dim(orca)
c("Number of obs:" = nrow(orca), "Number of variables:" = ncol(orca))

## ------------------------------------------------------------------------
# More in general, get the structure of the data
str(orca)
# names of the variable
names(orca)
# a quick summary of the data
summary(orca)

## ------------------------------------------------------------------------
# create an indicator variable for death in a new data-set called 'orca2'
orca2 = mutate(orca, dead = (event != "Alive"))
orca2

## ------------------------------------------------------------------------
# select only variables: age, sex, stage, time, and dead
select(orca2, age, sex, stage, time, dead)

## ------------------------------------------------------------------------
# select only male patients with diagnosed stage IV
filter(orca2, sex == "Male", stage == "IV")

## ------------------------------------------------------------------------
# order the data-set with increasing time, and descreasing age
arrange(orca2, time, desc(age))

## ---- eval = TRUE--------------------------------------------------------
orca %>% 
  mutate(dead = (event != "Alive")) %>%
  select(age, sex, stage, time, dead) %>%
  filter(sex == "Male", stage == "IV") %>%
  arrange(time, desc(age)) %>% head()

## ---- eval=FALSE---------------------------------------------------------
## ggplot(data = <DATA>) +
##   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

## ---- eval=TRUE, fig.width=9---------------------------------------------
# an example
ggplot(orca, aes(x = id, y = time)) +
  geom_linerange(aes(ymin = 0, ymax = time)) +
  geom_point(aes(color = event, shape = event)) + coord_flip()

## ---- eval=TRUE, fig.width=9---------------------------------------------
# another example
ggplot(data = orca, aes(x = age, y = time)) +
  geom_point(aes(shape = event, color = stage)) + 
  geom_smooth(se = FALSE)

## ----echo = FALSE, out.width = "100%"------------------------------------
knitr::include_graphics("ggplot.png")

## ----echo = FALSE, out.width = "100%"------------------------------------
knitr::include_graphics("visualization-geoms-2.png")

## ----echo = FALSE, out.width= "50%", fig.align='center'------------------
knitr::include_graphics("visualization-geoms-3.png")

## ------------------------------------------------------------------------
# first 6 observations
head(orca$age)

## ------------------------------------------------------------------------
# summary statistics (range, quartiles, and mean)
summary(orca$age)

## ------------------------------------------------------------------------
# save sample size, mean, median, and standard deviation in a new object
stat_age = c(n = length(orca$age), mean = mean(orca$age), 
               median = median(orca$age), sd = sd(orca$age))
stat_age

## ------------------------------------------------------------------------
ggplot(orca, aes(age)) +
  geom_histogram()

## ------------------------------------------------------------------------
ggplot(orca, aes(x = time, y = ..density..)) +
  geom_histogram(bins = 25) +
  geom_line(stat = "density")

## ------------------------------------------------------------------------
ggplot(orca, aes(x = 1, y = age)) + geom_boxplot() +
  labs(x = "", y = "Age (yr)") + theme_classic()

## ------------------------------------------------------------------------
# summary(orca$event)
tab = table(orca$event)
tab
# for proportions
round(prop.table(tab), 2)

## ------------------------------------------------------------------------
ggplot(orca, aes(event)) + geom_bar()

## ------------------------------------------------------------------------
library(scales)
ggplot(orca, aes(event)) + 
  geom_bar(aes(y = ..count../sum(..count..))) +
  labs(x = "", y = "Frequency") +
  scale_y_continuous(labels = percent)

## ------------------------------------------------------------------------
table(orca$stage)
orca %>% 
  group_by(stage) %>%
  summarize(mean = mean(age), se = sd(age))

## ------------------------------------------------------------------------
ggplot(orca, aes(x = stage, y = age)) + 
  geom_boxplot() + coord_flip()

## ------------------------------------------------------------------------
mod = lm(age ~ stage, data = orca)
summary(mod)

## ------------------------------------------------------------------------
ggplot(orca, aes(x = age, col = stage)) +
  geom_line(stat = "density")

## ------------------------------------------------------------------------
ggplot(orca, aes(x = age, group = stage)) +
  geom_line(stat = "density") +
  facet_grid(stage ~ .)

## ------------------------------------------------------------------------
# create binary variable 'dead' (0 alive 1 dead)
orca = mutate(orca, dead = factor(event != "Alive", labels = c("Alive", "Dead")))
tab2 = with(orca, table(sex, dead))
addmargins(tab2)
# get proportion (margin: 1 by row, 2 by col)
100*prop.table(tab2, margin = 2)
# Test the association
chisq.test(tab2)

## ------------------------------------------------------------------------
library(Epi)
stat.table(list(sex, dead), 
           list(count(), percent(sex)), 
           orca, margin = T)

## ------------------------------------------------------------------------
with(orca, twoby2(sex, relevel(dead, 2)))

## ---- eval=FALSE---------------------------------------------------------
## library(epitools)
## with(orca, epitab(table(dead, sex), verbose = T))

## ------------------------------------------------------------------------
ggplot(orca, aes(x = dead, group = sex)) + 
  geom_bar(aes(y = ..prop.., fill = sex), 
           position = "dodge") + 
  scale_y_continuous(labels = percent)

## ------------------------------------------------------------------------
ggplot(orca, aes(x = dead, group = sex)) + 
  geom_bar(aes(y = ..prop..), position = "dodge") +
  facet_grid(~ sex) +
  scale_y_continuous(labels = percent)

## ------------------------------------------------------------------------
mod_bin = glm(dead ~ relevel(sex, 2), data = orca, family = "binomial")
# presenting odds ratio
ci.exp(mod_bin)

## ------------------------------------------------------------------------
# predicted probabilities
orca %>%
  mutate(prob = predict(mod_bin, type = "response")) %>%
  head()

## ------------------------------------------------------------------------
orca %>%
  group_by(sex, dead) %>%
  summarise(mean = mean(age), sd = sd(age))

## ------------------------------------------------------------------------
stat.table(index = list(sex, dead),
           contents = list(mean(age), sd(age)),
           data = orca, margins = T)

## ---- fig.width=8--------------------------------------------------------
ggplot(orca, aes(x = age, col = dead)) + 
  geom_line(stat = "density") +
  facet_grid(~ sex)

## ---- fig.height=4-------------------------------------------------------
library(plotly)
ggplotly(
  ggplot(orca, aes(x = id, y = time)) + geom_linerange(aes(ymin = 0, ymax = time)) +
    geom_point(aes(shape = event, color = event)) + scale_shape_manual(values = c(1, 3, 4)) +
    labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic()
)

## ------------------------------------------------------------------------
library(survival)
su_obj = Surv(orca$time, orca$dead == "Dead")
head(su_obj, n = 20)

## ------------------------------------------------------------------------
fit_km = survfit(su_obj ~ 1, data = orca)
print(fit_km, print.rmean = TRUE)

## ------------------------------------------------------------------------
library(ggfortify)
fortify(fit_km) %>% head()

## ------------------------------------------------------------------------
library(survminer)
ggsurvplot(fit_km, risk.table = TRUE, fun = "pct", censor = T, 
           xlab = "Time (years)", palette = "black", legend = "none")

## ------------------------------------------------------------------------
mc = data.frame(q = c(.25, .5, .75), km = quantile(fit_km))
mc

## ------------------------------------------------------------------------
ggsurvplot(fit_km, xlab = "Time (years)", censor = F, legend = "none")$plot +
  geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, 
                              yend = 0), lty = 2) +
  geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2)

## ------------------------------------------------------------------------
library(flexsurv)
fit_wei = flexsurvreg(su_obj ~ 1, data = orca, dist = "weibull")
fit_wei

## ------------------------------------------------------------------------
data.frame(summary(fit_wei)) %>% head()

## ---- fig.width=10-------------------------------------------------------
library(gridExtra)
grid.arrange(
  ggplot(data.frame(summary(fit_wei)), aes(time, est)) + 
    geom_line() + labs(x = "Time (years)", y = "Survival"),
  ggplot(data.frame(summary(fit_wei, type = "hazard")), aes(time, est)) + 
    geom_line() + labs(x = "Time (years)", y = "Hazard"), ncol = 2
)

## ------------------------------------------------------------------------
su_stg  = survfit(su_obj ~ stage, data = orca)
su_stg

## ------------------------------------------------------------------------
survdiff(su_obj ~ stage, data = orca)

## ------------------------------------------------------------------------
ggsurvplot(su_stg, fun = "event", censor = F, xlab = "Time (years)")

## ------------------------------------------------------------------------
m1 = coxph(su_obj ~ sex + I((age-65)/10) + stage, data = orca)
ci.exp(m1)
# check proportionallity of the hazard
cox.zph(m1)

## ------------------------------------------------------------------------
newd = data.frame(sex = "Male", age = 40, stage = "IV")
fortify(survfit(m1, newdata = newd)) %>%
  ggplot(aes(x = time, y = surv)) +
  geom_step() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
  labs(x = "Time (years)", y = "Survival probability") + theme_classic()

## ---- eval=FALSE---------------------------------------------------------
## library(shiny)
## shinyApp(
##   ui = fluidPage(
##    titlePanel("Prediction from Cox model"),
##    sidebarLayout(
##       sidebarPanel(
##         sliderInput("age", "Select age:", min = 50, max = 75, value = 60),
##         selectInput("stage", "Select tumoral stage:", levels(orca$stage), "IV"),
##         radioButtons("sex", "Select sex:", levels(orca$sex), "Male")
##       ),
##       mainPanel(
##         plotOutput("surv")
##       )
##    )),
##   server = function(input, output){
##    output$surv = renderPlot({
##      newd = reactive({
##        data.frame(sex = input$sex, age = input$age, stage = input$stage)
##      })
##      fortify(survfit(m1, newdata = newd())) %>%
##        ggplot(aes(x = time, y = surv)) +
##        geom_step() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
##        labs(x = "Time (years)", y = "Survival probability") +
##        theme_classic()
##    })
##    }
## )

## ------------------------------------------------------------------------
m2 = flexsurvreg(su_obj ~ sex + I((age-65)/10) + stage, data = orca, dist = "weibull")
m2

## ------------------------------------------------------------------------
newd = data_frame(sex = "Male", age = 40, stage = "IV")
data.frame(unname(summary(m2, newdata = newd))) %>%
  ggplot(aes(x = time, y = est)) +
  geom_step() + geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = .2) +
  labs(x = "Time (years)", y = "Survival probability") + 
  theme_classic()

