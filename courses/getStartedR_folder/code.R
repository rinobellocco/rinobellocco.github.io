## code used in workshop 'get started with R'
## 2016-10-13


## Part 1: Get familiar with R (in RStudio)
## -----------------------------------------------------------------------------

## This is a comment

## R as hand calculator
2*150/25
exp(1)

log(2)
log(3)
log(4)

## create a vector
x <- c(2, 3, 4)
# print the vector to the console (i.e. display)
x

xlog <- log(x)
# how can I access a single element of a vector
xlog[2]
# first two elements of xlog
xlog[c(1, 3)]


## create a matrix by binding two (rows) vectors
y <- c(5, 6, 9)
A <- rbind(x, y)
A
## change (row)names to matrix A
rownames(A) <- c("x1", "x2")
A

# get a list of the objects that I've created so far
ls()


# and a matrix instead?
A[2, 3]


## Part 2: Read different data format in R
## -----------------------------------------------------------------------------

# load marathon data from R (.Rdata) format
load(url("http://alecri.github.io/downloads/data/marathon.Rdata"))
# check 'marathon' in the working environment
ls()

# removing an element
rm("A")
ls()

## how can I read a data in other format: txt, csv, dat, sav, sas7bdat
## from internet
mynewdata <- read.table("http://alecri.github.io/downloads/data/marathon.txt")
mydata_csv <- read.csv("http://alecri.github.io/downloads/data/marathon.csv")

## From a local path
## Windows user: remember to change '\' either with '\\' or '/'
#read.table("/Users/alecri/Dropbox/data/marathon.txt")

## how to install a pkg
install.packages("haven")

## load a package
library(haven)
library(foreign)
data_dta <- read_dta("http://alecri.github.io/downloads/data/marathon.dta")
data_sav <- read_sav("http://alecri.github.io/downloads/data/marathon.sav")
data_sas <- read_sas("http://alecri.github.io/downloads/data/marathon.sas7bdat")

## remove all the elements created so far
rm(list = ls())


## Part 3: Manipulate manage data
## -----------------------------------------------------------------------------

## re-load data
load(url("http://alecri.github.io/downloads/data/marathon.Rdata"))
ls()

library(tidyverse)
## first lines of my data
head(marathon)
## glimpse of my data
glimpse(marathon)
## get a structure of a data
str(marathon)

## how many lines (observations)
nrow(marathon)
## how many columns/variable
ncol(marathon)
names(marathon)

## display a variable of a dataset in the console
marathon$id

## select only some columns
sub_mar <- select(marathon, id, female, age, na, runtime, bmi)
## how can you filter a dataset
sub_female <- filter(sub_mar, female == "female", age > 30)
## create a variable
#marathon$runtimeh <- marathon$runtime/60
sub_female <- mutate(sub_female, runtimeh = runtime/60)
## removing variable
select(sub_female, -runtimeh)
#marathon$runtimeh <- NULL
## how can we arrange (sort) a data set
arrange(sub_female, age, desc(na))

marathon %>%
  ## create a new variable
  mutate(runtimeh = runtime/60) %>%
  ## select onyl some variables
  select(id, female, age, bmi, na, runtimeh) %>%
  ## filter female with age > 30
  filter(female == "female", age >= 30) %>%
  ## arrange by runtimeh
  arrange(runtimeh, desc(na))


## Part 4: Explore and obtain summary statistics +
## Part 5: Produce common useful graphs
## -----------------------------------------------------------------------------

## let's describe a quantitative variable: na
marathon$na
summary(marathon$na)
## how to get standard deviation
sd(marathon$na)
## how can I get quantiles
quantile(marathon$na, c(.3, .6))

## some summarize not in the summary function
c(n = length(marathon$na), mean = mean(marathon$na))
sum_na <- with(marathon, c(n = length(na), mean = mean(na), sd = sd(na), 
                 median = median(na)))
## round with no digits after the comma
round(sum_na, 0)


## let's learn how to do graphs

# scatterplot of wtidiff and na
with(marathon, plot(na ~ wtdiff))
ggplot(marathon) +
  geom_point(aes(x = wtdiff, y = na))

## customize the plot
p <- ggplot(marathon, aes(x = wtdiff, y = na)) +
  geom_point()
p + geom_smooth(se = F)

## plot the risk of hyponatremia as a function of wtdiff
ggplot(marathon, aes(x = wtdiff, y = as.numeric(nas135) - 1, col = female)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Risk of hyponatremia")

# plot a single continuous variable
p_hist <- ggplot(marathon, aes(x = na, y = ..density..)) +
  geom_histogram() + geom_density()
p_hist

## adding a vertical line
#hist(marathon$na)
p_hist + geom_density() +
  geom_vline(xintercept = 135, lty = "dashed")

## how to access the data build by ggplot function
example <- ggplot(marathon, aes(x = na)) +
  geom_histogram()
ggplot_build(example)

## histogram with density
ggplot(marathon, aes(x = na, y = ..density..)) +
  geom_histogram()

## how to get a boxplot
boxplot(marathon$na)
ggplot(marathon, aes(x = 1, y = na)) +
  geom_boxplot()


## let's focus on a binary variable
marathon$nas135
# how can we summarize a binary/dichotomous variable
summary(marathon$nas135)
# alternatevely we can ask for a table
tab <- table(marathon$nas135)
round(prop.table(tab), 2)

## table for weight change categories
prop.table(table(marathon$wtdiffc))

## how to graphically present a binary variable
ggplot(marathon, aes(x = nas135)) +
  geom_bar()

## how to modify to display percentages
ggplot_build(ggplot(marathon, aes(x = nas135)) +
               geom_bar())
ggplot(marathon, aes(x = nas135, y = ..count../sum(..count..))) +
  geom_bar() + ylab("Risk of hyponatremia") + xlab("")


## let's investigate if age is associated with risk of hyponatremia
summary(marathon$age)

# when missing values (NA) are present, we must be carefull with summary
# statistics: e.g. mean
mean(marathon$age, na.rm = T)
ggplot(marathon, aes(x = age, y = ..density.., color = nas135)) + 
  geom_line(stat = "density")

## stratified statistics
marathon %>% 
  group_by(nas135) %>%
  summarize(mean = mean(age, na.rm = T), median = median(age, na.rm = T))

## let's test if the mean age is different across levels of hyponatremia
t.test(age ~ nas135, data = marathon)
mod_lm <- lm(age ~ nas135, data = marathon)
# a summary of the model (i.e. more info)
summary(mod_lm)

## if we want to test if age is a predictor of risk of hyponatremia,
## we need to fit a logistic model
mod_glm <- glm(nas135 ~ age, data = marathon, family = "binomial")
summary(mod_glm)

## getting odds ratio from the previous model
exp(coef(mod_glm)[2])
## together with confidence intervals
exp(confint(mod_glm))

## Is the female proportion different accross levels of hyponatremia
prop.table(table(marathon$female))
tab <- with(marathon, table(female, nas135))
prop.table(tab, margin = 2)
chisq.test(tab)

#install.packages("Epi")
library(Epi)
twoby2(tab)
# I want to change the outcome
tab[, 1:2]
tab[, 2:1]
twoby2(tab[2:1, 2:1])

## let's compare the distribution of the number of previous marathons accross
## levels of hyponatremia
wilcox.test(prevmara ~ nas135, data = marathon)
marathon %>% group_by(nas135) %>%
  summarise(median = median(prevmara, na.rm = T))


## What if the outcome is continuous?
## And your predictor is continuous as well e.g.:
## Is age a predictor for levels of na
ggplot(marathon, aes(x = age, y = na)) +
  geom_point()

mod_lin <- lm(na ~ age, data = marathon)
summary(mod_lin)
cor.test(x = marathon$age, y = marathon$na)

## how can we plot the risk of hyponatremia as a function of bmi
mod_glm <- glm(nas135 ~ bmi, data = marathon, family = "binomial")
summary(mod_glm)

## let's predict the risk of hyponatremia
marathon$p_hat <- predict(mod_glm, marathon, type = "response")
p_mod <- ggplot(marathon, aes(x = bmi, y = p_hat)) +
  geom_line() + ylim(c(0, 1))
p_mod

## How to relax the assumption of linearity: a quadratic trend
mod_glm2 <- glm(nas135 ~ bmi + I(bmi^2), data = marathon, family = binomial)
summary(mod_glm2)
## test for non-linearity
anova(mod_glm, mod_glm2, test="Chisq")

## let's have the predictions from the new model
marathon$p_hat2 <- predict(mod_glm2, marathon, type = "response")
p_mod + geom_line(aes(y = p_hat2)) + ylim(c(0, .7))


## let's make some interactive plots
p_hist
p

#install.packages("plotly")
library(plotly)
ggplotly(p_hist)
ggplotly(p)