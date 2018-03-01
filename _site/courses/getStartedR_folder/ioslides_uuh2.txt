# this is a comment
# use R as a calculator

3^2
#install.packages("tidyverse")
library(tidyverse)
library(survival)

# read data from a file on your computer (put the path within quotes)
# NB: for windows user use either "\\" or "/" instead of "\"
#orca = read.table("/Users/alessiocrippa/Dropbox/oralca.txt")
orca = read.table("http://www.stats4life.se/data/oralca.txt")

# How many people were still alive at the end of the follow-up?
# How many died because of oral cancer?
orca$event
tab = table(orca$event)
# what about percentages?
tabprop = prop.table(tab)
tabprop

# create a new variable
orca = mutate(orca, dead = event!= "Alive")
table(orca$dead)
# check if the variable was properly defined
table(orca$event, orca$dead)

# type ?name_of_the_function to visit the help page
?Surv
# create a survival object
all = Surv(orca$time, orca$dead)
all

# estimate the survival function using the KM estimator
fitkm = survfit(all ~ 1, data = orca)
fitkm
# different from the follow-up time
summary(orca$time)

#install.packages("survminer")
library(survminer)
ggsurvplot(fitkm)
ggsave("kmfigure.pdf")

#ggsave("/Users/alessiocrippa/Dropbox/kmfigure.pdf")
# know the working directory

# where R is saving/looking for files by default (i.e. in the working directory)
getwd()

# putting percentage on the y-asix
ggsurvplot(fitkm, risk.table = T, censor = F, fun = "pct",
           surv.median.line = "hv")
# or alternatively
library(scales)
ggsurvplot(fitkm)$plot +
  scale_y_continuous(labels = percent)


# long life-table
summary(fitkm)

# install.packages("ggfortify")
library(ggfortify)

datkm = fortify(fitkm)
ggplot(datkm, aes(x = time)) +
  geom_step(aes(y = surv)) +
  geom_step(aes(y = upper), linetype = "dotted") +
  geom_step(aes(y = lower), linetype = "dotted") +
  theme_classic() +
  scale_y_continuous(labels = percent) +
  labs(x = "Time, years", y = "Survival (%)")

# measures of central tendency
quantile(fitkm, 0.25)


# compare the survival curves across different levels of diagnostic stage
table(orca$stage)
fitkm2 = survfit(Surv(time, dead) ~ stage, data = orca)
fitkm2

# producing a nice figure (put conf.int = F to remove the confidence bands)
ggsurvplot(fitkm2, risk.table = T, conf.int = T)

# to test if there are differences according to tumoral stage
survdiff(Surv(time, dead) ~ stage, data = orca)

# fit a cox model for modelling the (log) hazard as a function of several covariates 
fit1 = coxph(Surv(time, dead) ~ stage + sex + age, data = orca)
fit1
summary(fit1)

#install.packages("Epi")
library(Epi)
ci.exp(fit1)

fit1 = coxph(Surv(time, dead) ~ stage + strata(sex) + I(age/10), 
             data = orca)
summary(fit1)
ci.exp(fit1)

# check the proportionality assumption
proptest = cox.zph(fit1)
proptest

# graph for checking the Schoenfeld residuals
library(survminer)
ggcoxzph(proptest)

