
## ----packages, include=FALSE---------------------------------------------
library(tidyverse)
library(knitr)
library(Epi)
library(xtable)
library(broom)
library(mediation)
library(boot)

## ------------------------------------------------------------------------
load(url("http://alecri.github.io/downloads/data/mediation.Rdata"))
#load("../data/dat.Rda")
tab <- mediation  %>%
  mutate(diabetes = as.character(diabetes)) %>%
  group_by(diabetes) %>%
  summarise(race = sum(race == "Black-American"),
            fastfood = sum(fastfood == "yes"),
            dinp = round(mean(dinp), 2)) %>%
  rbind(c("total", sum(.$race), sum(.$fastfood), round(mean(mediation$dinp), 2))) 

tab %>%
  kable(digits = 2, col.names =  c("Diabetes", "Black-Am (X), n",
                                   "Fast-food (M1), n",
                                   "DiNP (M2), mean"))

## ---- results='hold'-----------------------------------------------------
tab <- with(mediation, table(race, diabetes))
prop.table(tab, margin = 1)

## ---- fig.width=3, fig.height=2------------------------------------------
mediation %>%
  group_by(race, diabetes) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = diabetes, y = prop, fill = race)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() + guides(fill = FALSE)

## ---- fig.width=3, fig.height=2------------------------------------------
mediation %>%
  group_by(diabetes) %>%
  do(tidy(summary(.$dinp))) %>%
  kable()

ggplot(mediation, aes(x = diabetes, y = dinp)) +
  geom_boxplot() +
  theme_classic()

## ---- fig.width=3, fig.height=2------------------------------------------
mediation %>%
  group_by(race) %>%
  do(tidy(summary(.$dinp))) %>%
  kable()

ggplot(mediation, aes(x = race, y = dinp)) +
  geom_boxplot() +
  theme_classic()

## ------------------------------------------------------------------------
fit_te <- glm(diabetes ~ race, data = mediation, family = "binomial")
ci.exp(fit_te) %>%
  kable()

## ------------------------------------------------------------------------
fit_m <- lm(dinp ~ race, data = mediation)
ci.lin(fit_m) %>%
  kable()

## ------------------------------------------------------------------------
fit_de <- glm(diabetes ~ race + dinp, data = mediation, family = "binomial")
ci.exp(fit_de) %>%
  kable()

## ------------------------------------------------------------------------
ax <- coef(fit_de)["dinp"]
bx <- coef(fit_m)[grep("race", names(coef(fit_m)))]
log_ie_p <-  ax*bx
ie_p <- exp(log_ie_p)

## ------------------------------------------------------------------------
log_te <- coef(fit_te)[grep("race", names(coef(fit_te)))]
log_de <- coef(fit_de)[grep("race", names(coef(fit_de)))]
log_ie_d <- log_te - log_de
ie_d <- exp(log_ie_d)
pm <- 100*log_ie_d/log_te

## ---- fig.width=3, fig.height=2------------------------------------------
mediation %>%
  group_by(fastfood) %>%
  do(tidy(summary(.$dinp))) %>%
  kable()

ggplot(mediation, aes(x = fastfood, y = dinp)) +
  geom_boxplot() +
  theme_classic()

## ---- results='hold'-----------------------------------------------------
tab2 <- with(mediation, table(race, fastfood))
prop.table(tab2, margin = 1)

## ---- fig.width=3, fig.height=2------------------------------------------
mediation %>%
  group_by(race, fastfood) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = fastfood, y = prop, fill = race)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() + guides(fill = FALSE)

## ------------------------------------------------------------------------
fit_te2 <- lm(dinp ~ race, data = mediation)
ci.lin(fit_te2)
te2 <- coef(fit_te2)[grep("race", names(coef(fit_te2)))]

## ------------------------------------------------------------------------
mediation$ff <- as.numeric(mediation$fastfood) - 1
fit_m1 <- lm(ff ~ race, data = mediation)
#fit_p <- glm(fastfood ~ race, data = mediation, family = binomial(link = "identity"))
ci.lin(fit_m1)
alpha2_x <- coef(fit_m1)[grep("race", names(coef(fit_m1)))]

## ------------------------------------------------------------------------
fit_de2 <- lm(dinp ~ race + fastfood, data = mediation)
ci.lin(fit_de2)
de2 <- coef(fit_de2)[grep("race", names(coef(fit_de2)))]
beta2_m <- coef(fit_de2)[grep("fastfood", names(coef(fit_de2)))]

## ------------------------------------------------------------------------
ie2_d <- te2 - de2
ie2_p <-  alpha2_x* beta2_m
pm2 <- 100*ie2_p/te2

## ---- eval = F, echo=T---------------------------------------------------
ind_eff <- function(data, indices) {
  d <- mediation[indices, ]
  fit_te2 <- lm(dinp ~ race, data = d)
  fit_de2 <- lm(dinp ~ race + fastfood, data = d)
  return(coef(fit_te2)[2] - coef(fit_de2)[2])
}
results <- boot(data = mediation, ind_eff, R = 1000)
boot.ci(results, type = "norm")


## ---- fig.width=4, fig.height=3------------------------------------------
ggplot(NULL, aes(x = c(results$t))) +
  geom_histogram(aes(y = ..density..), bins = 17, alpha = .5) +
  geom_line(stat = "density") + 
  theme_classic() + labs(x = "Sample ie")

## ---- eval=FALSE, echo=T-------------------------------------------------
fit_xm1 <- lm(ff ~ race, data = mediation)
fit_m1m2 <- lm(dinp ~ race + ff, data = mediation)
med_out <- mediate(fit_xm1, fit_m1m2, treat = "race", mediator = "ff",
                   robustSE = TRUE, sims = 1000)
summary(med_out)
