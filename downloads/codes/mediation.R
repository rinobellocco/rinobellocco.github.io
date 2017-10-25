## ----packages, include=FALSE---------------------------------------------
library(tidyverse)
library(Epi)
library(xtable)
library(broom)

## ------------------------------------------------------------------------
load(url("http://alecri.github.io/downloads/data/mediation.Rda"))
tab <- dat  %>%
  mutate(diabetes = as.character(diabetes)) %>%
  group_by(diabetes) %>%
  summarise(race = sum(race == "Black-American"),
            fastfood = sum(fastfood == "yes"),
            dinp = round(mean(dinp), 2)) %>%
  rbind(c("total", sum(.$race), sum(.$fastfood), round(mean(dat$dinp), 2))) 

tab %>%
  kable(digits = 2, col.names =  c("Diabetes", "Black-Am (X), n",
                                   "Fast-food (M1), n",
                                   "DiNP (M2), mean"))

## ------------------------------------------------------------------------
fit_te <- glm(diabetes ~ race, data = dat, family = "binomial")
ci.exp(fit_te) %>%
  kable()

## ------------------------------------------------------------------------
fit_m <- lm(dinp ~ race, data = dat)
ci.lin(fit_m) %>%
  kable()

## ------------------------------------------------------------------------
fit_de <- glm(diabetes ~ race + dinp, data = dat, family = "binomial")
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

## ------------------------------------------------------------------------
fit_te2 <- lm(dinp ~ race, data = dat)
ci.lin(fit_te2)
te2 <- coef(fit_te2)[grep("race", names(coef(fit_te2)))]

## ------------------------------------------------------------------------
dat$ff <- as.numeric(dat$fastfood) - 1
fit_m1 <- lm(ff ~ race, data = dat)
#fit_p <- glm(fastfood ~ race, data = dat, family = binomial(link = "identity"))
ci.lin(fit_m1)
alpha2_x <- coef(fit_m1)[grep("race", names(coef(fit_m1)))]

## ------------------------------------------------------------------------
fit_de2 <- lm(dinp ~ race + fastfood, data = dat)
ci.lin(fit_de2)
de2 <- coef(fit_de2)[grep("race", names(coef(fit_de2)))]
beta2_m <- coef(fit_de2)[grep("fastfood", names(coef(fit_de2)))]

## ------------------------------------------------------------------------
ie2_d <- te2 - de2
ie2_p <-  alpha2_x* beta2_m
pm2 <- 100*ie2_p/te2

