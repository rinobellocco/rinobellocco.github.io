## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(comment = NA, fig.align = "center", warning = FALSE)
options(width = 95, show.signif.stars = F)

## ----extract code (ignore), eval = FALSE, echo = FALSE-------------------
## knitr::purl("review_survival.Rmd")

## ----load packages, message=FALSE----------------------------------------
pkg <- c("knitr", "kfigr", "tidyverse", "survival", "ggfortify", "survminer", "plotly",
         "gridExtra", "Epi", "KMsurv", "gnm", "cmprsk", "mstate", "flexsurv", "splines",
         "epitools", "eha", "shiny")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)

## ----read data-----------------------------------------------------------
orca <- read.table("http://www.stats4life.se/data/oralca.txt", header = T)

## ----describe data-------------------------------------------------------
head(orca)
str(orca)
summary(orca)

## ----boxDiagram, fig.cap = "Figure 1: Box diagram for transitions.", anchor = "Figure"----
tm <- matrix(c(NA, NA, 1, NA), ncol = 2)
rownames(tm) <- colnames(tm) <- c("Alive", "Death")
tm2 <- matrix(c(NA, NA, NA, 1, NA, NA, 2, NA, NA), ncol = 3)
rownames(tm2) <- colnames(tm2) <- levels(orca$event)
par(mfrow = c(1, 2))
layout(rbind(c(1, 2, 2)))
boxes.matrix(tm, boxpos = TRUE)
title("A)")
boxes.matrix(tm2, boxpos = TRUE)
title("B)")

## ----table events--------------------------------------------------------
table(orca$event)
orca$all <- 1*(orca$event != "Alive")
table(orca$all)

## ----follow-up, fig.cap="Figure 2: Possible representations of follow-up time.", anchor = "Figure"----
ggplotly(
  orca %>%
    mutate(
      text = paste("Subject ID = ", id, "<br>", "Time = ", time, "<br>", "Event = ",  
                   event, "<br>", "Age = ", round(age, 2), "<br>", "Stage = ", stage)
    ) %>%
  ggplot(aes(x = id, y = time, text = text)) +
    geom_linerange(aes(ymin = 0, ymax = time)) +
    geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
    scale_shape_manual(values = c(1, 3, 4)) +
    labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic(),
  tooltip = "text"
)

## ----time-scales, fig.width=10-------------------------------------------
grid.arrange(
  ggplot(orca, aes(x = id, y = time)) +
  geom_linerange(aes(ymin = 0, ymax = time)) +
  geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
  scale_shape_manual(values = c(1, 3, 4)) +
  labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic(),
  orca %>%
  mutate(age_orig = age,
         age_end = age + time) %>%
  ggplot(aes(x = id, y = age_end)) +
  geom_linerange(aes(ymin = age_orig, ymax = age_end)) +
  geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
  scale_shape_manual(values = c(1, 3, 4)) + guides(fill = FALSE) +
  labs(y = "Age (years)", x = "Subject ID") + coord_flip() + theme_classic(),
  ncol = 2
)

## ----survObj-------------------------------------------------------------
su_obj <- Surv(orca$time, orca$all)
str(su_obj)

## ----KMmethod------------------------------------------------------------
fit_km <- survfit(su_obj ~ 1, data = orca)
# str(fit_km)
print(fit_km, print.rmean = TRUE)

## ----suvTable KM---------------------------------------------------------
dat_km <- fortify(fit_km)
head(dat_km)

## ----plot KM-------------------------------------------------------------
ggsurvplot(fit_km, risk.table = TRUE, xlab = "Time (years)", censor = T)

## ----alternative plots KM, fig.width = 12, results='hide'----------------
glist <- list(
  ggsurvplot(fit_km, fun = "event", main = "Cumulative proportion"),
  ggsurvplot(fit_km, fun = "cumhaz",  main = "Cumulative Hazard"),
  ggsurvplot(fit_km, fun = "cloglog", main = "Complementary log−log")
)
do.call(marrangeGrob, list(grobs = lapply(glist, function(x) x$plot), ncol = 3, nrow = 1))

## ----aggregating data----------------------------------------------------
cuts <- seq(0, 23, 1)
lifetab_dat = orca %>%
  mutate(time_cat = cut(time, cuts)) %>%
  group_by(time_cat) %>%
  summarise(nlost = sum(all == 0),
            nevent = sum(all == 1))

## ----suvTable lifetable--------------------------------------------------
dat_lt <- with(lifetab_dat, lifetab(tis = cuts[-length(cuts)], ninit = nrow(orca), 
                                    nlost = nlost, nevent = nevent))
round(dat_lt, 4)

## ----suvTable FH---------------------------------------------------------
fit_fh <- survfit(su_obj ~ 1, data = orca, type = "fleming-harrington", conf.type = "log-log")
dat_fh <- fortify(fit_fh)
## for the Nelson-Aalen estimator of the cumulative hazard
#dat_fh <- fortify(fit_fh, fun = "cumhaz")
head(dat_fh)

## ----comaring Surv estimates---------------------------------------------
ggplotly(
ggplot() +
  geom_step(data = dat_km, aes(x = time, y = surv, colour = "K-M")) +
  geom_step(data = dat_fh, aes(x = time, y = surv, colour = "N-A")) +
  geom_step(data = dat_lt, aes(x = cuts[-length(cuts)], y = surv, colour = "LT")) +
  labs(x = "Time (years)", y = "Survival", colour = "Estimator") +
  theme_classic()
)

## ----measure of central tendency-----------------------------------------
(mc <- data.frame(q = c(.25, .5, .75),
                 km = quantile(fit_km),
                 fh = quantile(fit_fh)))

## ----surv quantiles plot-------------------------------------------------
ggsurvplot(fit_km, xlab = "Time (years)", censor = F)$plot +
  geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, yend = 0), lty = 2) +
  geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2)

## ----parametric surv-----------------------------------------------------
fit_exp <- flexsurvreg(su_obj ~ 1, data = orca, dist = "exponential")
fit_exp
fit_w <- flexsurvreg(su_obj ~ 1, data = orca, dist = "weibull")
fit_ll <- flexsurvreg(su_obj ~ 1, data = orca, dist = "llogis")
fit_sp <- flexsurvspline(su_obj ~ 1, data = orca, k = 1, scale = "odds")

## ----comparison parametric surv, fig.width=12----------------------------
grid.arrange(
  ggplot(data.frame(summary(fit_exp)), aes(x = time)) + 
    geom_line(aes(y = est, col = "Exponential")) +
    geom_line(data = data.frame(summary(fit_w)), aes(y = est, col = "Weibull")) +
    geom_line(data = data.frame(summary(fit_ll)), aes(y = est, col = "Log-logistic")) +
    geom_line(data = data.frame(summary(fit_sp)), aes(y = est, col = "Flex splines")) +
    labs(x = "Time (years)", y = "Survival", col = "Distributions") + theme_classic(),
  ggplot(data.frame(summary(fit_exp, type = "hazard")), aes(x = time)) + 
    geom_line(aes(y = est, col = "Exponential")) +
    geom_line(data = data.frame(summary(fit_w, type = "hazard")), aes(y = est, col = "Weibull")) +
    geom_line(data = data.frame(summary(fit_ll, type = "hazard")), aes(y = est, col = "Log-logistic")) +
    geom_line(data = data.frame(summary(fit_sp, type = "hazard")), aes(y = est, col = "Flex splines")) +
    labs(x = "Time (years)", y = "Hazard", col = "Distributions") + theme_classic(),
  ncol = 2
)

## ----incidence rates-----------------------------------------------------
#ci.exp(glm(all ~ 0 + stage, data = orca, family = "poisson", offset = log(time)))
with(orca %>% group_by(stage) %>%
  summarise(D = sum(all),
            Y = sum(time)),
  cbind(stage, pois.approx(x = D, pt = Y)))

## ----comp surv stages----------------------------------------------------
su_stg  <- survfit(su_obj ~ stage, data = orca)
su_stg

## ----KM stages-----------------------------------------------------------
ggsurvplot(su_stg, fun = "event", censor = F, xlab = "Time (years)")

## ----surv table stages---------------------------------------------------
lifetab_stg <- fortify(su_stg)
lifetab_stg %>%
  group_by(strata) %>%
  do(head(., n = 3))

## ----cumhaz surv stages, fig.width = 12----------------------------------
glist <- list(
  ggsurvplot(su_stg, fun = "cumhaz"),
  ggsurvplot(su_stg, fun = "cloglog")
)
# plot(su_stg, fun = "cloglog")
do.call(marrangeGrob, list(grobs = lapply(glist, function(x) x$plot), ncol = 2, nrow = 1))

## ----M-H logrank test----------------------------------------------------
survdiff(su_obj ~ stage, data = orca)

## ----Peto & Peto test----------------------------------------------------
survdiff(su_obj ~ stage, data = orca, rho = 1)

## ------------------------------------------------------------------------
orca$agegr <- cut(orca$age, breaks = c(0, 55, 75, 95))
stat.table(list(sex, agegr), list(count(), percent(agegr)), margins = T, data = orca)

## ---- fig.width=12-------------------------------------------------------
su_agrx <- survfit(su_obj ~ agegr + sex, data = orca)
gg_agrx <- ggsurvplot(su_agrx)
gg_agrx$plot + theme_bw() + facet_wrap( ~ agegr)

## ----cox model-----------------------------------------------------------
m1 <- coxph(su_obj ~ sex + I((age-65)/10) + stage, data = orca)
summary(m1)

## ----test ph-------------------------------------------------------------
cox.zph.m1 <- cox.zph(m1)
cox.zph.m1

## ----plot ph, fig.height=12----------------------------------------------
ggcoxzph(cox.zph.m1)

## ----updated cox---------------------------------------------------------
orca2 <- orca %>%
  filter(stage != "unkn") %>%
  mutate(st3 = Relevel(droplevels(stage), list(1:2, 3, 4)))
m2 <- coxph(Surv(time, all) ~ sex + I((age-65)/10) + st3, data = orca2, ties = "breslow")
round(ci.exp(m2), 4)

## ----newdata for predictions---------------------------------------------
newd <- expand.grid(sex = c("Male", "Female"), age = c(40, 80), st3 = levels(orca2$st3))
newd$id <- 1:12
newd

## ----survival from cox, fig.width=12-------------------------------------
fortify(survfit(m2, newdata = newd)) %>%
  gather(strata, surv, surv.1:surv.12) %>%
  mutate(id = gsub("surv.","", strata)) %>%
  merge(newd, by = "id") %>% 
  ggplot(aes(x = time, y = surv, col = sex, linetype = factor(age))) +
  geom_step() + facet_grid(. ~ st3) +
  labs(x = "Time (years)", y = "Survival probability") + theme_classic()

## ----weibull model-------------------------------------------------------
m2w <- flexsurvreg(Surv(time, all) ~ sex + I((age-65)/10) + st3, data = orca2, dist = "weibull")
m2w

## ------------------------------------------------------------------------
m2wph <- weibreg(Surv(time, all) ~ sex + I((age-65)/10) + st3, data = orca2)
summary(m2wph)

## ----median surv weibull-------------------------------------------------
median.weibull <- function(shape, scale) qweibull(0.5, shape = shape, scale = scale)
set.seed(2153)
newd <- data.frame(sex = c("Male", "Female"), age = 65, st3 = "I+II")
summary(m2w, newdata = newd, fn = median.weibull, t = 1, B = 10000)

## ----median surv cox-----------------------------------------------------
survfit(m2, newdata = newd)

## ------------------------------------------------------------------------
cuts <- sort(unique(orca2$time[orca2$all == 1]))
orca_splitted <- survSplit(Surv(time, all) ~ ., data = orca2, cut = cuts, episode = "tgroup")
head(orca_splitted, 15)

## ---- conditional poisson------------------------------------------------
mod_poi <- gnm(all ~ sex + I((age-65)/10) + st3, data = orca_splitted, 
               family = poisson, eliminate = factor(time))
summary(mod_poi)

## ----comp poisson cox----------------------------------------------------
round(data.frame(cox = ci.exp(m2), poisson = ci.exp(mod_poi)), 4)

## ----poisson basehazard (takes time), echo=c(1:3), eval=-c(2, 3), eval=FALSE----
## orca_splitted$dur <- with(orca_splitted, time - tstart)
## mod_poi2 <- glm(all ~ -1 + factor(time) + sex + I((age-65)/10) + st3,
##                 data = orca_splitted, family = poisson, offset = log(dur))

## ----loading from url, echo=FALSE----------------------------------------
orca_splitted$dur <- with(orca_splitted, time - tstart)
#save(mod_poi2, file = "data/mod_poi2.Rdata")
load(url("http://www.stats4life.se/data/mod_poi2.Rdata"))

## ----step hazard---------------------------------------------------------
newd <- data.frame(time = unique(orca2$time), dur = 1,
                   sex = "Female", age = 65, st3 = "I+II")
blhaz <- 1000*data.frame(ci.pred(mod_poi2, newdata = newd))
xint <- unique(cbind(orca_splitted$tstart, orca_splitted$time))
ggplot(blhaz, aes(x = xint[, 1], y = Estimate, xend = xint[, 2], yend = Estimate)) + geom_segment() +
  scale_y_continuous(trans = "log", breaks = c(.001, .1, 50, 500, 5000)) +
  theme_classic() + labs(x = "Time (years)", y = "Baseline hazard")

## ----flexible hazard-----------------------------------------------------
k <- quantile(orca2$time, 1:5/6)
mod_poi2s <- glm(all ~ ns(time, knots = k) + sex + I((age-65)/10) + st3, 
                data = orca_splitted, family = poisson, offset = log(dur))
round(ci.exp(mod_poi2s), 3)
blhazs <- 1000*data.frame(ci.pred(mod_poi2s, newdata = newd))
ggplot(blhazs, aes(x = newd$time, y = Estimate)) + geom_line() +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), alpha = .2) +
  scale_y_continuous(trans = "log", breaks = c(20, 50, 100, 200, 500, 1000)) +
   theme_classic() + labs(x = "Time (years)", y = "Baseline hazard")

## ----predicted survivals-------------------------------------------------
newd <- data.frame(sex = "Female", age = 65, st3 = "I+II")
surv_cox <- fortify(survfit(m2, newdata = newd))
surv_weibull <- summary(m2w, newdata = newd, tidy = TRUE)
## For the poisson model we need some extra steps
tbmid <- sort(unique(.5*(orca_splitted$tstart + orca_splitted$time)))
mat <- cbind(1, ns(tbmid, knots = k), 0, 0, 0, 0)
Lambda <- ci.cum(mod_poi2s, ctr.mat = mat, intl = diff(c(0, tbmid)))
surv_poisson <- data.frame(exp(-Lambda))

## ----plot predicted survivals--------------------------------------------
ggplot(surv_cox, aes(time, surv)) + geom_step(aes(col = "Cox")) +
  geom_line(data = surv_weibull, aes(y = est, col = "Weibull")) +
  geom_line(data = surv_poisson, aes(x = c(0, tbmid[-1]), y = Estimate, col = "Poisson")) +
  labs(x = "Time (years)", y = "Survival", col = "Models") + theme_classic()

## ---- eval = FALSE-------------------------------------------------------
## library(shiny)
## library(tidyverse)
## library(splines)
## library(Epi)
## library(survival)
## library(flexsurv)
## library(ggfortify)
## 
## shinyApp(
## 
##   ui = fluidPage(
##     h2("Choose covariate pattern:"),
##     selectInput("sex", label = h3("Sex"),
##                 choices = list("Female" = "Female" , "Male" = "Male")),
##     sliderInput("age", label = h3("Age"),
##                 min = 20, max = 80, value = 65),
##     selectInput("st3", label = h3("Stage (3 levels)"),
##                 choices = list("Stage I and II" = "I+II", "Stage III" = "III",
##                                "Stage IV" = "IV")),
##     plotOutput("survPlot")
##   ),
## 
##   server = function(input, output){
## 
##     orca <- read.table("http://www.stats4life.se/data/oralca.txt", header = T)
##     orca2 <- orca %>%
##       filter(stage != "unkn") %>%
##       mutate(st3 = Relevel(droplevels(stage), list(1:2, 3, 4)),
##              all = 1*(event != "Alive"))
##     m2 <- coxph(Surv(time, all) ~ sex + I((age-65)/10) + st3, data = orca2, ties = "breslow")
##     m2w <- flexsurvreg(Surv(time, all) ~ sex + I((age-65)/10) + st3, data = orca2, dist = "weibull")
##     cuts <- sort(unique(orca2$time[orca2$all == 1]))
##     orca_splitted <- survSplit(Surv(time, all) ~ ., data = orca2, cut = cuts, episode = "tgroup")
##     orca_splitted$dur <- with(orca_splitted, time - tstart)
##     k <- quantile(orca2$time, 1:5/6)
##     mod_poi2s <- glm(all ~ ns(time, knots = k) + sex + I((age-65)/10) + st3,
##                      data = orca_splitted, family = poisson, offset = log(dur))
## 
##     newd <- reactive({
##       data.frame(sex = input$sex, age = input$age, st3 = input$st3)
##     })
## 
##     output$survPlot <- renderPlot({
##       newd <- newd()
##       surv_cox <- fortify(survfit(m2, newdata = newd))
##       surv_weibull <- summary(m2w, newdata = newd, tidy = TRUE)
##       tbmid <- sort(unique(.5*(orca_splitted$tstart + orca_splitted$time)))
##       mat <- cbind(1, ns(tbmid, knots = k), 1*(input$sex == "Male"), (input$age - 65)/10,
##                    1*(input$st3 == "III"), 1*(input$st3 == "IV"))
##       Lambda <- ci.cum(mod_poi2s, ctr.mat = mat, intl = diff(c(0, tbmid)))
##       surv_poisson <- data.frame(exp(-Lambda))
## 
##       ggplot(surv_cox, aes(time, surv)) + geom_step(aes(col = "Cox")) +
##         geom_line(data = surv_weibull, aes(y = est, col = "Weibull")) +
##         geom_line(data = surv_poisson, aes(x = c(0, tbmid[-1]), y = Estimate, col = "Poisson")) +
##         labs(x = "Time (years)", y = "Survival", col = "Models") + theme_classic()
##     })
##   }
## )

## ----quadratic age-------------------------------------------------------
m3 <- coxph(Surv(time, all) ~ sex + I(age-65) + I((age-65)^2) + st3, data = orca2)
summary(m3)

## ----plot non-linear HR--------------------------------------------------
age <- seq(20, 80, 1) - 65
hrtab <- ci.exp(m3, ctr.mat = cbind(0, age, age^2, 0, 0))
ggplot(data.frame(hrtab), aes(x = age+65, y = exp.Est.., ymin = X2.5., ymax = X97.5.)) +
  geom_line() + geom_ribbon(alpha = .1) +
  scale_y_continuous(trans = "log", breaks = c(.1, .25, .5, 1, 2)) +
  labs(x = "Age (years)", y = "Hazard ratio") + theme_classic() +
  geom_vline(xintercept = 65, lty = 2) + geom_hline(yintercept = 1, lty = 2)

## ----plot zph for sex----------------------------------------------------
plot(cox.zph.m1[1])
abline(h= m1$coef[1], col = 2, lty = 2, lwd = 2)

## ----time-dependent coef-------------------------------------------------
orca3 <- survSplit(Surv(time, all) ~ ., data = orca2, cut = c(5, 15), episode = "tgroup")
head(orca3)
m3 <- coxph(Surv(tstart, time, all) ~ relevel(sex, 2):strata(tgroup) + I((age-65)/10) + st3, data = orca3)
m3

## ----CIF-----------------------------------------------------------------
cif <- Cuminc(time = "time", status = "event", data = orca2)
head(cif)

## ----CIF and survival plot-----------------------------------------------
ggplot(cif, aes(time)) +
  geom_step(aes(y = `CI.Oral ca. death`, colour = "Cancer death")) +
  geom_step(aes(y = `CI.Oral ca. death`+ `CI.Other death`, colour = "Total")) +
  geom_step(aes(y = Surv, colour = "Overall survival")) + 
  labs(x = "Time (years)", y = "Proportion", colour = "") +
  theme_classic()

## ----CIF st3-------------------------------------------------------------
cif_stage <- Cuminc(time = "time", status = "event", group = "st3", data = orca2)
cif_stage %>% 
  group_by(group) %>%
  do(head(., n = 3))

## ----plot CIF st3--------------------------------------------------------
grid.arrange(
  ggplot(cif_stage, aes(time)) +
    geom_step(aes(y = `CI.Oral ca. death`, colour = group)) +
    labs(x = "Time (years)", y = "Proportion", title = "Cancer death by stage") + 
    theme_classic(),
  ggplot(cif_stage, aes(time)) +
    geom_step(aes(y = `CI.Other death`, colour = group)) +
    labs(x = "Time (years)", y = "Proportion", title = "Other deaths by stage") + 
    theme_classic(),
  ncol = 2
)

## ----cox competing-------------------------------------------------------
m2haz1 <- coxph(Surv(time, event == "Oral ca. death") ~ sex + I((age-65)/10) + st3, data = orca2)
round(ci.exp(m2haz1), 4)
m2haz2 <- coxph(Surv(time, event == "Other death") ~ sex + I((age-65)/10) + st3, data = orca2)
round(ci.exp(m2haz2), 4)

## ----Fine–Gray model-----------------------------------------------------
m2fg1 <- with(orca2, crr(time, event, cov1 = model.matrix(m2), failcode = "Oral ca. death"))
summary(m2fg1, Exp = T)
m2fg2 <- with(orca2, crr(time, event, cov1 = model.matrix(m2), failcode = "Other death"))
summary(m2fg2, Exp = T)

