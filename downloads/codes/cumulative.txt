## Perform cumulative dose-response meta-analysis

# use the dosresmeta version on github
library(dosresmeta)
library(tidyverse)
library(scales)
library(rms)
library(directlabels)

data("coffee_mort")

# check that the data is sorted by year
coffee_mort <- coffee_mort %>%
  arrange(year, id) %>%
  mutate(
    id2 = group_indices(., year, id)
  )

# main analysis
k <- quantile(coffee_mort$dose, c(1., .5, .9))
spl <- dosresmeta(formula = logrr ~ rcs(dose, k), id = id, se = se, 
                    type = type, cases = cases, n = n, data = coffee_mort)

# cumulative meta-analysis
spl_cum <- lapply(unique(coffee_mort$id2), function(i)
  dosresmeta(formula = logrr ~ rcs(dose, k), id = id, se = se, 
             type = type, cases = cases, n = n, data = subset(coffee_mort, id2 <= i))
  )

# graphical presentation
xref <- 0
newd <- data.frame(dose = c(xref, seq(0, 7, length.out = 500)))
newd %>% cbind(
  do.call("cbind",
          lapply(spl_cum, function(m) predict(m, newdata = ., expo = T)$pred)
  )) %>%
  gather(study, pred, -dose, convert = T) %>%
  ggplot(aes(dose, pred, col = factor(study))) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = pretty_breaks()) +
  labs(x = "Dose", y = "Relative risk", col = "Adding study") +
  theme_classic()

# or alternatevely
newd %>% cbind(
  do.call("cbind",
          lapply(spl_cum, function(m) predict(m, newdata = ., expo = T)$pred)
  )) %>%
  gather(study, pred, -dose, convert = T) %>%
  ggplot(aes(dose, pred, col = factor(study), label = factor(study))) +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = study), method = list(dl.trans(x = x + .5), 'last.bumpup')) +
  scale_y_continuous(trans = "log", breaks = pretty_breaks()) + xlim(c(0, 7.5)) +
  labs(x = "Dose", y = "Relative risk", col = "Excluded study") +
  theme_classic()

# predictions for selected values
newd_tab <- data.frame(dose = 0:7)
newd_tab <- cbind(
  do.call("rbind",
          lapply(spl_cum, function(m) 
            cbind(newd_tab, predict(m, newdata = newd_tab, expo = T)[, -c(1:2)]))
  ),
  id2 = rep(unique(coffee_mort$id2), each = nrow(newd_tab)))
arrange(newd_tab, dose, id2)
filter(newd_tab, dose == 4)
