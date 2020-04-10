## Perform leave-one-out dose-response meta-analysis

# use the dosresmeta version on github
library(dosresmeta)
library(tidyverse)
library(scales)
library(rms)
library(directlabels)

data("alcohol_cvd")

# main analysis
k <- quantile(alcohol_cvd$dose, c(1., .5, .9))
spl <- dosresmeta(formula = logrr ~ rcs(dose, k), id = id, se = se, 
                    type = type, cases = cases, n = n, data = alcohol_cvd)

# leave-one-out
spl_l1o <- lapply(unique(alcohol_cvd$id), function(i)
  dosresmeta(formula = logrr ~ rcs(dose, k), id = id, se = se, 
             type = type, cases = cases, n = n, data = subset(alcohol_cvd, id != i))
  )

# graphical presentation
xref <- 0
newd <- data.frame(dose = c(xref, seq(0, 45, length.out = 500)))
newd %>% cbind(
  do.call("cbind",
          lapply(spl_l1o, function(m) predict(m, newdata = ., expo = T)$pred)
  )) %>%
  gather(study, pred, -dose) %>%
  ggplot(aes(dose, pred, col = study)) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = pretty_breaks()) +
  labs(x = "Dose", y = "Relative risk", col = "Excluded study") +
  theme_classic()

# or alternatevely
newd %>% cbind(
  do.call("cbind",
          lapply(spl_l1o, function(m) predict(m, newdata = ., expo = T)$pred)
  )) %>%
  gather(study, pred, -dose) %>%
  ggplot(aes(dose, pred, col = study, label = study)) +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = study), method = list(dl.trans(x = x + .5), 'last.bumpup')) +
  scale_y_continuous(trans = "log", breaks = pretty_breaks()) + xlim(c(0, 45)) +
  labs(x = "Dose", y = "Relative risk", col = "Excluded study") +
  theme_classic()

# predictions for selected values
newd_tab <- data.frame(dose = c(xref, seq(10, 50, 10)))
newd_tab <- cbind(
  do.call("rbind",
          lapply(spl_l1o, function(m) 
            cbind(newd_tab, predict(m, newdata = newd_tab, expo = T)[, -c(1:2)]))
  ),
  id = rep(unique(alcohol_cvd$id), each = nrow(newd_tab)))
arrange(newd_tab, dose, id)
filter(newd_tab, dose == 20)
