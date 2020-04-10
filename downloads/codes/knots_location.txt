## Perform sensitivity analysis for knots location

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

# choosing alternative knots location
p_dose <- round(quantile(alcohol_cvd$dose, c(.1, .25, .5, .75, .9)), 2)
klist <- combn(p_dose, 3, simplify = F)
spl_k <- lapply(klist, function(k)
  dosresmeta(formula = logrr ~ rcs(dose, k), id = id, se = se, 
             type = type, cases = cases, n = n, data = alcohol_cvd)
)

# graphical presentation
xref <- 0
newd <- data.frame(dose = c(xref, seq(0, 45, length.out = 500)))
newd %>% cbind(
  do.call("cbind",
          lapply(spl_k, function(m) predict(m, newdata = ., expo = T)$pred)
  )) %>%
  `colnames<-`(c("dose", sapply(klist, paste, collapse = ", "))) %>%
  gather(k, pred, -dose) %>%
  ggplot(aes(dose, pred, col = k)) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = pretty_breaks()) +
  labs(x = "Dose", y = "Relative risk", col = "Knots location") +
  theme_classic()

# or alternatevely
newd %>% cbind(
  do.call("cbind",
          lapply(spl_k, function(m) predict(m, newdata = ., expo = T)$pred)
  )) %>%
  `colnames<-`(c("dose", sapply(klist, paste, collapse = ", "))) %>%
  gather(k, pred, -dose) %>%
  ggplot(aes(dose, pred, col = k, label = k)) +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = k), method = list(dl.trans(x = x + .5), 'last.bumpup')) +
  scale_y_continuous(trans = "log", breaks = pretty_breaks()) + xlim(c(0, 55)) +
  labs(x = "Dose", y = "Relative risk", col = "Knots location") +
  theme_classic()


# predictions for selected values
newd_tab <- data.frame(dose = c(xref, seq(10, 50, 10)))
newd_tab <- cbind(
  do.call("rbind",
          lapply(spl_k, function(m) 
            cbind(newd_tab, predict(m, newdata = newd_tab, expo = T)[, -c(1:2)]))
  ),
  k_loc = rep(sapply(klist, paste, collapse = ", "), each = nrow(newd_tab)))
arrange(newd_tab, dose, k_loc)
filter(newd_tab, dose == 20)
