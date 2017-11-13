library(dosresmeta)
library(tidyverse)
library(rms)

data("alcohol_cvd")

# combination of different power terms
grid <- fpgrid()
shift <- 0.5
scale <- 1

# traditional two-stage analysis
modi_fp <- apply(grid, 1, function(p)
  dosresmeta(formula = logrr ~ fracpol(dose, p = p, shift = shift, scale = scale), 
             type = type, id = id, se = se, cases = cases, n = n, 
             data = alcohol_cvd)
  )

# "best" (lowest AIC) model
bfp <- which.min(sapply(modi_fp, AIC))
grid[bfp, ]

# graphical prediction
xref <- 0
newd <- data.frame(dose = c(xref, seq(0, 45, length.out = 500)))
newd %>%
  bind_cols(
    lapply(modi_fp, function(m) predict(m, newdata = ., expo = T)$pred)
  ) %>%
  gather(fracpol, pred, -dose) %>%
  mutate(best = factor(fracpol == bfp, labels = c("p(3, 3)", "Other"))) %>%
  ggplot(aes(dose, pred, group = fracpol)) +
  geom_line(aes(col = best), size = 1) +
  scale_y_continuous(trans = "log", breaks = c(.6, .75, 1, 1.5, 2)) +
  scale_color_manual(values = c("grey", "black")) +
  labs(x = "Dose", y = "Relative risk", col = "Frac Pol") +
  theme_classic()

  
# alternative one-stage analysis
modi_fp_os <- apply(grid, 1, function(p)
  dosresmeta(formula = logrr ~ fracpol(dose, p = p, shift = shift, scale = scale), 
             type = type, id = id, se = se, cases = cases, n = n, 
             data = alcohol_cvd, proc = "1stage")
)

# "best" (lowest AIC) model
bfp_os <- which.min(sapply(modi_fp_os, AIC))
grid[bfp_os, ]

# graphical prediction
newd %>%
  bind_cols(
    lapply(modi_fp_os, function(m) predict(m, newdata = newd, expo = T)$pred)
  ) %>%
  gather(fracpol, pred, -dose) %>%
  mutate(best = factor(fracpol == bfp_os, labels = c("Other", "p(-1, -0.5)"))) %>%
  ggplot(aes(dose, pred, group = fracpol)) +
  geom_line(aes(col = best, alpha = best), size = 1) +
  scale_y_continuous(trans = "log", breaks = c(.6, .75, 1, 1.5, 2)) +
  scale_color_manual(values = c(`p(-1, -0.5)` = "black", "Other" = "grey")) +
  scale_alpha_manual(values = c(`p(-1, -0.5)` = 1, "Other" = .2)) +
  labs(x = "Dose", y = "Relative risk", col = "Frac Pol") + guides(alpha = F) +
  theme_classic()


# comparing with restricted cubic splines
k <- quantile(alcohol_cvd$dose, c(.1, .5, .9))
spl <- dosresmeta(formula = logrr ~ rcs(dose, k), type = type, id = id, se = se, 
                  cases = cases, n = n, data = alcohol_cvd)
spl_os <- dosresmeta(formula = logrr ~ rcs(dose, k), type = type, id = id, se = se, 
                  cases = cases, n = n, data = alcohol_cvd, proc = "1stage")

# comparing different strategies
newd %>%
  bind_cols(
    pred_fp = predict(modi_fp[[bfp]], newdata = ., expo = T)$pred,
    pred_fp_os = predict(modi_fp_os[[bfp_os]], newdata = ., expo = T)$pred,
    pred_spl = predict(spl_os, newdata = ., expo = T)$pred,
  ) %>%
  gather(model, pred, -dose) %>%
  mutate(model = factor(model, labels = c("FP 2stage", "FP 1stage", "RCSplines"))) %>%
  ggplot(aes(dose, pred, group = model)) +
  geom_line(aes(col = model)) +
  scale_y_continuous(trans = "log", breaks = c(.6, .75, 1, 1.5, 2)) +
  labs(x = "Dose", y = "Relative risk", col = "Frac Pol") +
  theme_classic()

# using AICs
# not really comparable (the frac pol better fits the data)
sapply(modi_fp, AIC)
sapply(list(modi_fp[[bfp]], spl), AIC)
# more comparable (spline better fits the data)
sapply(modi_fp_os, AIC)
sapply(list(modi_fp_os[[bfp]], spl_os), AIC)

