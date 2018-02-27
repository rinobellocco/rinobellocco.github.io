library(dosresmeta)
library(readxl)
library(tidyverse)

coffee_mort1 <- read.table("http://alessiocrippa.altervista.org/data/coffee_allcause.txt")
data("coffee_mort")
coffee_mort1 <- filter(coffee_mort1, !(id %in% unique(coffee_mort$id))) %>%
  select(names(coffee_mort))

coffee_mort_add <- read_excel("/Users/alessiocrippa/Dropbox/KI/Working/one_stage/AdditionalCode/one_category.xlsx", sheet = 1) %>%
  select(names(coffee_mort)) %>%
  bind_rows(coffee_mort1) %>% 
  as.data.frame() %>%
  arrange(id) 

save(coffee_mort_add, file = "coffee_mort_add.rda")


table(coffee_mort_add$id)
