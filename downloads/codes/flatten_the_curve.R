library(tidyverse)
library(readxl)
library(ggpubr)
library(patchwork)
theme_set(theme_minimal())

# download data ----

download.file(url = "https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data",
              destfile = paste0(getwd(), "/Folkhalsomyndigheten_Covid19.xlsx"))


# read and pivot data ----

fall <- read_excel("Folkhalsomyndigheten_Covid19.xlsx", sheet = "Antal per dag region") %>% 
  pivot_longer(-"Statistikdatum", values_to = "dag_fall", names_to = "region") %>% 
  mutate(region = replace(region, region == "Totalt_antal_fall", "Sverige")) %>% 
  filter(Statistikdatum < Sys.Date() - 2)
# grou_by Stokcholm vs Other (and Sweden)
fall_sthlm <- fall %>% 
  mutate(
    sthlm = replace(region, !region %in% c("Sverige", "Stockholm"), "Other regions")
  ) %>% 
  group_by(Statistikdatum, sthlm) %>% 
  summarise(dag_fall = sum(dag_fall)) %>% 
  filter(dag_fall > 0) %>%
  group_by(sthlm) %>% 
  mutate(date_from_1 = seq_along(Statistikdatum))

avlidna <- read_excel("Folkhalsomyndigheten_Covid19.xlsx", 
                      sheet = "Antal avlidna per dag", col_types = c("date", "numeric")) %>% 
  filter(!is.na(Datum_avliden), Datum_avliden < Sys.Date() - 2)

iva <- read_excel("Folkhalsomyndigheten_Covid19.xlsx", 
                      sheet = "Antal intensivvårdade per dag") %>% 
  filter(Datum_vårdstart < Sys.Date() - 2)

antal_agegr <- read_excel("Folkhalsomyndigheten_Covid19.xlsx", 
                          sheet = "Totalt antal per åldersgrupp") %>% 
  mutate(
    age_group = case_when(
      Åldersgrupp %in% c("Ålder_0_9", "Ålder_10_19", 
                         "Ålder_20_29", "Ålder_30_39") ~ "< 40 years",
      Åldersgrupp %in% c("Ålder_40_49", "Ålder_50_59", "Ålder_60_69") ~ "[40 - 70) years",
      Åldersgrupp %in% c("Ålder_70_79", "Ålder_80_90", "Ålder_90_plus") ~ ">= 70 years"
    )
  ) %>% 
  filter(!is.na(age_group)) %>% 
  group_by(age_group) %>% 
  summarise(
    Totalt_antal_fall = sum(Totalt_antal_fall),
    Totalt_antal_intensivvårdade = sum(Totalt_antal_intensivvårdade),
    Totalt_antal_avlidna = sum(Totalt_antal_avlidna)
  ) %>% 
  pivot_longer(-age_group, values_to = "value", names_to = "variable") %>% 
  mutate(
    age_group = factor(age_group, levels = c("< 40 years", "[40 - 70) years", ">= 70 years")),
    variable = factor(gsub("_", " ", variable), levels = c("Totalt antal fall", 
                                                           "Totalt antal intensivvårdade",
                                                           "Totalt antal avlidna"))
  ) %>% 
  group_by(variable) %>% 
  mutate(perc = value/sum(value)) %>% 
  arrange(age_group, variable)



# new cases ----
filter(fall, region == "Sverige", dag_fall > 1) %>% 
  ggplot(aes(Statistikdatum, dag_fall)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F)
pfall <- ggplot(fall_sthlm, aes(date_from_1, dag_fall)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Day after 1st covid19 case", y = "Daily number of new covid19 cases") +
  facet_grid(sthlm ~ ., scales = "free")
pfall

# deaths ----
pdeaths <- ggplot(avlidna, aes(Datum_avliden, Antal_avlidna)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F) +
  labs(x = "", y = "Daily numer of covid19-deaths")
pdeaths

# iva ----
piva <- ggplot(iva, aes(Datum_vårdstart, Antal_intensivvårdade)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F) +
  labs(x = "", y = "Number of people in intensive care")
piva

# by age group ----
antal_agegr
agegr_value <- ggplot(antal_agegr, aes(variable, value, fill = age_group)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Absolute numbers") +
  guides(fill = FALSE)
agegr_perc <- ggplot(antal_agegr, aes(variable, perc, fill = age_group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Percentage numbers") +
  theme(legend.position = "bottom")
pagegr <- ggarrange(agegr_value, agegr_perc, ncol = 2, nrow = 1, 
                    common.legend = TRUE, legend = "bottom")
pagegr


# patchwork plot ----

(pfall | (pdeaths / piva)) /
  (pagegr + gridExtra::tableGrob(antal_agegr[, 1:3], rows = NULL) +
     plot_layout(widths = c(1.3, .7))) +
  plot_layout(widths = c(2, 2), heights = c(2, 1))
ggsave("flatten_the_cruve.jpeg", width = 16, height = 10)
ggsave("flatten_the_cruve.pdf", width = 16, height = 10)

