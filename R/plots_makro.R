library(here)
library(ggplot2)
library(scales)
library(padr)
library(dplyr)
month <- lubridate::month

source(here("R", "FUN_ggsejv.R"))

mijene <- readRDS(here("podaci", "mijene_tidy.Rds"))

mijene %>%
  filter(mijena == "plima") %>% 
  ggplot(aes(x = vrijeme, y = visina, colour = br_mijene)) +
  geom_point(size = .5) +
  geom_smooth(se = FALSE)

# kewl
mijene %>% 
  ggplot(aes(x = vrijeme, y = visina, colour = mijena)) +
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE) +
  labs(x = "")

# malo čudno
mijene %>% 
  thicken("month") %>% 
  filter(!is.na(vrijeme_month)) %>%
  filter(mijena == "plima") %>% 
  group_by(mijena, vrijeme_month) %>%
  mutate(visina_month = mean(visina, na.rm = TRUE)) %>%
  ggplot(aes(x = vrijeme, y = visina_month, colour = br_mijene)) +
  geom_point(size = .5) +
  # geom_smooth(se = FALSE)
  labs(x = "")

# kad je najviša plima u danu?

plime <- mijene %>%
  filter(mijena == "plima")

plime_valid <- plime %>%
  filter(!is.na(visina))

najvisa_plima <- plime_valid %>%
  thicken("day") %>% 
  group_by(vrijeme_day) %>% 
  summarise(visina = max(visina, na.rm = TRUE))

najvisa_plima_vrijeme <- left_join(
  najvisa_plima,
  plime_valid %>% thicken("day")
          )

View(najvisa_plima_vrijeme)

lubridate::day(najvisa_plima_vrijeme$vrijeme) <- 1
lubridate::month(najvisa_plima_vrijeme$vrijeme) <- 1
lubridate::year(najvisa_plima_vrijeme$vrijeme) <- 1

najvisa_plima_vrijeme %>% 
  ggplot(aes(x = vrijeme_day, y = vrijeme, size = visina)) +
  geom_point(alpha = .5) +
  scale_x_date(date_breaks = "1 month")

najvisa_plima_vrijeme %>%
  filter(month(vrijeme_day) == 3) %>% 
  ggplot(aes(x = vrijeme_day, y = vrijeme, size = visina)) +
  geom_point(alpha = .5) +
  scale_x_date(date_breaks = "1 day")


najvisa_plima_vrijeme %>%
  mutate(
    month = months(vrijeme_day, abbreviate = TRUE) %>% forcats::fct_inorder()
      ) %>% 
  ggplot(aes(x = vrijeme_day, y = vrijeme)) +
  geom_point(colour = "magenta") +
  scale_x_date(date_breaks = "2 day",
               date_minor_breaks = "1 day",
               labels = date_format("%d"),
               expand = c(0,0)) +
  scale_y_datetime(date_breaks = "3 hour",
               labels = date_format("%H"),
               expand = c(0,0)) +
  facet_wrap(vars(month), scales = "free_x") +
  labs(title = "Najveća plima u danu - Zadar - 2018",
       x = "dan u mjesecu", y = "vrijeme u danu") +
  theme_dark()


ggsejv(filename = "plima_makro_zd.svg",
       AA = "A4.l")
