library(here)
library(ggplot2)
# library(ggalt)
library(anytime)
day <- lubridate::day;  hour <- lubridate::hour # library(lubridate)
library(padr)
library(tibble)
library(dplyr)

source(here("R", "FUN_ggsejv.R"))

mijene <- readRDS(here("podaci", "mijene_tidy_ST.Rds"))
lasci <- readRDS(here("podaci", "__lasci_tidy_ST.Rds"))

# 9 navecer et time manipulations ---------------------------------------------

vri <- pull(mijene, vrijeme)
lubridate::hour(vri) <- 21
lubridate::minute(vri) <- 0

mijene_ready <- mijene %>% 
  add_column(devet_navecer = vri) %>%
  mutate(
    samo_vrijeme = strptime(vrijeme, "%Y-%m-%d %H:%M:%S") %>% format("%H:%M"),
    samo_vrijeme_plima = case_when(mijena == "oseka" ~ NA_character_,
                                   TRUE              ~ samo_vrijeme
    )
  )

nice_date <- function(x, plus = FALSE) {
  if (plus) {
    x.plus <- as.Date(x) + 1
    format(x.plus, "%d.%m.")
    
  } else {
    as.Date(x) %>% format("%d.%m.")
    
  }
}

# PLOT today + 4 --------------------------------------------------------------

mijene_ready_todayPLUS4 <- mijene_ready %>%
  arrange(vrijeme) %>% 
  thicken("day", by = "vrijeme") %>% 
  arrange(vrijeme_day)
# filter(vrijeme_day >= anydate(Sys.Date()) & vrijeme_day <= anydate(Sys.Date()) + 4)

lasci.dan <- lasci %>%
  # filter(dan >= anydate(Sys.Date()) & dan <= anydate(Sys.Date()) + 4) %>%
  mutate(danUtjednu = lubridate::wday(dan, label = TRUE, week_start = 1))

nau <- lubridate::now()
lubridate::hour(nau) <- 0
lubridate::minute(nau) <- 0
lubridate::second(nau) <- 0

dani_na_grafu <- c(
  nau,
  nau + 3*3600*24) %>% 
  anytime()

# brejks <- seq.POSIXt(
#   from = dani_na_grafu[1],
#   to = dani_na_grafu[2],
#   by = "3 hour")

mikroplot_final <- mijene_ready_todayPLUS4 %>% 
  ggplot() +
  geom_rect(data = lasci, aes(xmin = sunrise_tt, xmax = sunset_tt, ymin = 0, ymax = .6), alpha = .1, fill = "white") +
  geom_point(aes(x = vrijeme, y = visina, colour = mijena), show.legend = FALSE) +
  geom_text(aes(x = vrijeme, y = visina, label = samo_vrijeme_plima), nudge_y = .02, colour = "white") +
  ggalt::geom_xspline(aes(x = vrijeme, y = visina), colour = "grey80") +
  geom_vline(data = data_frame(sad = Sys.time()), aes(xintercept = sad)) +
  coord_cartesian(ylim = c(0.1, max(mijene$visina, na.rm = TRUE))) +
  scale_x_datetime(limits = dani_na_grafu,
                   # breaks = brejks,
                   date_breaks = "3 hour",
                   date_minor_breaks = "1 hour",  #  ,
                   # labels = function(x) if_else(is.na(lag(x)) | !day(lag(x)) == day(x),
                   #                              paste(hour(x), "\n", nice_date(x, plus = TRUE), "  "),  #  wday(x, label = TRUE)),
                   #                              paste(hour(x))),
                   labels = scales::date_format("%b %d - %H:%M")) +
  labs(subtitle = paste("Split --", "generated on:", Sys.time()),
       x = "", y = "razina mora") +
  theme_dark() +
  theme(axis.text.x=element_text(angle=45,hjust=1))

# other plot options ------------------------------------------------------

# geom_vline(aes(xintercept = devet_navecer), colour = "grey", alpha = .5, size = 2) +
# coord_cartesian(ylim = c(min(mijene$visina, na.rm = TRUE), max(mijene$visina, na.rm = TRUE))) +
# scale_x_datetime(breaks = scales::pretty_breaks(n = 20),
#                  labels = ) +
# geom_smooth(method = "lm", formula = y ~ splines::bs(x, degree = 300), se = FALSE) +
# geom_smooth(formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +

# scale_x_datetime(limits = c(anytime(Sys.Date()), anytime(Sys.Date() + 4)),
#                  breaks = scales::pretty_breaks(n = 30),
#                  date_minor_breaks = "1 hour",
#                  date_labels = "%d.%m. / %H") +

# write plot --------------------------------------------------------------

# file_dir_plot.pdf <- here("out", "plots", "todayPLUS4.pdf")
file_dir_plot.svg <- here("out", "plots", "todayPLUS4_ST.svg")
# file_dir_plot.png <- here("out", "plots", "todayPLUS4.png")

# ggsave(file_dir_plot.pdf, plot = mikroplot_final,
#        # device = "pdf",
#        width = 8.27, height = 5.83, units = "in")

# ggsave(file_dir_plot.png, plot = mikroplot_final,
#        # device = "pdf",
#        width = 8.27, height = 5.83, units = "in")

ggsejv(filename = file_dir_plot.svg,
       plot = mikroplot_final,
       AA = "A5.l")



# ggsave(here("out", "plots", "todayPLUS4.svg"), plot = mikroplot_final, device = "svg", width = 8.27, height = 5.83, units = "in")  #  

if (file.exists("Rplots.pdf")) print("Rplots.pdf je bug")

