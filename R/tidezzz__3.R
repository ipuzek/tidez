library(here)
library(tibble)
# library(stringr)
ymd_hm <- lubridate::ymd_hm # library(lubridate)
library(tidyr)
library(dplyr)

# alternativno s readhtml -------------------------------------------------

theurl.zd <- "http://tides.mobilegeographics.com/calendar/year/7148.html"
tables.zd <- XML::readHTMLTable(theurl.zd, header = TRUE,
                                as.data.frame = TRUE,
                                stringsAsFactors = FALSE)

# theurl.st <- "http://tides.mobilegeographics.com/calendar/year/6030.html"
# tables.st <- XML::readHTMLTable(theurl.st, header = TRUE)

date <- seq(
  from = as.Date("20180101", "%Y%m%d"),
  to = as.Date("20181231", "%Y%m%d"),
  by = "days")

rinejm <- function(x) {
  
  for (i in 1:12) {
    names(x[[i]])[2] <- "plima.1"
    names(x[[i]])[3] <- "oseka.1"
    names(x[[i]])[4] <- "plima.2"
    names(x[[i]])[5] <- "oseka.2"
    names(x[[i]])[6] <- "plima.3"
  }
  
  x
  
}

tables.zd.ren <- rinejm(tables.zd)

dfzd <- bind_rows(tables.zd.ren) %>% as_data_frame()


# sređivanje et al --------------------------------------------------------------------

convert_visina <- function(x) {
  as.numeric(
    stringr::str_replace(x, pattern = " m", replacement = "")
  )
}

convert_time_custom <- function(x) {
  stringr::str_replace(x, ":", "-") %>% ymd_hm(tz = "CET")
}

dfzd.sep <- dfzd %>%
  separate(plima.1, into = c("plima.1.t", "plima_1_h"), sep = " / ") %>%
  separate(plima.2, into = c("plima.2.t", "plima_2_h"), sep = " / ") %>%
  separate(plima.3, into = c("plima.3.t", "plima_3_h"), sep = " / ") %>%
  separate(oseka.1, into = c("oseka.1.t", "oseka_1_h"), sep = " / ") %>%
  separate(oseka.2, into = c("oseka.2.t", "oseka_2_h"), sep = " / ")

dfzd.sep.date <- add_column(dfzd.sep, dan = date, .before = 1) %>% 
  select(-Day)

dfzd.sep.date.vis <- dfzd.sep.date %>%
  mutate_at(vars(ends_with("_h")), convert_visina)

dfzd.sep.date.vis.dttm <- dfzd.sep.date.vis %>% 
  unite("plima_1_tt", dan, plima.1.t, remove = FALSE) %>% 
  unite("plima_2_tt", dan, plima.2.t, remove = FALSE) %>% 
  unite("plima_3_tt", dan, plima.3.t, remove = FALSE) %>% 
  unite("oseka_1_tt", dan, oseka.1.t, remove = FALSE) %>%
  unite("oseka_2_tt", dan, oseka.2.t, remove = FALSE) %>% 
  mutate_at(vars(ends_with("_tt")), convert_time_custom 
  )

tidyyy.mijene <- function(df = dfzd.sep.date.vis.dttm, mijena, atribut){
  df %>% 
  select(starts_with(mijena)) %>% 
  gather(kljuc, velju, ends_with(atribut)) %>% 
  select(kljuc,velju) %>% 
  separate(kljuc, c("mijena", "br_mijene", "tmp")) %>% 
  select(-tmp)
}

plime <- add_column(
  tidyyy.mijene(mijena = "plima_", atribut = "_tt"),
  visina = pull(tidyyy.mijene(mijena = "plima_", atribut = "_h"), velju)
  ) %>% 
  rename(vrijeme = velju)
  

oseke <- add_column(
  tidyyy.mijene(mijena = "oseka_", atribut = "_tt"),
  visina = pull(tidyyy.mijene(mijena = "oseka_", atribut = "_h"), velju)
  ) %>% 
  rename(vrijeme = velju)

mijene <- bind_rows(plime, oseke)

saveRDS(mijene, here("podaci", "mijene_tidy.Rds"))

# solunarni

lasci <- dfzd.sep.date.vis %>%
  unite("sunrise_tt", dan, Sunrise, remove = FALSE) %>% 
  unite("sunset_tt", dan, Sunset, remove = FALSE) %>% 
  mutate_at(vars(ends_with("_tt")), convert_time_custom) %>% 
  select(dan, sunrise_tt, sunset_tt)

saveRDS(lasci, here("podaci", "__lasci_tidy.Rds"))
