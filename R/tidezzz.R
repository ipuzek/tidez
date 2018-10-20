library(readxl)
library(tibble)
library(purrr)
library(dplyr)

setwd("/home/ivan/IvanOstalo/rRibolov/R_magic/tidez/")
plima_8 <- read_excel("plima_8.xlsx")

colnames(plima_8) <-  make.unique(colnames(plima_8))


library(stringr)
library(lubridate)
library(anytime)


plima <- select(plima_8, contains("High")) %>% 
  lapply(str_split_fixed, " / ", 2) %>% 
  lapply(as_tibble) %>%
  reduce(cbind)

colnames(plima) <-  make.unique(colnames(plima))

as_tibble(plima)

# riješiti se praznih

broj_znakova <- function(x) sapply(x, str_length) %>% sum(na.rm = TRUE)
plim <- plima[lapply(plima, broj_znakova) %>% sapply(as.logical)]
plim <- as_tibble(plim)
# konvertirati znakiće u vremena i visine

convert_visina <- function(x) {
  as.numeric(
    str_replace(x, pattern = " m", replacement = "")
  )
}

plimz <- plim %>% 
  mutate_at(
          vars(starts_with("V2")),
          convert_visina) %>% 
  mutate_at(
    vars(starts_with("V1")),
    parse_date_time, orders = "IMp")

# srediti dane
library(forcats)

plimz$dan <- fct_inorder(plima_8$Day)
  
  
library(ggplot2)

ggplot(plimz, aes(x = dan, y = V1)) +
  geom_point(aes(size = V2)) +
  coord_flip() +
  theme_minimal()

ggplot(plimz, aes(x = dan, y = V1.1)) +
  geom_point(aes(size = V2.1))





### datetime troublez #
# seq(from = ymd("2016-12-01"),
#     by = 1,
#     length.out = 31)
# 
# year(plimz$V1)
# month(plimz$V1)
# date(plimz$V1)
# 
# hour(plimz$V1) + minute(plimz$V1)
# 
# x <- ymd("2012-03-26")
# date(x)
# minute(x)
# minute(x) <- 1
# minute(x) <- 61
# minute(x) > 2
# 
