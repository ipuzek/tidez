library(here)
library(readxl)
library(tibble)
library(purrr)
library(dplyr)

# TODO # samo Zadar, podudaranje plime (i plime +2) i solunarnih evenata

# alternativno s readhtml -------------------------------------------------

theurl.zd <- "http://tides.mobilegeographics.com/calendar/year/7148.html"
tables.zd <- XML::readHTMLTable(theurl.zd, header = TRUE,
                                as.data.frame = TRUE,
                                stringsAsFactors = FALSE)

# theurl.st <- "http://tides.mobilegeographics.com/calendar/year/6030.html"
# tables.st <- XML::readHTMLTable(theurl.st, header = TRUE)


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



# KRAJ readhtml --------------------------------------------------------------------

library(stringr)
library(forcats)
library(lubridate)
library(anytime)


convert_visina <- function(x) {
  as.numeric(
    str_replace(x, pattern = " m", replacement = "")
  )
}

tbl <- tables.zd.ren[[1]] %>% as_data_frame()

tbl$Day

razdvoji_bindaj_mutiraj <- function(x, grad) {
  
  dejz <- x[["Day"]]
  
  xx <- rbind(
    
  select(x, contains("plima")) %>% 
    map(str_split_fixed, " / ", 2) %>%
    map_df(as.data.frame.matrix, .id = "ajdi") %>% 
    as_tibble() ,
  
  select(x, contains("oseka")) %>% 
    map(str_split_fixed, " / ", 2) %>%
    map_df(as.data.frame.matrix, .id = "ajdi") %>% 
    as_tibble()
  
  ) %>% 
    
    mutate(razina = convert_visina(V2),
           # vrijeme = parse_date_time(V1, orders = "IMp"),
           mijena = if_else(grepl("plima", ajdi), "plima", "oseka"))
  
  xx$dan <- rep(dejz,5)
  xx$dan <- fct_inorder(xx$dan)
  
  xx$grad <- grad
  
  xx
  
}

# # vidi podatke
# tables.zd.ren[[5]] %>%  ## MJESEC ##
#   razdvoji_bindaj_mutiraj(grad = "Zadar") %>% 
#   filter(grepl("plima", ajdi)) %>%
#   filter(!is.na(razina)) %>% 
#   View

library(ggplot2)



tables.zd.ren[[10]] %>%  ## MJESEC ##
  razdvoji_bindaj_mutiraj(grad = "Zadar") %>% 
  filter(grepl("plima", ajdi)) %>% 
  ggplot(aes(x = dan, y = vrijeme)) +
  geom_point(aes(size = razina, colour = mijena)) +
  scale_y_datetime(date_breaks = "1 hours", date_labels = "%H") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



rbind(
  tables.zd.ren[[8]] %>% razdvoji_bindaj_mutiraj("Zadar"),
  tables.st.ren[[8]] %>% razdvoji_bindaj_mutiraj("Split")
) %>% 
  filter(mijena == "plima") %>% 
  ggplot(aes(x = dan, y = vrijeme)) +
  geom_point(aes(size = razina, colour = grad)) +
  scale_y_datetime(date_breaks = "1 hours", date_labels = "%H") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  





# riješiti se praznih

broj_znakova <- function(x) sapply(x, str_length) %>% sum(na.rm = TRUE)
plim <- plima[lapply(plima, broj_znakova) %>% sapply(as.logical)]
plim <- as_tibble(plim)


plimz.st <- plim %>% 
  mutate_at(
    vars(starts_with("V2")),
    convert_visina) %>% 
  mutate_at(
    vars(starts_with("V1")),
    parse_date_time, orders = "IMp")

# srediti dane
library(forcats)

plimz.st$dan <- fct_inorder(plima_8$Day)


library(ggplot2)

ggplot(plimz, aes(x = dan, y = V1)) +
  geom_point(aes(size = V2.1)) +
  coord_flip() +
  theme_minimal()

ggplot(plimz, aes(x = dan, y = V1.1)) +
  geom_point(aes(size = V2.1))

plimz$grad <- "Zadar"
plimz.st$grad <- "Split"

# prva plima

rbind(plimz, plimz.st) %>% 
  ggplot(aes(x = dan, y = V1.1)) +
  geom_point(aes(size = V2.1, colour = grad)) +
  labs(title = "Prva plima", y = "sat") +
  scale_y_datetime(date_breaks = "1 hours", date_labels = "%H") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  

# druga plima

rbind(plimz, plimz.st) %>% 
  ggplot(aes(x = dan, y = V1.2)) +
  geom_point(aes(size = V2.2, colour = grad)) +
  labs(title = "Druga plima", y = "sat") +
  scale_y_datetime(date_breaks = "1 hours", date_labels = "%H") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# prva + druga Zadar

plimz %>% 
  ggplot(aes(x = dan)) +
  geom_point(aes(y = V1.1, size = V2.1), colour = "grey") +
  geom_point(aes(y = V1.2, size = V2.2))
    
### ovo ima najviše smisla
# prva + druga # Zadar + Split

rbind(plimz, plimz.st) %>% 
  ggplot(aes(x = dan)) +
  geom_point(aes(y = V1.1, size = V2.1, colour = grad)) +
  geom_point(aes(y = V1.2, size = V2.2, colour = grad)) +
  labs(title = "Druga plima", y = "sat") +
  scale_y_datetime(date_breaks = "1 hours", date_labels = "%H") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
#  +
  labs(title = "Druga plima", y = "sat") +
  scale_y_datetime(date_breaks = "1 hours", date_labels = "%H") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

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

vrij <- tables.zd.ren[[10]]

vrij$Day
