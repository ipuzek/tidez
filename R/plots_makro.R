library(ggplot2)
library(padr)

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

