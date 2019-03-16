rm( list = ls())

install.packages("tidyverse")
install.packages("rvest")
install.packages("magrittr")
install.packages("ggmap")
install.packages("stringr")
install.packages("htmr")
install.packages("maps")
install.packages("googleAuthR")
library("tidyverse")
library("rvest")
library("magrittr")
library("ggmap")
library("maps")
library("stringr")
library("googleAuthR")


options("googleAuthR.client_id" = "YOUR_CLIENT_ID")
options("googleAuthR.client_secret" = "YOUR_CLIENT_SECRET")



html.global_talent <- read_html("https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos")

df.global_talent_RAW <- html.global_talent %>%
  html_nodes("table") %>%
  extract2(1) %>%
  html_table()



print(df.global_talent_RAW)

# X1              X2  X3          X4
#  1     Switzerland   6   Australia
#  2       Singapore   7  Luxembourg
#  3  United Kingdom   8     Denmark
#  4   United States   9     Finland
#  5          Sweden  10      Norway



df.global_talent_1 <- df.global_talent_RAW %>% select(X1, X2) %>% rename(rank = X1, country = X2)
df.global_talent_2 <- df.global_talent_RAW %>% select(X3, X4) %>% rename(rank = X3, country = X4)



#===========
# RECOMBINE
#===========

df.global_talent <- rbind(df.global_talent_1, df.global_talent_2)


# INSPECT
glimpse(df.global_talent)
print(df.global_talent)




glimpse(df.global_talent)

# Observations: 10
# Variables: 2
# $ rank     " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9", " 10"
# $ country  " Switzerland", " Singapore", " United Kingdom", " United States", " Sw...




df.global_talent <- df.global_talent %>% mutate(country = str_trim(country)
                                                ,rank = str_trim(rank)
)
print(df.global_talent)

map.world <- map_data("world")


# INSPECT
as.factor(df.global_talent$country) %>% levels()

# RECODE NAMES
df.global_talent$country <- recode(df.global_talent$country
                                   ,'United States' = 'USA'
                                   ,'United Kingdom' = 'UK'
)
map.world_joined <- left_join(map.world, df.global_talent, by = c('region' = 'country'))


map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))
head(map.world_joined)


df.country_points <- data.frame(country = c("Singapore","Luxembourg"),stringsAsFactors = F)
glimpse(df.country_points)


register_google(key = 'AIzaSyAFNLwIbEPtEvdtDaIdW6V8hob8egsC7DA')


geocode.country_points <- geocode(df.country_points$country)


df.country_points <- cbind(df.country_points,geocode.country_points)



ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
  geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = 'Countries with highest "talent competitiveness"'
       ,subtitle = "source: INSEAD, https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos") +
  theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )



