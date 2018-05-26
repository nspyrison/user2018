#####UseR2018 Datathon
#https://user2018.r-project.org/datathon/ #datathon
#https://docs.google.com/forms/d/e/1FAIpQLSe9r5xwi6I0-CpJIDcGBatKmM3UX2IjC7_zlj1_4p42txMGZw/viewform #submission form
#https://github.com/AtlasOfLivingAustralia/ALA4R # ala4r

###going to common name: 
#vignette("ALA4R")
#library("ALA4R")


#library(tidyverse)
#library(tibble)
library(ggplot2)
library(visdat)
library(GGally)
library(magrittr)
library(dplyr)
library(lubridate)

#reptile_brief <- read.csv(file=".//datathon//data//Reptiles-brief.csv", header=TRUE, sep=",") #takes about 10 sec or so?

###dimdate
reptile_brief$date <- as.Date(reptile_brief$eventDate, "%Y-%m-%d")
reptile_brief$wday <- 
  lubridate::wday(reptile_brief$date, label = TRUE, week_start=1)
reptile_brief$day0 <- 
  as.integer(reptile_brief$date-min(reptile_brief$date))
reptile_brief$year <- lubridate::year(reptile_brief$date)
reptile_brief$month <- lubridate::month(reptile_brief$date, label = TRUE)

###filtering
reptile_brief <- reptile_brief %>% filter(coordinateUncertaintyInMeters<2001)
reptile_brief <- reptile_brief %>% filter(decimalLongitude<1000)
reptile_brief <- reptile_brief %>% filter(decimalLongitude>100)
reptile_brief <- reptile_brief %>% filter(decimalLatitude<0)
reptile_brief <- reptile_brief %>% filter(year>1950)

samp <- reptile_brief[sample(nrow(reptile_brief), 5000), ] %>% 
 as.data.frame()

#save(samp, file ="./datathon/data/samp.rda")
load(file ="./datathon/data/samp.rda")

xymap <- ggplot(samp, aes(y=decimalLatitude, x=decimalLongitude)) + 
  geom_point(alpha=.1) +coord_fixed()
xymap

hist(samp$coordinateUncertaintyInMeters)

for (i in 1:9) print(length(unique(samp[,i]))/nrow(samp)) #pct of 

ggplot(samp, aes(x=year)) + geom_histogram()




visdat::vis_dat(samp)
GGally::ggpairs(data = samp[, c(6:8)])

??get_map
library(ggmap)
map <- get_map(location = 'Australia', zoom = 4)
mapPoints <- ggmap(map) + geom_point(aes(x = lon, y = lat, size = sitenum), alpha = .5)

###
load(file = "data/bal_posts_raw.rda")
bal_posts_clean <- bal_posts_raw
#str(bal_posts_clean)
bal_posts_clean$from_id <- as.integer(bal_posts_clean$from_id) #ALL NA after
#bal_posts_clean <- rename(bal_posts_clean, "created_datetime" = "created_time")
#bal_posts_clean <- rename(bal_posts_clean, id = form_message_id)
bal_posts_clean$created_datetime <- bal_posts_clean$created_time
bal_posts_clean$created_date <- substr(bal_posts_clean$created_datetime, 1, 10)
bal_posts_clean$created_time <- substr(bal_posts_clean$created_datetime, 11, 24)
bal_posts_clean$created_date <- as.Date(bal_posts_clean$created_date, "%Y-%m-%d")
bal_posts_clean$wday <- 
  lubridate::wday(bal_posts_clean$created_date, label = TRUE)
bal_posts_clean$day0 <- 
  as.integer(bal_posts_clean$created_date-min(bal_posts_clean$created_date))
bal_posts_clean$likes_count <- as.integer(bal_posts_clean$likes_count)
bal_posts_clean$comments_count <- as.integer(bal_posts_clean$comments_count)
bal_posts_clean$shares_count <- as.integer(bal_posts_clean$shares_count)
save(bal_posts_clean, file = "data/bal_posts_clean.rda")



###
p <- plot_geo(samp, sizes = c(1, 250)) %>%
  add_markers(alpha = .1,
    x = ~decimalLongitude, y = ~decimalLatitude, #size = ~pop, color = ~family, 
    hoverinfo = "text", text = ~paste(df$family, "<br />")
  ) #%>%
  #layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
p


####
library(plotly)
library(gapminder)

p <- gapminder %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
p
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="animations-mulitple-trace")
chart_link

library(ggplot2)
library(maps)
library(ggthemes)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(x = lon, y = lat, size = followers),
             data = rladies, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

