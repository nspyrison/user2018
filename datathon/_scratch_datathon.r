#####UseR2018 Datathon
#https://user2018.r-project.org/datathon/ #datathon
#https://docs.google.com/forms/d/e/1FAIpQLSe9r5xwi6I0-CpJIDcGBatKmM3UX2IjC7_zlj1_4p42txMGZw/viewform #submission form
#https://www.ala.org.au/ ##DATA AL
#https://github.com/AtlasOfLivingAustralia/ALA4R # ala4r
#https://en.wikipedia.org/wiki/Species_diversity

###going to common name: 
#vignette("ALA4R")
#library("ALA4R")


#library(tidyverse)
library(tibble)
library(ggplot2)
library(visdat)
library(GGally)
library(magrittr)
library(dplyr)
library(lubridate)
library(ggmap)

#reptile_brief <- read.csv(file=".//datathon//data//Reptiles-brief.csv", header=TRUE, sep=",") #takes about 10 sec or so?
#birds_brief <- read.csv(file.choose(), nrows=200000)

dat <- birds_brief

###dimdate
dat$date <- as.Date(dat$eventDate, "%Y-%m-%d")
dat$wday <- lubridate::wday(dat$date, label = TRUE, week_start=1)
dat$day0 <- as.integer(dat$date-min(dat$date))
dat$year <- lubridate::year(dat$date) %>% as.integer()
dat$month <- lubridate::month(dat$date, label = TRUE)

###filtering
dat %<>% filter(decimalLongitude<1000)
dat %<>% filter(decimalLongitude>100)
dat %<>% filter(decimalLatitude<0)
dat %<>% filter(decimalLatitude>-45)
quantile(dat$decimalLatitude)
##reptile
#dat %<>% filter(year>1950) 
#dat %<>% filter(coordinateUncertaintyInMeters<2001)
#birbs
dat %<>% filter(year>1975) 
dat %<>% filter(is.na(coordinateUncertaintyInMeters)
                |coordinateUncertaintyInMeters<100)

str(dat)
drop <- c("Data.Resource.ID","basisOfRecord","eventDate",
          "coordinateUncertaintyInMeters")
dat <- dat[, !(names(dat) %in% drop)]
str(dat)

### EDA
GGally::ggpairs(data = dat[4:ncol(dat)])
visdat::vis_dat(dat, warn_large_data = F) #bird NAs in uncertainty

ggplot(dat, aes(x=year)) + geom_histogram(stat="count")
ggplot(dat, aes(x=month)) + geom_histogram(stat="count")
ggplot(dat, aes(x=wday)) + geom_histogram(stat="count")

### sciname
sciname <- table(dat$scientificName) %>% as.data.frame() %>% filter(Freq > 0) %>% arrange(-Freq) 
ggplot(sciname, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
sciname %<>% filter(Freq > 100)
ggplot(sciname, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### family
family <- table(dat$family) %>% as.data.frame() %>% filter(Freq > 0) %>% arrange(-Freq) 
ggplot(family, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
family %<>% filter(Freq > 100)
ggplot(family, aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


save(dat, file ="bird_200k_clean.rda") 
#"./datathon/data/bird_20k_clean.rda"

#set.seed(123)
#samp <- dat[sample(nrow(dat), 5000), ] %>% as.data.frame()
#
#save(samp, file ="./datathon/data/bird_samp.rda")
##save(samp, file ="./datathon/data/samp.rda")
##load(file ="./datathon/data/samp.rda")

### EDA
visdat::vis_dat(dat, warn_large_data = F) #bird NAs in uncertainty
GGally::ggpairs(data = samp[, c(6:8)]) 

### MAPPING 101
xymap <- ggplot(dat, aes(y=decimalLatitude, x=decimalLongitude), alpha=.1) +
  coord_fixed()
xymap

??get_map
library(ggmap)
map <- get_map(location = 'Australia', zoom = 4)
ggmap <- ggmap(map) + 
  geom_point(data = dat, aes(y=decimalLatitude, x=decimalLongitude), alpha=.1) + 
  coord_fixed()

ggmap(myMap) +
  geom_point(data = df[, c("long","lat", "pop")], aes(x=long, y = lat, colour = pop > 1000000))



### ELSE
for (i in 1:9) print(length(unique(samp[,i]))/nrow(samp)) #pct of 
ggplot(samp, aes(x=year)) + geom_histogram()

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
