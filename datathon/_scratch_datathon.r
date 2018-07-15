# ######UseR2018 Datathon
# browseURL("https://user2018.r-project.org/datathon/") # datathon
# browseURL("https://docs.google.com/forms/d/e/1FAIpQLSe9r5xwi6I0-CpJIDcGBatKmM3UX2IjC7_zlj1_4p42txMGZw/viewform") #submission form
# browseURL("https://www.ala.org.au/") # ala data
# browseURL("https://github.com/AtlasOfLivingAustralia/ALA4R") # ala4r package
# browseURL("https://en.wikipedia.org/wiki/Species_diversity") 
#   # wiki spieces diversity

# mangoDBlite

#library(tidyverse)
library(tibble)
library(ggplot2)
library(visdat)
library(GGally)
library(magrittr)
library(dplyr)
library(lubridate)
library(ggmap)

dummy <- 
  read.csv(file="./datathon/data/Birds-brief.csv", nrows=3, skip=0, header=T)
# birds_brief_m1 <-
#   read.csv(file="./datathon/data/Birds-brief.csv", nrows=1000000, skip=0)
# birds_brief_m2 <-
#   read.csv(file="./datathon/data/Birds-brief.csv", nrows=1000000, skip=1000000)
birds_brief_m3 <-
  read.csv(file="./datathon/data/Birds-brief.csv", nrows=1000000, skip=2000000)


dat <- birds_brief_m3
colnames(dat) <- colnames(dummy)
###dimdate
dat$date <- as.Date(as.character(dat$eventDate), "%Y-%m-%d")
dat$wday <- lubridate::wday(dat$date, label = TRUE, week_start=1)
dat$year <- as.integer(lubridate::year(dat$date) )
dat$quarter <- as.integer(lubridate::quarter(dat$date) )
dat$month <- lubridate::month(dat$date, label = TRUE)

###filtering
dat <- filter(dat, decimalLongitude < 1000)
dat <- filter(dat, decimalLongitude > 100)
dat <- filter(dat, decimalLatitude < 0)
dat <- filter(dat, decimalLatitude > -45)
#birbs
dat %<>% filter(year>1975)
dat %<>% filter(is.na(coordinateUncertaintyInMeters) | 
           coordinateUncertaintyInMeters<100)

#str(dat)
drop <- c("Record.ID","Data.Resource.ID","basisOfRecord","eventDate",
          "coordinateUncertaintyInMeters")
dat <- as.tibble(dat[, !(names(dat) %in% drop)] )

birds_brief_m3_clean <- dat

save(birds_brief_m3_clean,
    file = "./datathon/data/birds_brief_m3_clean.rda")

stop()
stop()
stop()
### EDA
load(file="./datathon/data/birds_brief_m1_clean.rda")
load(file="./datathon/data/birds_brief_m2_clean.rda")
load(file="./datathon/data/birds_brief_m3_clean.rda")


birds_brief_full_clean <- rbind(birds_brief_m1_clean, 
                                birds_brief_m2_clean,
                                birds_brief_m3_clean)

save(birds_brief_full_clean,
     file = "./datathon/data/birds_brief_full_clean.rda")

# dat <- birds_brief_m1_clean
# 
# dat <- filter(dat, decimalLongitude<155)
# dat <- filter(dat, decimalLongitude>140)
# dat <- filter(dat, decimalLatitude<-27.5)
# dat <- filter(dat, decimalLatitude>-40)
# #999836 to 985254 obs

load(file = "./datathon/data/birds_brief_full_clean.rda")

dat <- birds_brief_full_clean
samp <- sample_n(dat, 5000)
save(samp, file = "./datathon/data/samp_5k_of3m.rda")
load(file = "./datathon/data/samp_5k_of3m.rda")
dat <- samp
#unique(dat$scientificName)


GGally::ggpairs(data = dat[3:ncol(dat)])
#visdat::vis_dat(dat, warn_large_data = F) #bird NAs in uncertainty
# quantile(dat$decimalLatitude)

(g1 <- ggplot(dat, aes(x=year)) + geom_histogram(stat="count"))
(g2 <- ggplot(dat, aes(x=month)) + geom_histogram(stat="count"))
(g3 <- ggplot(dat, aes(x=wday)) + geom_histogram(stat="count"))


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

map_samp <- samp[, c(2,3,4,5,6,7,10)]
### MAPPING 101
xymap <- ggplot(map_samp, aes(y=decimalLatitude, x=decimalLongitude),
                alpha=.1) + coord_fixed() + geom_jitter(width=.5, height=.3) 
xymap

MASS::kde2d(xymap)
?kde2d

library(MASS)
f1 <- kde2d(x=map_samp$decimalLongitude, y=map_samp$decimalLatitude,
            h = rep(1.5, 2), n = 50, lims = c(0.5, 6, 0.5, 6))

??get_map
library(ggmap)
map <- get_map(location = 'Australia', zoom = 4)
ggmap <- ggmap(map) + 
  geom_jitter(data = dat, 
              aes(y=decimalLatitude, x=decimalLongitude, frame=year), 
              alpha=.1, width=.5, height=.3) + coord_fixed() 
plotly::ggplotly(ggmap)


xymap <- 
  ggplot(map_samp, aes(y=decimalLatitude, x=decimalLongitude, frame=year),
         alpha=.1) + coord_fixed() + geom_jitter(width=.5, height=.3) 
plotly::ggplotly(xymap)

###EXAMPLE
# ggmap(myMap) +
#   geom_point(data = df[, c("long","lat", "pop")], aes(x=long, y = lat, colour = pop > 1000000))



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

