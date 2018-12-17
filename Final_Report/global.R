#Set up library for all data

library(tidyverse)
library(magrittr)
library(readr)
library(scales)
library(knitr)
library(leaflet)
library(geojsonio)
library(moments)
library(benford.analysis)
library(plotly)
library(DT)
options("scipen"=100, "digits"=4)
########################################################
imdb <- read_csv("movies.csv")

imdb.budget <- imdb %>% filter(budget != 0)

position <- c("89","94","00","04","09","14")

table.budget <- read_csv("table.budget.csv")

imdb.company <- imdb %>% dplyr::select(company,year,budget,gross) %>% filter(budget != 0) %>% arrange(desc(budget)) 

comp <- imdb %>% arrange(desc(budget)) 
comp <- comp[1:100,] 
comp <- comp %>% mutate(n = 1) %>% group_by(company) %>% mutate(count = sum(n)) %>% filter(count != 1) %>% dplyr::select(-c(count,n)) %>% ungroup() %>% group_by(company,year)%>% mutate(gross = sum(gross)) %>% ungroup() %>% group_by(company,year,gross) %>% summarise(budget = sum(budget))


imdb.map <- read_csv("imdb.map.csv")
wm <- geojsonio::geojson_read("WorldMap.geo.json", what = "sp")
wm@data <- left_join(wm@data,imdb.map, by ="name")

##rating

imdb.rating <- imdb %>% filter(rating == "PG"|rating == "PG-13"|rating == "R") 

imdb.rv <- imdb.rating %>% group_by(year,rating) %>% mutate(allvote = sum(votes)) %>% ungroup() %>% group_by(year,rating,allvote) %>% summarise(count = n()) %>% mutate(average = allvote/count)

imdb.rs <- imdb.rating %>% group_by(year,rating) %>% mutate(allscore = sum(score)) %>% ungroup() %>% group_by(year,rating,allscore) %>% summarise(count = n()) %>% mutate(average = allscore/count)

imdb.score <- imdb %>% dplyr::select(name,
                                     director,
                                     rating,
                                     country,
                                     score,
                                     company,
                                     year)%>% 
  filter(rating == "PG"|rating == "PG-13"|rating == "R")


imdb.benfold <- imdb %>% select(year,votes,score)
########################################################
##Genre
#From plot, we know how is the distribution of Years about different genres movies in this density plot. We can know from plot that before 2000, the drama ,musical,Romance and Thriller are more popular than after 2000. Some Scentific Movie become more popular from 2000 to 2010.


## Leaflet Map
bins <- c(1,5,10,25,50,100,200,300,1000,Inf)
pal <- colorBin("YlGnBu", domain = wm@data$count, bins = bins)

country <- leaflet(wm) %>%
  setView(41.5,12.6,2) %>% 
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(count),
              label = ~paste0("Movies made by ",name, ":", formatC(count))) %>%
  addLegend(pal = pal,title = "# of Movies", values = ~count, opacity = 1.0)

## rating 

bar.rating <- ggplot(data = imdb.rating)+ geom_bar(aes(x = year,fill = rating),position = "fill")+ labs(x = "Year", y = "Oroportion of three Movie Types", title = "The ratio of three movie types each year") +  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12), legend.title = element_text(size =18))

density.rating <- ggplot(data = imdb.rating) + geom_density(aes(x = year,color = rating))+ labs(x = "Year", y = "density of three Movie Types", title = "The density of three movie types each year") +  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12), plot.title = element_text(size = 15, face = "bold"))


## benfold 's law
bfd.voter <- benford(imdb.benfold$votes, number.of.digits = 2)
plot(bfd.voter, multiple = FALSE)
bfd.voter

ana <- data.frame(bfd.voter$MAD,bfd.voter$distortion.factor)
colnames(ana)<- c("MAD", "distortion.factor")

b <- ggplot(data = imdb.benfold)+geom_histogram(aes(x = votes),bins = 100, color = "blue") + labs(x = "Votes", y = "Number of Movies")

votedis <- ggplotly(b)

suspects_ranked <- suspectsTable(bfd.voter)
kable(head(suspects_ranked))
suspects <- getSuspects(bfd.voter,imdb.score)

sodata  <- DT::datatable(bfd.voter$s.o.data, options = list(lengthMenu = c(5, 10,15), pageLength = 5))

data <- DT::datatable(bfd.voter$data, options = list(lengthMenu = c(5, 10,15), pageLength = 5))

suspect <- DT::datatable(suspects, options = list(lengthMenu = c(5, 10,15), pageLength = 5))

bfd <- DT::datatable(bfd.voter$bfd, options = list(lengthMenu = c(5, 10,15), pageLength = 5))