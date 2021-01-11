library(googleway)
library(dplyr)
library(tidyverse)
library(spatstat)
library(maptools)
library(here)
library(sf)
library(tmap)
library(units)
library(sp)
library(spdep)
library(reshape2)
library(OpenStreetMap)
library(tmaptools)


#London geography
London_borough <- st_read(here::here("Desktop/CASA0005/statistical-gis-boundaries-london/ESRI","London_Borough_Excluding_MHW.shp"))
london_outline <- London_borough %>% summarise(area = sum(HECTARES))
uk_bb <- st_bbox(London_borough)
osm_map <- read_osm(uk_bb)

#airbnb dataset
airbnb <- read.csv('Desktop/CASA0005/listings.csv')
airbnb <- airbnb[,6:16]
airbnb <- airbnb %>% filter(room_type == c('Private room','Entire home/apt'))  %>% filter(minimum_nights < 90)  %>% filter(as.Date(last_review) > '2019-07-01') %>% filter( availability_365 >= 180) %>% filter(price < 1000)
airbnb <- airbnb[,1:5]
airbnb <- airbnb %>% st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%st_transform(., 27700)

#airbnb density
ggplot(tt1, aes(x=V1,y=V2))+geom_point()+coord_equal()+stat_bin2d(bins=10)+ coord_cartesian(ylim=c(155000,200000),xlim = c(500000,560000))+scale_fill_gradient2(high = 'darkorange')+ggtitle( 'Density of Private Room')+xlab('')+ylab('')
ggplot(tt, aes(x=V1,y=V2))+geom_point()+coord_equal()+stat_bin2d(bins=10)+ coord_cartesian(ylim=c(155000,200000),xlim = c(500000,560000))+scale_fill_gradient2(high = '#004C99')+ggtitle( 'Density of Entire Home/apt')+xlab('')+ylab('')

#airbnb price
tm_shape(osm_map)+tm_rgb()+tm_shape(airbnb[airbnb$room_type == c('Private room'),])+tm_dots(col = 'price',breaks = c(0,50,100,200,500,1000),size = 0.2)+tm_layout(frame =  F,title = 'Private Room',legend.position = c('right','bottom'))
tm_shape(osm_map)+tm_rgb()+tm_shape(airbnb[airbnb$room_type == c('Entire home/apt'),])+tm_dots(col = 'price',palette = 'Blues',breaks = c(0,100,200,400,800,1000),size = 0.2)+tm_layout(frame =  F,title = 'Entire Home/apt',legend.position = c('right','bottom'))


#public transportation accessibility

api_key <- 'AIzaSyCANnaLbIZl-wgWa9S0Ka-yuNc2iTHyO20'

tube_station <- read.table('Desktop/CASA0005/undergroundStations.txt',header = T, sep = ',')
tube_station_4326 <- read.table('Desktop/CASA0005/undergroundStations.txt',header = T, sep = ',')
tube_station <- tube_station %>% st_as_sf(., coords = c("x", "y"), crs = 4326) %>%st_transform(., 27700)
tube_station =  tube_station[london_outline,] %>% st_intersection(london_outline)


#shortest tude distance
for(i in seq(1:nrow(airbnb_tmp))){
  tmp_origin = paste(airbnb_tmp$latitude[i],'+',airbnb_tmp$longitude[i],sep ='')
  distance_tmp <- c()
  for (j in seq(1:nrow(tube_station))){
    tmp_des = paste(tube_station_4326$y[j],'+',tube_station_4326$x[j],sep ='')
    distance_tmp[j] <- gmapsdistance(origin = tmp_origin , destination = tmp_des ,mode = "walking",key = api_key)$Distance
  }
  airbnb_tmp$distance[i] <- min(distance_tmp) 
  print(i)
}

#sightseeing spot accessibility
sightseeing <- read.csv('Desktop/CASA0005/london_sightseeing.csv')
sightseeing <- sightseeing %>% st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%st_transform(., 27700)

airbnb_buffer_2km <- st_buffer(airbnb, dist = 2000)

for(i in seq(1:nrow(airbnb))){
  airbnb$sighseeing_count[i] <- sum(as.integer(st_intersects(sightseeing, airbnb_buffer_2km[i,], sparse=FALSE)))
}

#crime data
crime_data <- read.csv('Desktop/CASA0005/london crime.csv')
airbnb <- left_join(airbnb,crime_data,by = c('neighbourhood' = 'borough.name'))
London_borough <- left_join(London_borough,crime_data,by = c('NAME' = 'borough.name'))


#cluster

airbnb_cluster <- data.frame(airbnb$price,airbnb$sighseeing_count,airbnb$cime.rate.per.100,airbnb$shortest_tube_distance)
colnames(airbnb_cluster) <- c('price','sighseeing_count','crime_rate','shortest_tube_distance')
airbnb_cluster <- scale(airbnb_cluster) 

wss_values <- list()
for (i in 1:15) {
  wss_values[i] <- sum(kmeans(airbnb_cluster,centers=i)$withinss)
}

# vector to dataframe
wss_values <- as.data.frame(wss_values)

# transpose
wss_values <- as.data.frame(t(wss_values))

# add cluster numbers
wss_values$cluster <- seq.int(nrow(wss_values))
names(wss_values) <- c('wss','cluster')

# plot elbow plot to decide the number of clusters
ggplot(data=wss_values, aes(x=cluster,y=wss)) +
  geom_point() +
  geom_path() + 
  scale_x_continuous(breaks=seq(1,15)) +
  xlab('number of clusters') +
  ylab('within sum of squares')


#set the cluster number as 6
for (i in 1:10){
  print(paste0('starting run: ', i))
  clust7 <- kmeans(x=airbnb_cluster, centers=6, iter.max=1000000, nstart=1)
  fit[i] <- clust7$tot.withinss
  if (fit[i] < min(fit[1:(i-1)])){
    clusters <- clust7}
}

airbnb$cluster <- clusters$cluster


# assign to new variable for clarity
kfit <- clusters
# cluster sizes
kfit_size <- kfit$size
# inspect
kfit_size

kfit_mean<- as_tibble(aggregate(airbnb_cluster,by=list(kfit$cluster),FUN=mean))
names(kfit_mean)[1] <- 'cluster'
kfit_mean


# transform shape to tidy format
kfit_mean_long <- pivot_longer(kfit_mean, cols=(-cluster))

# plot mean feature values of each cluster
ggplot(kfit_mean_long, aes(x=cluster, y=value, colour=name)) + 
  geom_line () +
  scale_x_continuous(breaks=seq(1,6,by=1)) +
  theme_minimal() +
  theme(legend.title = element_blank())

#plot each cluster
tm_shape(London_borough)+tm_borders()+tm_shape(airbnb) + tm_bubbles(col = 'cluster',size = 0.2, palette='RdYlBu' )
