#for basic data manipuldation
require(stats)
require(plyr)
require(dplyr) 
require(lubridate) #for processing time-series data
require(geosphere)
require(reshape)
require(tibble)

#for basic visualization
require(extrafont) #for using 'Helvetica'
require(RColorBrewer)
require(ggplot2) #basic visualization
require(grid)

#for mapdata
require(maps)
require(mapdata)
require(leaflet) #real-time mapping

#for k-means, k-nn, and xgboost model
require(cluster)
require(class)
require(xgboost)

tr <- data.frame(read.csv("../raw_data/train.csv", header=TRUE))
te <- data.frame(read.csv("../raw_data/test.csv", header=TRUE))
sc <- data.frame(read.csv(url("http://web.mta.info/developers/data/nyct/subway/StationEntrances.csv")))
sc <- sc[,c(1:5)] 

glimpse(tr)
glimpse(te)
glimpse(sc)

#multiplot function
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots == 1) { print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col)) }}}

#Dividing datetime into hours, weekdays, and monthes
#processing some columns to analyze
tr_pcd <- tr %>%
  mutate(vendor_id=as.factor(vendor_id)) %>%
  mutate(flg=as.factor(ifelse(store_and_fwd_flag=="Y", 1, 0))) %>%
  mutate(p_hour=hour(pickup_datetime)) %>%
  mutate(p_wday=as.factor(weekdays(as.Date(pickup_datetime)))) %>%
  mutate(p_wday=factor(p_wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(p_wday=revalue(p_wday, c("Monday"="MON", "Tuesday"="TUE", "Wednesday"="WED", "Thursday"="THU", "Friday"="FRI", "Saturday"="SAT", "Sunday"="SUN"))) %>%
  mutate(p_month=as.factor(month(pickup_datetime))) %>%
  mutate(dist=distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude))) %>%
  select(id, vendor_id, passenger_count, flg, p_hour, p_wday, p_month, dist, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude, trip_duration)

te_pcd <- te %>%
  mutate(vendor_id=as.factor(vendor_id)) %>%
  mutate(flg=as.factor(ifelse(store_and_fwd_flag=="Y", 1, 0))) %>%
  mutate(p_hour=hour(pickup_datetime)) %>%
  mutate(p_wday=as.factor(weekdays(as.Date(pickup_datetime)))) %>%
  mutate(p_wday=factor(p_wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(p_wday=revalue(p_wday, c("Monday"="MON", "Tuesday"="TUE", "Wednesday"="WED", "Thursday"="THU", "Friday"="FRI", "Saturday"="SAT", "Sunday"="SUN"))) %>%
  mutate(p_month=as.factor(month(pickup_datetime))) %>%
  mutate(dist=distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude))) %>%
  select(id, vendor_id, passenger_count, flg, p_hour, p_wday, p_month, dist, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude)


#===========================================================================================#
#single variables check
#vendor_id frequency
#vendor_id frequency
vi_freq <- tr_pcd %>% 
  group_by(vendor_id) %>%
  count() %>%
  ggplot(aes(vendor_id, n, fill=vendor_id)) +
  geom_col() + 
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="vendor_id frequency")

#trip_duration by vendor_id
vi_box <- tr_pcd %>% 
  group_by(vendor_id) %>% 
  ggplot(aes(vendor_id, trip_duration, group=vendor_id)) +
  geom_boxplot() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="Trip_duration by vendor_id") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

#mean(trip_duration) by vendor_id & p_hour
vi_line <- tr_pcd %>%
  group_by(vendor_id, p_hour) %>% 
  mutate(m_td = mean(trip_duration)) %>%
  ungroup() %>%
  ggplot(aes(p_hour, m_td, group=vendor_id, colour=vendor_id)) + 
  geom_line() +
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="trip_duration by p_hour & vendor_id")

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(vi_freq, vi_box, vi_line, layout=layout)
#===========================================================================================#

#===========================================================================================#
#store_and_fwd_flag frequency
sf_freq <- tr_pcd %>% 
  group_by(flg) %>%
  count() %>%
  ggplot(aes(flg, n, fill=flg)) +
  geom_col() + 
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="store_and_forward_flag frequency")

#trip_duration by store_and_fwd_flag
sf_box <- tr_pcd %>% 
  group_by(flg) %>% 
  ggplot(aes(flg, trip_duration, group=flg, colour=flg)) +
  geom_boxplot() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="Trip_duration by store_and_forward_flag") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

#mean(trip_duration) by store_and_forward_flag & p_hour
sf_line <- tr_pcd %>%
  group_by(flg, p_hour) %>% 
  mutate(m_td = mean(trip_duration)) %>%
  ungroup() %>%
  ggplot(aes(p_hour, m_td, group=flg, colour=flg)) + 
  geom_line() +
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="trip_duration by p_hour & store_and_forward_flag")

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(sf_freq, sf_box, sf_line, layout=layout)
#===========================================================================================#

#===========================================================================================#
#passenger_count frequency
pc_freq <- tr_pcd %>% 
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill=passenger_count)) +
  geom_col() + 
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="passenger_count frequency")

#trip_duration by passenger_count
pc_box <- tr_pcd %>% 
  group_by(passenger_count) %>% 
  ggplot(aes(passenger_count, trip_duration, group=passenger_count)) +
  geom_boxplot() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="Trip_duration by passenger_count") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

#mean(trip_duration) by passenger_count & p_hour
pc_line <- tr_pcd %>%
  filter(!passenger_count %in% c(0,7,8,9)) %>%
  group_by(passenger_count, p_hour) %>% 
  mutate(m_td = mean(trip_duration)) %>%
  ungroup() %>%
  ggplot(aes(p_hour, m_td, group=as.factor(passenger_count), colour=as.factor(passenger_count))) + 
  geom_line() +
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="right") + 
  labs(title="trip_duration by p_hour & passenger_count")

#mean(trip_duration) by passenger_count & p_wday
pc_line2 <- tr_pcd %>%
  filter(!passenger_count %in% c(0,7,8,9)) %>%
  group_by(passenger_count, p_wday) %>% 
  mutate(m_td = mean(trip_duration)) %>%
  ungroup() %>%
  ggplot(aes(p_wday, m_td, group=as.factor(passenger_count), colour=as.factor(passenger_count))) + 
  geom_line() +
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="right") + 
  labs(title="trip_duration by p_wday & passenger_count")

layout <- matrix(c(1,2,3,3,4,4), 3,2, byrow=TRUE)
multiplot(pc_freq, pc_box, pc_line, pc_line2, layout=layout)
#===========================================================================================#

#===========================================================================================#
#distance & trip_duration scatterplot
dist_td <- tr_pcd %>%
  ggplot(aes(dist, trip_duration)) +
  geom_point() + 
  theme_gray(base_family = "Helvetica") +
  labs(x = "haversine distance", y = "trip_duration") +
  labs(title="distance & trip_duration")

#distance & trip_duration scatterplot (log10)
dist_td_log <- tr_pcd %>%
  mutate(dist=dist+1) %>% #prevent being infinite value while log transformation
  ggplot(aes(dist, trip_duration)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() + 
  theme_gray(base_family = "Helvetica") +
  labs(x = "haversine distance", y = "trip_duration") +
  labs(title="log10(distance) & log10(trip_duration)")

layout <- matrix(c(1,2), 2,1, byrow=TRUE)
multiplot(dist_td, dist_td_log, layout=layout)
#===========================================================================================#

#===========================================================================================#
hour_freq <- tr_pcd %>%
  group_by(p_hour) %>%
  count() %>%
  ggplot(aes(p_hour, n, fill=p_hour)) +
  geom_col() + 
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="p_hour frequency")

hour_td <- tr_pcd %>%
  group_by(p_hour) %>%
  ggplot(aes(p_hour, trip_duration, group=p_hour)) +
  geom_boxplot() + 
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="Trip_duration by p_hour") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

hour_m_td <- tr_pcd %>%
  group_by(p_hour) %>%
  mutate(m_td=mean(trip_duration)) %>%
  ggplot(aes(p_hour, m_td, fill=p_hour)) +
  geom_point() + 
  geom_line() + 
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="mean(trip_duration) by p_hour") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

wday_freq <- tr_pcd %>%
  group_by(p_wday) %>%
  count() %>%
  ggplot(aes(p_wday, n, fill=p_wday)) +
  geom_col() + 
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="p_wday frequency")

wday_td <- tr_pcd %>%
  group_by(p_wday) %>%
  ggplot(aes(p_wday, trip_duration, group=p_wday)) +
  geom_boxplot() + 
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="Trip_duration by p_wday") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

wday_m_td <- tr_pcd %>%
  group_by(p_wday) %>%
  mutate(m_td=mean(trip_duration)) %>%
  ggplot(aes(p_wday, m_td, fill=p_wday)) +
  geom_point() + 
  geom_line() + 
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="mean(trip_duration) by p_wday") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

month_freq <- tr_pcd %>%
  group_by(p_month) %>%
  count() %>%
  ggplot(aes(p_month, n, fill=p_month)) +
  geom_col() + 
  scale_y_log10() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") + 
  labs(title="p_month frequency")

month_td <- tr_pcd %>%
  group_by(p_wday) %>%
  ggplot(aes(p_month, trip_duration, group=p_month)) +
  geom_boxplot() + 
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="Trip_duration by p_month") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

month_m_td <- tr_pcd %>%
  group_by(p_month) %>%
  mutate(m_td=mean(trip_duration)) %>%
  ggplot(aes(p_month, m_td, fill=p_month)) +
  geom_point() + 
  geom_line() + 
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="none") +
  scale_y_log10() + 
  labs(title="mean(trip_duration) by p_month") + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE)

layout <- matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=TRUE)
multiplot(hour_freq, hour_td, hour_m_td, wday_freq, wday_td, wday_m_td, month_freq, month_td, month_m_td, layout=layout)
#===========================================================================================#

#===========================================================================================#
#smoothing이 필요하겠다
temp <- tr_pcd %>%
  group_by(p_hour) %>%
  mutate(m_td=mean(trip_duration)) %>%
  select(p_hour, m_td) %>%
  distinct()

loess_smth <- loess(temp$m_td~temp$p_hour, span=0.5)

loess_line <- tr_pcd %>%
  group_by(p_hour) %>%
  mutate(m_td=mean(trip_duration)) %>%
  ungroup() %>%
  select(p_hour, m_td) %>%
  distinct() %>%
  ggplot() + 
  geom_point(aes(x=p_hour, y=m_td, group=p_hour)) +
  geom_smooth(aes(x=c(0:23), y=predict(loess_smth, c(0:23)), colour="grey10"), method="loess") + 
  labs(title="mean(trip_duration) by p_hour with loess smoothing function") + 
  theme(legend.position="none")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(loess_line, layout=layout)
#===========================================================================================#

#===========================================================================================#
#interaction relationship of p_datetime
hour_wday <- tr_pcd %>%
  group_by(p_hour, p_wday) %>%
  mutate(m_td=mean(trip_duration)) %>%
  ungroup() %>%
  ggplot(aes(p_hour, m_td, fill=p_wday, colour=p_wday)) +
  geom_line() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="right") +
  scale_y_log10() + 
  labs(title="Trip_duration by p_hour & p_wday")

hour_month <- tr_pcd %>%
  group_by(p_hour, p_month) %>%
  mutate(m_td=mean(trip_duration)) %>%
  ungroup() %>%
  ggplot(aes(p_hour, m_td, fill=as.factor(p_month), colour=as.factor(p_month))) +
  geom_line() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="right") +
  scale_y_log10() + 
  labs(title="Trip_duration by p_hour & p_month")

wday_month <- tr_pcd %>%
  group_by(p_wday, p_month) %>%
  mutate(m_td=mean(trip_duration)) %>%
  ungroup() %>%
  ggplot(aes(p_month, m_td, fill=p_wday, colour=p_wday)) +
  geom_point() +
  theme_gray(base_family = "Helvetica") +
  theme(legend.position="right") +
  scale_y_log10() + 
  labs(title="Trip_duration by p_wday & p_month")

layout <- matrix(c(1,2,3),3,1,byrow=TRUE)
multiplot(hour_wday, hour_month, wday_month, layout=layout)
#===========================================================================================#

#===========================================================================================#
freq_hmap <- tr_pcd %>%
  group_by(p_wday, p_hour) %>%
  count() %>%
  ggplot(aes(p_hour, p_wday, fill = n)) +
  geom_tile() +
  theme_gray(base_family = "Helvetica") +
  labs(x = "p_hour", y = "p_wday") +
  labs(title="Frequency heatmap") + 
  scale_fill_distiller(palette = "Spectral")

td_hmap <- tr_pcd %>%
  group_by(p_wday, p_hour) %>%
  mutate(m_td = mean(trip_duration)) %>%
  ggplot(aes(p_hour, p_wday, fill = m_td)) +
  geom_tile() +
  theme_gray(base_family = "Helvetica") +
  labs(x = "p_hour", y = "p_weekday") +
  labs(title="Trip_duration heatmap") + 
  scale_fill_distiller(palette = "Spectral")

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(freq_hmap, td_hmap, layout=layout)
#===========================================================================================#

#===========================================================================================#
#맵 정보를 불러온다.
state <- map_data("state")
nyc <- state %>% filter(region=="new york")
county <- map_data("county")
nyc_county <- county %>% filter(region=="new york")

#check overall coords on states map
#load map data
state <- map_data("state")
nyc <- state %>% filter(region=="new york")
county <- map_data("county")
nyc_county <- county %>% filter(region=="new york")

#check overall coords on states map
tr_coords <- nyc %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey", alpha=0.3) +
  geom_polygon(aes(x=long, y=lat, group=group), fill=NA, colour="white", size=0.3) + 
  coord_map() + 
  geom_point(data=tr_pcd, aes(x=pickup_longitude, y=pickup_latitude), colour="red", size=0.01) + 
  labs(title="training set coords") + 
  theme(legend.position="none")
#we can check some coords are outside the United States as well as NYC

set <- rbind(
  tr_pcd %>% select(pickup_longitude, pickup_latitude) %>% mutate(set="tr"),
  te_pcd %>% select(pickup_longitude, pickup_latitude) %>% mutate(set="te"))

nyc %>% 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey", alpha=0.3) +
  geom_polygon(aes(x=long, y=lat, group=group), fill=NA, colour="white", size=0.3) + 
  coord_map() + 
  geom_point(data=set, aes(x=pickup_longitude, y=pickup_latitude, group=set), size=0.01, alpha=0.5) + 
  labs(title="test set coords") + 
  theme(legend.position="none")


layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(tr_coords, te_coords, layout=layout)

#filter coords outside NYC 
out <- tr_pcd %>%
  filter((pickup_longitude > max(nyc$long) | pickup_longitude < min(nyc$long)) | (pickup_latitude > max(nyc$lat) | pickup_latitude < min(nyc$lat)))

#See those coords in details with real map 
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=out$pickup_longitude, lat=out$pickup_latitude, popup="coords outsides New York")
#이후 분석은 이 43개의 좌표들은 제외하고 진행한다.
tr_pcd <- tr_pcd %>%
  filter(!id %in% out$id)
#===========================================================================================#

#===========================================================================================#
#Processing location data with k_means clustering#
#Using original coords data is unmeaningful because infinite coords can exist on euclidean space
set.seed(171221)
k_val <- 100
p_loc <- tr_pcd %>% 
  select(pickup_longitude, pickup_latitude)
d_loc <- tr_pcd %>%
  select(dropoff_longitude, dropoff_latitude)

p_loc_clusters <- kmeans(p_loc, k_val)
d_loc_clusters <- kmeans(d_loc, k_val)
tr_pcd$p_cluster <- p_loc_clusters$cluster
tr_pcd$d_cluster <- d_loc_clusters$cluster

p_cl_info <- data.frame(center=c(1:k_val), pickup_longitude=p_loc_clusters$centers[,1], pickup_latitude=p_loc_clusters$centers[,2], size=p_loc_clusters$size)
d_cl_info <- data.frame(center=c(1:k_val), pickup_longitude=d_loc_clusters$centers[,1], pickup_latitude=d_loc_clusters$centers[,2], size=d_loc_clusters$size)

#cl_info의 center별 평균 trip_duration 속성 추가
p_cl_info <- tr_pcd %>%
  group_by(p_cluster) %>%
  mutate(m_td=mean(trip_duration)) %>%
  mutate(center=p_cluster) %>%
  ungroup() %>%
  select(center, m_td) %>%
  distinct() %>%
  merge(p_cl_info, by="center")

d_cl_info <- tr_pcd %>%
  group_by(d_cluster) %>%
  mutate(m_td=mean(trip_duration)) %>%
  mutate(center=d_cluster) %>%
  ungroup() %>%
  select(center, m_td) %>%
  distinct() %>%
  merge(d_cl_info, by="center")

leaflet(data=p_cl_info) %>%
  addTiles() %>%
  addMarkers(~pickup_longitude, ~pickup_latitude, popup=as.character(p_cl_info$center))

#k-means는 아웃라이어에 너무 영향을 많이 받는다. 실제로 leaflet을 통해 뉴욕을 한참 벗어난 지역이 보이기도...
#본래라면 outlier의 영향력을 희석시키기 위해 k-medoids를 사용해야하지만, 당장 r에선 사용할 수 있는 패키지가 없으므로 임시방편으로 belonging이 낮은 녀석들을 해체한다.

c_num <- 30 #number of centroids to use
p_cl_info <- p_cl_info %>%
  arrange(desc(size)) %>%
  head(n=c_num) %>%
  mutate(center=c(1:c_num))

d_cl_info <- d_cl_info %>%
  arrange(desc(size)) %>%
  head(n=c_num) %>%
  mutate(center=c(1:c_num))

leaflet() %>%
  addTiles() %>%
  addMarkers(lng=p_cl_info$pickup_longitude, lat=p_cl_info$pickup_latitude, popup=as.character(p_cl_info$center))
#===========================================================================================#

#===========================================================================================#
#centroid별 belonging의 개수 확인
nyc_county %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey", alpha=0.3) +
  geom_polygon(aes(x=long, y=lat, group=group), fill=NA, colour="white", size=0.3) + 
  coord_map() + 
  coord_cartesian(xlim=c(-74.05,-73.75), ylim=c(40.6, 40.85)) +
  geom_point(data=p_cl_info %>% arrange(size), 
             aes(x=pickup_longitude, y=pickup_latitude, size=size, color=desc(size)), alpha=0.7) +
  labs(title="centroids by # of allocated coords") + 
  theme(legend.position="none")

#centroid별 trip_duration의 평균 확인
nyc_county %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), fill="grey", alpha=0.3) +
  geom_polygon(aes(x=long, y=lat, group=group), fill=NA, colour="white", size=0.3) + 
  coord_map() + 
  coord_cartesian(xlim=c(-74.05,-73.75), ylim=c(40.6, 40.85)) +
  geom_point(data=p_cl_info %>% arrange(size), 
             aes(x=pickup_longitude, y=pickup_latitude, size=m_td, color=desc(m_td)), alpha=0.7) +
  labs(title="centroids by mean(trip_duration)") + 
  theme(legend.position="none", axis.text.x = element_te)
?element_text
#===========================================================================================#

#===========================================================================================#
#update the value with the new Centroid
tr_pcd$p_cluster <- knn(train=p_cl_info %>% select(pickup_longitude, pickup_latitude), 
                        test=tr_pcd %>% select(pickup_longitude, pickup_latitude),
                        cl=p_cl_info$center, k=1)

te_pcd$p_cluster <- knn(train=p_cl_info %>% select(pickup_longitude, pickup_latitude), 
                        test=te_pcd %>% select(pickup_longitude, pickup_latitude),
                        cl=p_cl_info$center, k=1)

tr_pcd$d_cluster <- knn(train=d_cl_info %>% select(pickup_longitude, pickup_latitude), 
                        test=tr_pcd %>% select(pickup_longitude, pickup_latitude),
                        cl=d_cl_info$center, k=1)

te_pcd$d_cluster <- knn(train=d_cl_info %>% select(pickup_longitude, pickup_latitude), 
                        test=te_pcd %>% select(pickup_longitude, pickup_latitude),
                        cl=d_cl_info$center, k=1)
#===========================================================================================#

#===========================================================================================#
