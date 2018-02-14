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

#===========================================================================================#
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


#===========================================================================================#
set.seed(171221)

state <- map_data("state")
nyc <- state %>% filter(region=="new york")
county <- map_data("county")
nyc_county <- county %>% filter(region=="new york")

out <- tr_pcd %>%
  filter((pickup_longitude > max(nyc$long) | pickup_longitude < min(nyc$long)) | (pickup_latitude > max(nyc$lat) | pickup_latitude < min(nyc$lat)))
tr_pcd <- tr_pcd %>%
  filter(!id %in% out$id)

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

c_num <- 30 #number of centroids to use
p_cl_info <- p_cl_info %>%
  arrange(desc(size)) %>%
  head(n=c_num) %>%
  mutate(center=c(1:c_num))

d_cl_info <- d_cl_info %>%
  arrange(desc(size)) %>%
  head(n=c_num) %>%
  mutate(center=c(1:c_num))

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

temp <- tr_pcd %>%
  group_by(p_hour) %>%
  mutate(m_td=mean(trip_duration)) %>%
  select(p_hour, m_td) %>%
  distinct()

loess_smth <- loess(as.numeric(temp$m_td)~as.numeric(temp$p_hour), span=0.5)

tr_pcd$p_hour_smth <- predict(loess_smth, (as.numeric(as.character(tr_pcd$p_hour))+1))
tr_pcd$vendor_id <- as.factor(ifelse(tr_pcd$vendor_id==2, 1, 0))
tr_pcd <- tr_pcd %>%
  mutate(p_hour=as.factor(p_hour)) %>%
  mutate(log_dist=log10(dist+1)) %>%
  mutate(p_month=as.factor(p_month))
tr_onehot <- data.frame(cbind(with(tr_pcd, model.matrix(~p_wday+p_hour+p_month+p_cluster+d_cluster + 0))))
tr_data <- data.frame(cbind(tr_pcd %>% select(p_hour_smth, dist, log_dist, passenger_count, vendor_id, flg, 
                                              pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude), tr_onehot))
tr_id <- tr_pcd %>% select(id)
tr_lab <- log(tr_pcd$trip_duration+1)

te_pcd$p_hour_smth <- predict(loess_smth, (as.numeric(as.character(te_pcd$p_hour))+1))
te_pcd$vendor_id <- as.factor(ifelse(te_pcd$vendor_id==2, 1, 0))
te_pcd <- te_pcd %>%
  mutate(p_hour=as.factor(p_hour)) %>%
  mutate(log_dist=log10(dist+1)) %>%
  mutate(p_month=as.factor(p_month))
te_onehot <- data.frame(cbind(with(te_pcd, model.matrix(~p_wday+p_hour+p_month+p_cluster+d_cluster + 0))))
te_data <- data.frame(cbind(te_pcd %>% select(p_hour_smth, dist, log_dist, passenger_count, vendor_id, flg, 
                          pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude), te_onehot))
te_id <- te_pcd %>% select(id)
#===========================================================================================#

#===========================================================================================#
tr_matrix <- as(as.matrix(tr_data), "dgCMatrix")
te_matrix <- as(as.matrix(te_data), "dgCMatrix")
tr_matrix <- xgb.DMatrix(data=tr_matrix, label=tr_lab)

set.seed(171221)
tr_pcd2 <- tr_pcd
tr_num <- sample(nrow(tr_pcd2), 1000000)

tr_data2 <- tr_data[tr_num,]
tr_lab2 <- tr_lab[tr_num]
va_data <- tr_data[-tr_num,]
va_lab <- tr_lab[-tr_num]

tr2_matrix <- as(as.matrix(tr_data2), "dgCMatrix")
tr2_matrix <- xgb.DMatrix(data=tr2_matrix, label=tr_lab2)
va_matrix <- as(as.matrix(va_data), "dgCMatrix")
va_matrix <- xgb.DMatrix(data=va_matrix, label=va_lab)

param <- list(objective = "reg:linear",
              eval_metric = "rmse", 
              booster = "gbtree",
              eta = 0.5, #learning rate
              gamma = 0.01,
              colsample_bytree = 0.8, #variables per tree 
              subsample = 0.8, #data subset per tree 
              max_depth = 20, #tree levels
              seed = 171221)
#watchlist <- list(train=tr2_matrix, valid=va_matrix)

cv.res <- xgb.cv(params=param, data=tr_matrix, nfold=5, early_stopping_rounds=5, nrounds=10)
cv.res

cv.res$evaluation_log %>%
  select(iter, train_rmse_mean, test_rmse_mean) %>%
  melt(id.vars="iter", measure.vars=c("train_rmse_mean", "test_rmse_mean")) %>%
  ggplot() +
    geom_line(aes(x=iter, y=value, group=variable, colour=variable)) + 
    scale_y_log10()

#===========================================================================================#
param <- list(objective = "reg:linear",
              eval_metric = "rmse",
              booster = "gbtree",
              eta = 0.03,
              gamma = 0.01,
              colsample_bytree = 0.8,
              subsample = 0.8,
              max_depth = 20,
              seed = 171221)

#cv.res <- xgb.cv(params=param, data=tr_matrix, nfold=5, early_stopping_rounds=3, nrounds=100)
setwd("/Users/hodong/Desktop/jupyter_prac/kaggle_taxi/")
model <- xgboost(data=tr_matrix, nrounds=200, params=param, verbose=1, print_every_n=5)
xgb.save(model, "taxi_xgb")
model <- xgb.load("taxi_xgb")
model #niter : 200
model <- xgb.train(data=tr_matrix, label=tr_pcd$trip_duration, nrounds=30, params=param, verbose=1, xgb_model=model, print_every_n = 5,
                   watchlist = watchlist)

#model <- xgboost(data=tr_matrix, label=log(tr_pcd$trip_duration+1), nrounds=200, params=param, verbose=1) #RMSLE로 활용
xgb_temp <- predict(model, te_matrix)
xgb_result <- data.frame(id=te_id, trip_duration=(exp(xgb_temp)-1))
write.csv(xgb_result, file="xgb_result2.csv", row.names=FALSE)


imp_matrix <- as.tibble(xgb.importance(feature_names = colnames(tr_data), model = model))

imp_matrix %>%
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Features", y = "Importance")

imp_matrix <- rbind(imp_matrix, data.frame(
  Feature=c('p_hour', 'p_wday', 'p_month', 'p_cluster', 'd_cluster'),
  Gain=c(sum(imp_matrix[grep('p_hour', imp_matrix$Feature),]$Gain),
         sum(imp_matrix[grep('p_wday', imp_matrix$Feature),]$Gain),
         sum(imp_matrix[grep('p_month', imp_matrix$Feature),]$Gain),
         sum(imp_matrix[grep('p_cluster', imp_matrix$Feature),]$Gain),
         sum(imp_matrix[grep('d_cluster', imp_matrix$Feature),]$Gain)),
  Cover=c(sum(imp_matrix[grep('p_hour', imp_matrix$Feature),]$Cover),
          sum(imp_matrix[grep('p_wday', imp_matrix$Feature),]$Cover),
          sum(imp_matrix[grep('p_month', imp_matrix$Feature),]$Cover),
          sum(imp_matrix[grep('p_cluster', imp_matrix$Feature),]$Cover),
          sum(imp_matrix[grep('d_cluster', imp_matrix$Feature),]$Cover)),
  Frequency=c(sum(imp_matrix[grep('p_hour', imp_matrix$Feature),]$Frequency),
              sum(imp_matrix[grep('p_wday', imp_matrix$Feature),]$Frequency),
              sum(imp_matrix[grep('p_month', imp_matrix$Feature),]$Frequency),
              sum(imp_matrix[grep('p_cluster', imp_matrix$Feature),]$Frequency),
              sum(imp_matrix[grep('d_cluster', imp_matrix$Feature),]$Frequency)))) %>%
  filter(Feature %in% c("p_hour_smth", "dist", "log_dist", "passenger_count", "vendor_id", "flg", 
                        "pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude", 
                        "p_hour", "p_wday", "p_month", "p_cluster", "d_cluster"))

imp_matrix <- imp_matrix  %>%
  filter(Feature %in% c("p_hour_smth", "dist", "log_dist", "passenger_count", "vendor_id", "flg", 
                        "pickup_longitude", "pickup_latitude", "dropoff_longitude", "dropoff_latitude", 
                        "p_hour", "p_wday", "p_month", "p_cluster", "d_cluster"))

imp_matrix %>%
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Features", y = "Importance")

xgb.plot.tree(feature_names=colnames(tr_data), model=model)
