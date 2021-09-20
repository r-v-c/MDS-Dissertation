library("reshape2")
library("xts")
library("sp")
library("spacetime")
library("ggplot2")
library("dplyr")
library("gstat")
library("RColorBrewer")
library("STRbook")
library("tidyr")
library("glmnet")
library("animation")
library("dplyr")
library("ggplot2")
library("gstat")
library("maps")
library(corrplot)

###### STEP 1 : DATA WRANGLING ######

faV1 = read.csv(file.choose())
head(faV1)
numconf = as.numeric(factor(faV1$confidence))
numday = as.numeric(factor(faV1$acq_date))
dateacq = as.Date(faV1$acq_date)
head(faV1)
head(dateacq)

numday2 = numday*10000
timedate = faV1$acq_time + numday2
head(timedate)
summary(timedate)
max(faV1$acq_time)
timedate = ts(timedate)

acq_time2 = faV1$acq_time/100
head(acq_time2)
time = ts(acq_time2)
head(time)
class(time)
time2 = ts(faV1$acq_time)
head(time2, 10)
24*60*60
plot(timedate)

chtime = as.character(faV1$acq_time)
faV1$chtime = chtime
head(chtime)
newv <- do.call(paste,        # Applying do.call & paste functions
                c(faV1[c("chtime", "acq_date")],
                  sep = "-"))
head(newv)
newtimedate = as.Date(newv)
head(newtimedate)

newtimedatenum = as.numeric(newv)
#head(newtimedatenum)
timedatanew = as.xts(newtimedate, order.by = 'timeDate' , unique=FALSE)
head(timedatanew)

acq_time3<- faV1$acq_time*100
head(faV1$acq_time)
head(acq_time3)


hoursmins <- colsplit(faV1$acq_time,"",names=c("hours","mins"))
head(hoursmins, 100)
hoursmins$mins[200]
length(as.character(hoursmins$mins[200]))

hours = c()
hourmin <- paste(hoursmins$hours,":", hoursmins$mins)
head(hourmin)



mins <- as.POSIXct(hoursmins$mins, format = "%H:%M", origin = dateacq)
head(mins)
hours1 <- as.xts(hoursmins$hours, format = "%H", order.by = dateacq)
class(hours1)
hours1
mins1 <- as.xts(hoursmins$mins, format= "%M", order.by = dateacq)
mins1
hours2 <- as.character(hours1)
hours2


timev <- as.POSIXct(acq_time3, format = "%Y-%m-%d %H:%M", origin = dateacq)
head(timev, 10)

alldata = select(faV1, -c(acq_date, confidence, satellite, instrument, version))
alldata = cbind(alldata, dateacq, numconf, numday, timedate, timev)
alldata$Time <- format(as.POSIXct(alldata$timev), format = "%H:%M:%S") 
dim(alldata)
head(alldata)

numsat = as.numeric(factor(faV1$satellite))
numinst = as.numeric(factor(faV1$instrument))
numver = as.numeric(factor(faV1$version))
#numday = as.numeric(factor(data$daynight))
numconf = as.numeric(factor(faV1$confidence))
numdata = select(faV1, -c(dateacq), -c(satellite), -c(instrument), -c(confidence), -c(version), -c(acq_date))
numdata = cbind(numdata, numdate, numconf)
head(numdata)
par(mfrow = c(1,1))
corrplot(cor(numdata), method="number")
boxplot(bright_ti4 ~ confidence, data = faV1)
boxplot(bright_ti5 ~ confidence, data = faV1)


sep07 <- filter(alldata, dateacq<"2019-09-08")

head(sep07)
sep07conf <- ggplot(sep07) + # plot points
  geom_point(aes(x = longitude,y = latitude, col=as.factor(numconf)), # attribute color
             size = 2) + # make all points larger
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  ggtitle("Confidence") +
  #geom_path(data = map_data("state"), aes(x = 0.5, y=1)) + facet_grid(acq_date) + # facet by time
  theme_bw() # B&W theme

sep07conf


V1_ti4 <- ggplot(faV1) + # plot points
  geom_point(aes(x = longitude,y = latitude, # lon and lat
                 colour = bright_ti4), # attribute color
             size = 2) + # make all points larger
  col_scale() + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  ggtitle("Brightness (ti4)") +
  theme(plot.title = element_text(hjust = 0.5))+
  #geom_path(data = map_data("state"), aes(x = 0.5, y=1)) + facet_grid(acq_date) + # facet by time
  theme_bw() # B&W theme


##### empirical spatial means #####
spat_av <- group_by(sep07, latitude, longitude) %>% # group by lon-lat
  summarise(mu_emp = mean(bright_ti4)) # mean for each lon-lat

#plot average max temp per station
lat_means <- ggplot(spat_av) +
  geom_point(aes(latitude, mu_emp)) +
  xlab("Latitude (deg)") +
  ylab("Brightness (degK)") + theme_bw()
lon_means <- ggplot(spat_av) +
  geom_point(aes(longitude, mu_emp)) +
  xlab("Longitude (deg)") +
  ylab("Brightness (degK)") + theme_bw()
lat_means
lon_means


#### empirical temporal means  ####

#1.group data by time
#2. summarise
head(sep07)
V1_av <- group_by(sep07, numday) %>%
  summarise(meanV1bright = mean(bright_ti4))
head(V1_av)
#3.plot
gV1av <-
  ggplot() +
  geom_line(data = sep07, aes(x = numday, y = bright_ti4, group = longitude),
            colour = "blue", alpha = 0.04) +
  geom_line(data = V1_av, aes(x = numday, y = meanV1bright)) +
  xlab("Day") + ylab("Brightness (degK)") +
  theme_bw()
gV1av


##latitudinal hovmoller plot:
#1. generate regular grid of spatial and temporal points
lim_lat <- range(faV1$latitude) # latitude range
lim_t <- range(faV1$daynum) # time range

lim_t

lat_axis <- seq(lim_lat[1], # latitude axis
                lim_lat[2],
                length=25)
t_axis <- seq(lim_t[1], # time axis
              lim_t[2],
              length=100)
lat_t_grid <- expand.grid(latitude = lat_axis,
                          dateacq = t_axis)

#2. associate each data-points latitudinal coordinate with the one 
#closest on the grid:
#do this by finding disatnce from lat coord to each point of the grid, find which is closest, allocate
V1_grid <- faV1
dists <- abs(outer(faV1$latitude, lat_axis, "-")
)
V1_grid$latitude <- lat_axis[apply(dists, 1, which.min)]

#3. group by lat and time, then average over all station values
V1_lat_Hov <- group_by(V1_grid, latitude, daynum) %>%
  summarise(bright_ti4 = mean(bright_ti4))
#here there are no data points, when not the case, interpolate (akima package)

#4. plot
Hovmoller_lat <- ggplot(V1_lat_Hov) + # take data
  geom_tile(aes(x = latitude, y = daynum, fill = bright_ti4)) + # plot
  fill_scale(name = "degK") + # add color scale
  scale_y_reverse() + # rev y scale
  ylab("Day number (days)") + # add y label
  xlab("Latitude (degrees)") + # add x label
  theme_bw() # change theme

Hovmoller_lat


##longitudinal hovmoller plot:
#1. generate regular grid of spatial and temporal points
lim_lon <- range(faV1$longitude) # latitude range
#lim_t <- range(Tmax$t) # time range
lon_axis <- seq(lim_lon[1], # latitude axis
                lim_lon[2],
                length=25)
#t_axis <- seq(lim_t[1], # time axis
#              lim_t[2],
#              length=100)
lon_t_grid <- expand.grid(longitude = lon_axis,
                          daynum = t_axis)

#2. associate each data-points latitudinal coordinate with the one 
#closest on the grid:
#do this by finding disatnce from lat coord to each point of the grid, find which is closest, allocate
V1_grid2 <- faV1
dists2 <- abs(outer(faV1$longitude, lon_axis, "-")
)
V1_grid2$longitude <- lon_axis[apply(dists, 1, which.min)]

#3. group by lat and time, then average over all station values
V1data_lon_Hov <- group_by(V1_grid2, longitude, daynum) %>%
  summarise(bright_ti4 = mean(bright_ti4))
#here there are no data points, when not the case, interpolate (akima package)

#4. plot
Hovmoller_lon <- ggplot(V1data_lon_Hov) + # take data
  geom_tile(aes(x = longitude, y = daynum, fill = bright_ti4)) + # plot
  fill_scale(name = "degK") + # add color scale
  scale_y_reverse() + # rev y scale
  ylab("Day number (days)") + # add y label
  xlab("Longitude (degrees)") + # add x label
  theme_bw() # change theme

Hovmoller_lon


##### A N I M A T I O N S  #####

#1. define function that plots spatial map of max temp as function of time:
V1_t <- function(tau) {
  V1_sub <- filter(faV1, faV1$daynum == tau) # subset data
  ggplot(V1_sub) +
    geom_point(aes(x = longitude,y = latitude, colour = bright_ti4), # plot
               size = 4) + # pt. size
    col_scale(name = "brightness (kelvin)") +
    theme_bw() # B&W theme
}

#2. function that plots data for every day in data set
gen_anim <- function() {
  for(daynum in lim_t[1]:lim_t[2]){ # for each time point
    plot(V1_t(daynum)) # plot data at this time point
  }
}
ani.options(interval = 0.2) # 0.2s interval between frames
saveHTML(gen_anim(), # run the main function
         autoplay = FALSE, # do not play on load
         loop = FALSE, # do not loop
         verbose = FALSE, # no verbose
         outdir = ".", # save to current dir
         single.opts = "'controls': ['first', 'previous',
'play', 'next', 'last',
'loop', 'speed'],
'delayMin': 0",
         htmlfile = "faV1_ti4_anim.html") # save filename

#3. to view load 'NOAA_anim.html' from working directory










RMSE <- function (Y, Ypred)  {
  Ypred <- as.numeric(Ypred)
  Y <- as.numeric(Y)
  return(sqrt(mean((Y - Ypred)^2)))
}
MSE <- function (Y, Ypred)  {
  Ypred <- as.numeric(Ypred)
  Y <- as.numeric(Y)
  return(mean((Y - Ypred)^2))
}



4758/3*2
set.seed(0)
training.1 <- sample(1:4758, 3172)
tr1 = sep02[training.1,]
test1 = sep02[-c(training.1),]
nrow(test1)
set.seed(5)
training.2 <- sample(1:4758, 3172)
tr2 = sep02[training.2,]
test2 = sep02[-c(training.2),]
set.seed(397)
training.3 <- sample(1:4758, 3172)
tr3 = sep02[training.3,]
test3 = sep02[-c(training.3),]

tr1c = tr1
tr2c = tr2
tr3c = tr3
tst1c = test1
tst2c = test2
tst3c = test3

coordinates(tr1c) = ~ latitude + longitude
coordinates(tr2c) = ~ latitude + longitude
coordinates(tr3c) = ~ latitude + longitude
coordinates(tst1c) = ~ latitude + longitude
coordinates(tst2c) = ~ latitude + longitude
coordinates(tst3c) = ~ latitude + longitude

min(sep02$latitude)
max(sep02$longitude)

spv1 <- variogram(object = bright_ti4 ~ longitude + latitude, # fixed effect component
                  data = tr1c, cutoff = 100000, width = 3) 
spv2 <- variogram(object = bright_ti4 ~ longitude + latitude, # fixed effect component
                  data = tr2c, cutoff = 100000, width = 3) 
spv3 <- variogram(object = bright_ti4 ~ longitude + latitude, # fixed effect component
                  data = tr3c, cutoff = 100000, width = 3) 

plot(spv1, pch = 19)
plot(spv2, pch=19)
plot(spv3, pch=19)

tr1c$Time <- format(as.POSIXct(tr1c$timev), format = "%H:%M:%S") 
head(tr1c$Time)
head(tr1c$timev)

tv1 <- variogram(object = bright_ti4 ~ timedate, data = tr1c, cutoff = 10000, width = 3)
plot(tv1)
tv1.1 <- variogram(object = bright_ti4 ~ acq_time, data = tr1c, cutoff = 10000, width = 3)
plot(tv1.1)

tv1.2 <- variogram(object = bright_ti4 ~ Time, data = tr1c, cutoff = 1000000, width = 1)
plot(tv1.2)
tv2.2 <- variogram(object = bright_ti4 ~ Time, data = tr2c, cutoff = 1000000, width = 1)
plot(tv2.2)
tv3.2 <- variogram(object = bright_ti4 ~ Time, data = tr3c, cutoff = 1000000, width = 1)
plot(tv3.2)


tv1.3 <- variogram(object = bright_ti4 ~ timev, data = tr1c, cutoff = 1000000, width = 1)
plot(tv1.3)
tv2.3 <- variogram(object = bright_ti4 ~ timev, data = tr2c, cutoff = 1000000, width = 1)
plot(tv2.3)
tv3.3 <- variogram(object = bright_ti4 ~ timev, data = tr3c, cutoff = 1000000, width = 1)
plot(tv3.3)



?stConstruct
STobjtr1<- stConstruct(x = tr1, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field
STobjtr2<- stConstruct(x = tr2, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field
STobjtr3<- stConstruct(x = tr3, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field

nrow(sep07)
26852/3*2
set.seed(0)
training.4 <- sample(1:26582, 17901)
tr4 = sep07[training.4,]
test4 = sep07[-c(training.4),]
nrow(test4)
set.seed(5)
training.5 <- sample(1:26582, 17901)
tr5 = sep07[training.5,]
test5 = sep07[-c(training.5),]
set.seed(397)
training.6 <- sample(1:26582, 17901)
tr6 = sep07[training.6,]
test6 = sep07[-c(training.6),]

tr4c = tr4
tr5c = tr5
tr6c = tr6
tst4c = test4
tst5c = test5
tst6c = test6

dim(tr4)
test = data.frame()
s = sample(1:17901, 100)
class(s)
s
for (i in 1:100){
  y <- s[i] 
  z <- tr4[y,1:17]
  test[i,] <-z
} 
y = c()
for (i in 1:100){
  y[i] = s[i]
}
y
class(y)

nrow(test)
dim(test)
head(test)

test = tr4[s,1:17]
test
dim(test)  
?select

coordinates(tr4c) = ~ latitude + longitude
coordinates(tr5c) = ~ latitude + longitude
coordinates(tr6c) = ~ latitude + longitude
coordinates(tst4c) = ~ latitude + longitude
coordinates(tst5c) = ~ latitude + longitude
coordinates(tst6c) = ~ latitude + longitude

min(sep07$latitude)
max(sep07$longitude)

spv4 <- variogram(object = bright_ti4 ~ longitude + latitude, # fixed effect component
                  data = tr4c, cutoff = 100000, width = 3) 
spv5 <- variogram(object = bright_ti4 ~ longitude + latitude, # fixed effect component
                  data = tr5c, cutoff = 100000, width = 3) 
spv6 <- variogram(object = bright_ti4 ~ longitude + latitude, # fixed effect component
                  data = tr6c, cutoff = 100000, width = 3) 

plot(spv4, pch = 19)
plot(spv5, pch=19)
plot(spv6, pch=19)


tv4.3 <- variogram(object = bright_ti4 ~ timev, data = tr4c, cutoff = 1000000, width = 1)
plot(tv4.3)
tv5.3 <- variogram(object = bright_ti4 ~ timev, data = tr5c, cutoff = 1000000, width = 1)
plot(tv5.3)
tv6.3 <- variogram(object = bright_ti4 ~ timev, data = tr6c, cutoff = 1000000, width = 1)
plot(tv6.3)



?stConstruct
STobjtr1<- stConstruct(x = tr4, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field
STobjtr2<- stConstruct(x = tr5, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field
STobjtr3<- stConstruct(x = tr6, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field
head(tr4)
dim(STobjtr1)
?variogramST
max(timev)

training.7 <- sample(1:26582, 1790)
tr7 = sep07[training.7,]
test7 = sep07[-c(training.7),]
nrow(test7)
nrow(tr7)
set.seed(5)
training.8 <- sample(1:26582, 1790)
tr8 = sep07[training.8,]
test8 = sep07[-c(training.8),]
set.seed(397)
training.9<- sample(1:26582, 1790)
tr9 = sep07[training.9,]
test9 = sep07[-c(training.9),]

STobjtr7<- stConstruct(x = tr7, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field
STobjtr8<- stConstruct(x = tr8, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field
STobjtr9<- stConstruct(x = tr9, # data set
                       space = c("longitude", "latitude"), # spatial fields
                       time = "timev",
                       interval = FALSE) # time field

vvST7 <- variogramST(formula = bright_ti4 ~ 1 + latitude + longitude, # fixed effect component
                     data = STobjtr7, # July data
                     width = 10, # spatial bin (80 km)
                     cutoff = 1000, # consider pts < 1000 km apart
                     tlags = 1:8,
                     tunit = 'days') # 0 days to 6 days

vvST8 <- variogramST(formula = bright_ti4 ~ 1 + latitude + longitude, # fixed effect component
                     data = STobjtr8, # July data
                     width = 10, # spatial bin (80 km)
                     cutoff = 1000, # consider pts < 1000 km apart
                     tlags = 1:8,
                     tunit = 'days') # 0 days to 6 days
vvST9 <- variogramST(formula = bright_ti4 ~ 1 + latitude + longitude, # fixed effect component
                     data = STobjtr9, # July data
                     width = 10, # spatial bin (80 km)
                     cutoff = 1000, # consider pts < 1000 km apart
                     tlags = 1:8,
                     tunit = 'days') # 0 days to 6 days
plot(vvST7)
plot(vvST8)
plot(vvST9)



training.10 <- sample(1:26582, 5000)
tr10 = sep07[training.10,]
test10 = sep07[-c(training.10),]
nrow(test10)
nrow(tr10)
set.seed(5)
training.11 <- sample(1:26582, 5000)
tr11 = sep07[training.11,]
test11 = sep07[-c(training.11),]
set.seed(397)
training.12<- sample(1:26582, 5000)
tr12 = sep07[training.12,]
test12 = sep07[-c(training.12),]

STobjtr10<- stConstruct(x = tr10, # data set
                        space = c("longitude", "latitude"), # spatial fields
                        time = "timev",
                        interval = FALSE) # time field
STobjtr11<- stConstruct(x = tr11, # data set
                        space = c("longitude", "latitude"), # spatial fields
                        time = "timev",
                        interval = FALSE) # time field
STobjtr12<- stConstruct(x = tr12, # data set
                        space = c("longitude", "latitude"), # spatial fields
                        time = "timev",
                        interval = FALSE) # time field

vvST10 <- variogramST(formula = bright_ti4 ~ latitude + longitude, # fixed effect component
                      data = STobjtr10, # July data
                      width = 10, # spatial bin (80 km)
                      cutoff = 10000, # consider pts < 1000 km apart
                      tlags = 1:8,
                      tunit = 'days') # 0 days to 6 days

vvST11 <- variogramST(formula = bright_ti4 ~  latitude + longitude, # fixed effect component
                      data = STobjtr11, # July data
                      width = 10, # spatial bin (80 km)
                      cutoff = 10000, # consider pts < 1000 km apart
                      tlags = 1:8,
                      tunit = 'days') # 0 days to 6 days
vvST12 <- variogramST(formula = bright_ti4 ~ latitude + longitude, # fixed effect component
                      data = STobjtr12, # July data
                      width = 10, # spatial bin (80 km)
                      cutoff = 10000, # consider pts < 1000 km apart
                      tlags = 1:8,
                      tunit = 'days') # 0 days to 6 days
plot(vvST10)
plot(vvST11)
plot(vvST12)

sp.fit1 = fit.variogram(spv4, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
sp.fit2 = fit.variogram(spv5, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
sp.fit3 = fit.variogram(spv6, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

temp.fit1 = fit.variogram(tv4.3, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
temp.fit2 = fit.variogram(tv5.3, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
temp.fit3 = fit.variogram(tv6.3, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)


sp.fit1
sp.fit2
sp.fit3
temp.fit1
temp.fit2
temp.fit3

sepvgm7 <- vgmST(stModel = "separable",
                 space = vgm(psill = 302.6388, "Mat", 281.9383, nugget = 432.5438, kappa = 1),
                 time = vgm(psill = 0, "Mat", 13.01415, nugget = 411.8109, kappa = 0.9)
                 ,sill = 300)
sepvgm8 <- vgmST(stModel = "separable",
                 space = vgm(psill = 776.3008, "Mat", 154.9163, nugget = 437.0133, kappa = 1.3),
                 time = vgm(psill = 0, "Mat", 13.01341, nugget = 415.1228, kappa = 1)
                 ,sill = 1.6)
sepvgm9 <- vgmST(stModel = "separable",
                 space = vgm(psill = 759.8344, "Mat", 167.4368, nugget = 435.1083, kappa = 1.2),
                 time = vgm(psill =0.00002142269, "Exp", 10.99833, nugget = 413.06)
                 ,sill = 1.6)
sepMSE7 <- fit.StVariogram(vvST7, sepvgm7)
sepMSE7 <- attr(sepvgm7, "optim")$value
sepMSE7
sepMSE8 <- fit.StVariogram(vvST8, sepvgm8)
sepMSE8 <- attr(sepvgm8, "optim")$value
sepMSE8
sepMSE9 <- fit.StVariogram(vvST9, sepvgm9)
sepMSE9 <- attr(sepvgm9, "optim")$value
sepMSE9
sepMSE10 <- fit.StVariogram(vvST10, sepvgm7)
sepMSE10 <- attr(sepMSE10, "optim")$value
sepMSE10
sepMSE11 <- fit.StVariogram(vvST11, sepvgm8)
sepMSE11 <- attr(sepMSE11, "optim")$value
sepMSE11
sepMSE12 <- fit.StVariogram(vvST12, sepvgm9)
sepMSE12 <- attr(sepMSE12, "optim")$value
sepMSE12


STmodel1 = vgmST(stModel = "metric",
                 joint = vgm(psill=67.27358, "Gau", range = 518.3859, nugget = 443.23369),
                 sill = 67,
                 stAni = 410,
                 tunit = "days",
                 sunit = "km")
STmodel2 = vgmST(stModel = "metric",
                 joint = vgm(psill=67.27358, "Gau", range = 518.3859, nugget = 443.23369),
                 sill = 67,
                 stAni = 410,
                 tunit = "days",
                 sunit = "km")
STmodel3 = vgmST(stModel = "metric",
                 joint = vgm(psill=67.27358, "Gau", range = 518.3859, nugget = 443.23369),
                 sill = 67,
                 stAni = 410,
                 tunit = "days",
                 sunit = "km")
STm1  = fit.StVariogram(vvST4, STmodel1)
STm2  = fit.StVariogram(vvST5, STmodel2)
STm3  = fit.StVariogram(vvST6, STmodel3)
STMSE1 <- attr(STm1, "optim")$value
STMSE2 <- attr(STm2, "optim")$value
STMSE3 <- attr(STm3, "optim")$value
STm10  = fit.StVariogram(vvST10, STmodel1)
STm11  = fit.StVariogram(vvST11, STmodel2)
STm12  = fit.StVariogram(vvST12, STmodel3)
STm10
STm11
STm12
STMSE10 <- attr(STm10, "optim")$value
STMSE11 <- attr(STm11, "optim")$value
STMSE12 <- attr(STm12, "optim")$value
STMSE10
STMSE11
STMSE12

set.seed(397)
t.10 <- sample(1:nrow(test10), nrow(tr10))
t10new = test10[t.10,]
nrow(t10new)
nrow(tr10)
t.11 <- sample(1:nrow(test11), nrow(tr11))
t11new = test10[t.11,]
t.12 <- sample(1:nrow(test12), nrow(tr12))
t12new = test12[t.12,]

STtest10 <- stConstruct(t10new, space = c('latitude', 'longitude'), time = 'timev')
STtest11 <- stConstruct(t11new, space = c('latitude', 'longitude'), time = 'timev')
STtest12 <- stConstruct(t12new, space = c('latitude', 'longitude'), time = 'timev')
# 1. create space-time prediction grid
#(make sure coordinate reference system CRS of SpatialPoints is the same as the prediction grid)
spat_pred_grid10 <- expand.grid(
  lon = seq(114, 133, length = 20),
  lat = seq(-35, -26, length = 20)) %>%
  SpatialPoints(proj4string = CRS(proj4string(STtest10)))
gridded(spat_pred_grid10) <- TRUE

spat_pred_grid11 <- expand.grid(
  lon = seq(114, 133, length = 20),
  lat = seq(-35, -26, length = 20)) %>%
  SpatialPoints(proj4string = CRS(proj4string(STtest11)))
gridded(spat_pred_grid11) <- TRUE

spat_pred_grid12 <- expand.grid(
  lon = seq(114, 133, length = 20),
  lat = seq(-35, -26, length = 20)) %>%
  SpatialPoints(proj4string = CRS(proj4string(STtest12)))
gridded(spat_pred_grid12) <- TRUE

max(sep07$dateacq)
STobjtr10[1]
spat_pred_grid10

#For temporal grid, we consider 6 equally spaced days in July:
temp_pred_grid <- as.POSIXct("2019-09-01 09:26:40") + seq(0, 500000, length = 6)
temp_pred_grid

#combine spatial and temporal grids to construct STF object:
DE_pred10 <- STF(sp = spat_pred_grid10, # spatial part
                 time = temp_pred_grid) # temporal part

DE_pred11 <- STF(sp = spat_pred_grid11, # spatial part
                 time = temp_pred_grid) # temporal part

DE_pred12 <- STF(sp = spat_pred_grid12, # spatial part
                 time = temp_pred_grid) # temporal part


# 3. Now krige:
?krigeST
STobjtr10[[13]]


STobjtr10@data$lat <- STobjtr10@sp$latitude
STobjtr11@data$lat <- STobjtr11@sp$latitude
STobjtr12@data$lat <- STobjtr12@sp$latitude
STobjtr10@data$lon <- STobjtr10@sp$longitude
STobjtr11@data$lon <- STobjtr11@sp$longitude
STobjtr12@data$lon <- STobjtr12@sp$longitude
STtest10@data$lat <- STtest10@sp$latitude
STtest11@data$lat <- STtest11@sp$latitude
STtest12@data$lat <- STtest12@sp$latitude
STtest10@data$lon <- STtest10@sp$longitude
STtest11@data$lon <- STtest11@sp$longitude
STtest12@data$lon <- STtest12@sp$longitude

pred_kriged1 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STobjtr10, # data set w/o 14 July
                        newdata = DE_pred10, # prediction grid
                        modelList = sepvgm7, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances
pred_kriged2 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STobjtr11, # data set w/o 14 July
                        newdata = DE_pred11, # prediction grid
                        modelList = sepvgm8, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances
pred_kriged3 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STobjtr12, # data set w/o 14 July
                        newdata = DE_pred12, # prediction grid
                        modelList = sepvgm9, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances

pred_kriged4 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STobjtr10, # data set w/o 14 July
                        newdata = STtest10, # prediction grid
                        modelList = sepvgm7, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances
pred_kriged5 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STobjtr11, # data set w/o 14 July
                        newdata = STtest11, # prediction grid
                        modelList = sepvgm8, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances
pred_kriged6 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STobjtr12, # data set w/o 14 July
                        newdata = STtest12, # prediction grid
                        modelList = sepvgm9, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances

pred_kriged7 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STtest10, # data set w/o 14 July
                        newdata = DE_pred10, # prediction grid
                        modelList = sepvgm7, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances
pred_kriged8 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STtest11, # data set w/o 14 July
                        newdata = DE_pred11, # prediction grid
                        modelList = sepvgm8, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances
pred_kriged9 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                        data = STtest12, # data set w/o 14 July
                        newdata = DE_pred12, # prediction grid
                        modelList = sepvgm9, # semivariogram
                        computeVar = TRUE,
                        nmax = 100, 
                        tunits = 'days',
                        stAni = 1) # compute variances


pred_krigedf1 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                         data = STobjtr10, # data set w/o 14 July
                         newdata = STtest10, # prediction grid
                         modelList = sepvgm7, # semivariogram
                         computeVar = TRUE,
                         
                         tunits = 'days',
                         stAni = 1) # compute variances

pred_krigedf2 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                         data = STobjtr11, # data set w/o 14 July
                         newdata = STtest11, # prediction grid
                         modelList = sepvgm8, # semivariogram
                         computeVar = TRUE,
                         
                         tunits = 'days',
                         stAni = 1) # compute variances
pred_krigedf3 <- krigeST(bright_ti4 ~ lat + lon, # latitude trend
                         data = STobjtr12, # data set w/o 14 July
                         newdata = STtest12, # prediction grid
                         modelList = sepvgm9, # semivariogram
                         computeVar = TRUE,
                         
                         tunits = 'days',
                         stAni = 1) # compute variances


STVgmtest
pred_kriged_STV <- krigeST(bright_ti4 ~ 1 + lat, # latitude trend
                           data = STObj5, # data set w/o 14 July
                           newdata = DE_pred, # prediction grid
                           modelList = STVgmtest, # semivariogram
                           computeVar = TRUE,
                           nmax = 1000, 
                           tunit = 'days',
                           sunit = 'km',
                           stAni = 497) # compute variances
pred_kriged1
# 4. plot

color_pal <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(16))
stplot(pred_kriged1,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged1$se <- sqrt(pred_kriged1$var1.var)
stplot(pred_kriged1[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)


color_pal <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(16))
stplot(pred_kriged2,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged2$se <- sqrt(pred_kriged2$var1.var)
stplot(pred_kriged2[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

color_pal <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(16))
stplot(pred_kriged3,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged3$se <- sqrt(pred_kriged3$var1.var)
stplot(pred_kriged3[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)


stplot(pred_kriged4,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged4$se <- sqrt(pred_kriged4$var1.var)
stplot(pred_kriged4[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)


stplot(pred_kriged5,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged5$se <- sqrt(pred_kriged5$var1.var)
stplot(pred_kriged5[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

stplot(pred_kriged6,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged6$se <- sqrt(pred_kriged6$var1.var)
stplot(pred_kriged6[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

stplot(pred_kriged7,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged7$se <- sqrt(pred_kriged7$var1.var)
stplot(pred_kriged7[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

stplot(pred_kriged8,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged8$se <- sqrt(pred_kriged8$var1.var)
stplot(pred_kriged8[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

stplot(pred_kriged9,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_kriged9$se <- sqrt(pred_kriged9$var1.var)
stplot(pred_kriged9[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)


stplot(pred_krigedf1,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_krigedf1$se <- sqrt(pred_krigedf1$var1.var)
stplot(pred_krigedf1[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)


stplot(pred_krigedf2,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_krigedf2$se <- sqrt(pred_krigedf2$var1.var)
stplot(pred_krigedf2[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

stplot(pred_krigedf3,
       main = "Predictions (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

#to plot prediction (kriging) standard errors:
pred_krigedf3$se <- sqrt(pred_krigedf3$var1.var)
stplot(pred_krigedf3[, , "se"],
       main = "Prediction std. errors (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

nrow(pred_kriged3)
MSEvec1= c()
MAEvec1 = c()
n=length(pred_kriged1$var1.pred)
for (i in 1:n) {
  a = test10$bright_ti4[i]
  p =pred_kriged1$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec1[i] = MAE
  MSEvec1[i] = MSE
  
}

mean(MSEvec1)
sum(MSEvec1)
mean(MAEvec1)
sum(MAEvec1)



MSEvec2= c()
MAEvec2 = c()
# = nrow(tr2)
for (i in 1:n) {
  a = test11$bright_ti4[i]
  p =pred_kriged2$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec2[i] = MAE
  MSEvec2[i] = MSE
  
}

mean(MSEvec2)
sum(MSEvec2)
mean(MAEvec2)
sum(MAEvec2)
summary(pred_kriged1)


MSEvec3= c()
MAEvec3 = c()
#n = nrow(test3)
for (i in 1:n) {
  a = test12$bright_ti4[i]
  p =pred_kriged3$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec3[i] = MAE
  MSEvec3[i] = MSE
  
}

mean(MSEvec3)
sum(MSEvec3)
mean(MAEvec3)
sum(MAEvec3)


MSEvec4= c()
MAEvec4 = c()
#n = nrow(test10)
for (i in 1:n) {
  a = test10$bright_ti4[i]
  p =pred_kriged4$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec4[i] = MAE
  MSEvec4[i] = MSE
  
}

mean(MSEvec4)
sum(MSEvec4)
mean(MAEvec4)
sum(MAEvec4)

MSEvec5= c()
MAEvec5 = c()
#n = nrow(test11)
for (i in 1:n) {
  a = test11$bright_ti4[i]
  p =pred_kriged5$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec5[i] = MAE
  MSEvec5[i] = MSE
  
}

mean(MSEvec5)
sum(MSEvec5)
mean(MAEvec5)
sum(MAEvec5)

MSEvec6= c()
MAEvec6 = c()
#n = nrow(test12)
for (i in 1:n) {
  a = test12$bright_ti4[i]
  p =pred_kriged6$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec6[i] = MAE
  MSEvec6[i] = MSE
  
}

mean(MSEvec6)
sum(MSEvec6)
mean(MAEvec6)
sum(MAEvec6)


MSEvec7= c()
MAEvec7 = c()
n = nrow(test10)
for (i in 1:n) {
  a = test10$bright_ti4[i]
  p =pred_kriged7$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec7[i] = MAE
  MSEvec7[i] = MSE
  
}

mean(MSEvec7)
sum(MSEvec7)
mean(MAEvec7)
sum(MAEvec7)


MSEvec8= c()
MAEvec8 = c()
#n = nrow(test11)
for (i in 1:n) {
  a = test11$bright_ti4[i]
  p =pred_kriged8$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec8[i] = MAE
  MSEvec8[i] = MSE
  
}

mean(MSEvec8)
sum(MSEvec8)
mean(MAEvec8)
sum(MAEvec8)

MSEvec9= c()
MAEvec9 = c()
#n = nrow(test12)
for (i in 1:n) {
  a = test12$bright_ti4[i]
  p =pred_kriged9$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvec9[i] = MAE
  MSEvec9[i] = MSE
  
}

mean(MSEvec9)
sum(MSEvec9)
mean(MAEvec9)
sum(MAEvec9)


MSEvecf1= c()
MAEvecf1 = c()
n = nrow(t10new)
t10new$bright_ti4[6]
pred_krigedf1$var1.pred[6]
for (i in 1:4999) {
  a = t10new$bright_ti4[i]
  p =pred_krigedf1$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvecf1[i] = MAE
  MSEvecf1[i] = MSE
}
mean(MSEvecf1)
sum(MSEvecf1)
mean(MAEvecf1)
sum(MAEvecf1)

MSEvecf2= c()
MAEvecf2 = c()
#n = nrow(test12)
for (i in 1:n) {
  a = t11new$bright_ti4[i]
  p =pred_krigedf2$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvecf2[i] = MAE
  MSEvecf2[i] = MSE
}
mean(MSEvecf2)
sum(MSEvecf2)
mean(MAEvecf2)
sum(MAEvecf2)

MSEvecf3= c()
MAEvecf3 = c()
#n = nrow(test12)
for (i in 1:n) {
  a = t12new$bright_ti4[i]
  p =pred_krigedf3$var1.pred[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAEvecf3[i] = MAE
  MSEvecf3[i] = MSE
}
mean(MSEvecf3)
sum(MSEvecf3)
mean(MAEvecf3)
sum(MAEvecf3)

a = sum(t11new$bright_ti4)
p = sum(pred_krigedf2$var1.pred)
n = nrow(t10new)
m = length(pred_krigedf1$var1.pred)
MSE= ((a-p)^2) * (1/n)
n
m
sqrt(x)






stplot(STtest10,
       main = "Brightness (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)


stplot(STtest11,
       main = "Brightness (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)

stplot(STtest12,
       main = "Brightness (degrees Kelvin)",
       layout = c(3, 2),
       col.regions = color_pal)


RMSE(t10new$bright_ti4, pred_krigedf1$var1.pred)
RMSE(t11new$bright_ti4, pred_krigedf2$var1.pred)
RMSE(t12new$bright_ti4, pred_krigedf3$var1.pred)







head(tr10)
lm1= lm(data = tr12 , bright_ti4 ~ latitude + longitude)
#m.fit(x = lmf,  y = tr1)
#lm.fit
predict_lm1<-predict(lm1, newdata = test12)


MSElm1 = c()
MAElm1  = c()
n = nrow(test10)
for (i in 1:n) {
  a = test12$bright_ti4[i]
  p =predict_lm1[i]
  MSE =  (1/n) *((a-p)^2 )
  MAE = (1/n) *(a  - p)
  MAElm1[i] = MAE
  MSElm1[i] = MSE
  
}

mean(MSElm1)
sum(MSElm1 )
mean(MAElm1 )
sum(MAElm1 )



library(glmnet)
head(sep07)
y = sep07$bright_ti4
x = select(sep07,c('latitude', 'longitude', 'type', 'scan', 'track', 'acq_time','numday')) # Here we exclude the first column because it is the salary variable
head(x)

lasso = glmnet(x, y, alpha = 1) #1 is defualt so dont have to onclide alpha =1
names(lasso)
lasso$lambda
dim(lasso$beta)
lasso$beta[,1:3]   # this returns only the predictor coefficients
coef(lasso)[,1:3]  # this returns intercept + predictor coefficients

par(mfrow=c(1, 2))
plot(lasso, xvar = 'lambda')
plot(lasso, xvar = 'norm')  # or simply plot(lasso) because xvar = 'norm' is the default

# Lasso regression with cross-validation for lambda

set.seed(1)
class(x)
x = as.matrix(x)
class(x)
lasso.cv = cv.glmnet(x, y)
lasso.cv$lambda.min  #0.02761765
lasso.cv$lambda.1se  #0.4101134

round(cbind(
  coef(lasso.cv, s = 'lambda.min'),
  coef(lasso.cv, s = 'lambda.1se')),   # here we can also use coef(lasso.cv) becauce 'lambda.1se' is the default
  3)

par(mfrow=c(1,2))
plot(lasso.cv)
plot(lasso, xvar = 'lambda')
abline(v = log(lasso.cv$lambda.min), lty = 3) # careful to use the log here and below
abline(v = log(lasso.cv$lambda.1se), lty = 3)
# large diffence between the 2 lines, when this is the case the predictive performance
#is usually better on the line with fewer predictors


# Comparing predictive performance of min-CV lambda vs. 1-se lambda lasso

repetitions = 50
cor.1 = c()
cor.2 = c()

set.seed(29)                
for(i in 1:repetitions){
  # Step (i) data splitting
  training.obs = sample(1:506,  354)
  y.train = sep07$bright_ti4[training.obs]
  x.train = select(sep07[training.obs, ],c('latitude', 'longitude', 'type', 'scan', 'track', 'acq_time','numday'))
  x.train = as.matrix(x.train)
  y.test = sep07$bright_ti4[-training.obs]
  x.test = select(sep07[-training.obs, ],c('latitude', 'longitude', 'type', 'scan', 'track', 'acq_time','numday'))
  x.test = as.matrix(x.test)
  
  # Step (ii) training phase
  lasso.train = cv.glmnet(x.train, y.train)
  
  # Step (iv) generating predictions
  predict.1 = predict(lasso.train, x.test, s = 'lambda.min')
  predict.2 = predict(lasso.train, x.test, s = 'lambda.1se')
  
  
  # Step (v) evaluating predictive performance
  cor.1[i] = cor(y.test, predict.1)
  cor.2[i] = cor(y.test, predict.2)
}

par(mfrow = c(1,1))
boxplot(cor.1, cor.2, names = c('min-CV lasso','1-se lasso'), ylab = 'Test correlation', col = 7)



MSE(y.test, predict.1)
MSE(y.test, predict.2)
RMSE(y.test, predict.1)
RMSE(y.test, predict.2)
