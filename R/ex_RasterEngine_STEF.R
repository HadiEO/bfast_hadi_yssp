# read the raster stack of NDVI (normalised difference vegetation index,  30m resolution). It contains 183 NDVI layers, for 2013 through 2016, acquired by Landsat 7, 8 and Sentinel-2A sensors. Note that this test data set is pre-processed already, and is ready for analysis. Also note that the data set is already normalised spatially using the global spatial normalisation.

# re_test <- brick("C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/example_codes/L7L8S2_30m_ndvi_kafa_Global_subset.tif")

# extract a subset from the test data set

# re_testx <- crop(re_test, c(810000, 820000, 820000,830000))
# saveRDS(re_testx, "C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/example_codes/L7L8S2_30m_ndvi_kafa_Global_subset_cropped.tif")
re_testx <- readRDS("C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/example_codes/L7L8S2_30m_ndvi_kafa_Global_subset_cropped.tif")

# read the image acquistion dates. I save these in STEF/data directory

my_dates <- readRDS(file = "C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/example_codes/my_dates.rds")

#Plot the first layer in the raster stack

x11()
raster::plot(raster(re_testx, 1))

## We start the monitoring in 2016. Note this may take long, depending on your computer 


## sequential processing example:

# rad <- spatial.tools::rasterEngine(inraster=re_testx, fun=stef_monitor,window_dims=c(windowwidth=15,windowwidth =15),
#                     args=list(mYear = 2016,density = F,my_dates =my_dates,threshold = 0.05,spatiaNormPercentile =95, windowwidth=9,tryCatchError=F))


## paralell processing example:

## register the cores

sfQuickInit(cpus=4)

t <- system.time(
  rad <- spatial.tools::rasterEngine(inraster=re_testx, fun=stef_monitor, window_dims=c(windowwidth=9,windowwidth =9),
                    args=list(mYear = 2016,density = F,my_dates =my_dates,threshold = 0.05,spatiaNormPercentile =95, windowwidth=9,tryCatchError=T,sPatioNormalixse =F))
  )

## unregister the cores

sfQuickStop()

write_rds(rad, "C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/example_codes/rad_inExSTEFmonitor.rds")

# use random forest model to calculate the probability of forest --------
# disturbance

## read the training data (this data set was used by Hamunyela et al (2017))

training_data <- readRDS(file = "C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/example_codes/trainingData.rds") # RC ==Real change; FC == False change

# set the seed and training the random forest model
set.seed(100)

rf_model <- randomForest(label ~ ., data=training_data,  ntree = 501, importance=TRUE,
                         proximity=TRUE, probability = T)


# Apply the trained random forest model to the entire image 

## Exclude the first layer (date of change)
radx <-subset(rad, c(2:nlayers(rad)))

## rename the layers to the colnames in the training data
names(radx) <- colnames(training_data)[1:17]

## do the prediction
m3 <- predict(radx,rf_model, type='prob',na.rm=TRUE, index=2)

## plot the computed probability map for forest disturbance
plot (m3)

##Combine the date of change layer with the probability map for forest disturbance 

cDate <-subset(rad, 1)

cMap <- stack(cDate,m3)
names(cMap) <- c("Date_of_forest_disturbance", "Probability_of_forest_disturbance")
plot(cMap)
