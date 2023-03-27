library(raster)
library(rgdal)
library(sf)
library(sp)
library(RStoolbox)
library(rasterVis)
library(mapview)
library(data.table)
library(RColorBrewer)
library(plotly)
library(grDevices)
library(caret)
library(randomForest)#mis
library(ranger)
library(MLmetrics)
library(nnet)
library(NeuralNetTools)
library(LiblineaR)
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(snow)
library(parallel)
library(tidyr)
library(maptools)



b2<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B02_20m.jp2")
b3<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B03_20m.jp2")
b4<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B04_20m.jp2")
b5<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B05_20m.jp2")
b6<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B06_20m.jp2")
b7<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B07_20m.jp2")
b8<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B8A_20m.jp2")
b11<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B11_20m.jp2")
b12<-raster("C:/Users/Rober/Desktop/R20m/T31UFS_20210530T104619_B12_20m.jp2")

brick_for_prediction_norm<-stack(b2,b3,b4,b5,b6,b7,b8,b11,b12)

poly<-shapefile("C:/Users/Rober/Desktop/QGIS_progetto/dati_creati/aree_di_saggio/VA3/habitat_va3_31N.shp")
poly@data$id <- as.integer(factor(poly@data$id))
setDT(poly@data)

brick_for_prediction_norm<-crop(brick_for_prediction_norm, poly)

ptsamp1<-subset(poly, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
#saveRDS(ptsamp1_1, file=paste0 ("C:/Users/MicTorresani/OneDrive - Scientific Network South Tyrol/unibo/master students/roberta/dati/punti_random", nome, "_ptsamp1_1.rds"))

ptsamp2<-subset(poly, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
#saveRDS(ptsamp1_1, file=paste0 ("C:/Users/MicTorresani/OneDrive - Scientific Network South Tyrol/unibo/master students/roberta/dati/punti_random", nome, "_ptsamp1_1.rds"))

ptsamp3<-subset(poly, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
#saveRDS(ptsamp3_3, file=paste0 ("C:/Users/MicTorresani/OneDrive - Scientific Network South Tyrol/unibo/master students/roberta/dati/punti_random", nome, "_ptsamp3_3.rds"))

ptsamp4<-subset(poly, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
#saveRDS(ptsamp4_4, file=paste0 ("C:/Users/MicTorresani/OneDrive - Scientific Network South Tyrol/unibo/master students/roberta/dati/punti_random", nome, "_ptsamp4_4.rds"))




#in quest parte prendo le informazioni dei pixel dove il punto random è caduto e lo salvo in un dataframe. 
dt1 <- brick_for_prediction_norm %>% 
  raster::extract(y = ptsamp1_1) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp1_1@data] # add the class names to each row

dt2 <- brick_for_prediction_norm %>% 
  raster::extract(y = ptsamp2_2) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp2_2@data]

dt3 <- brick_for_prediction_norm %>% 
  raster::extract(y = ptsamp3_3) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp3_3@data] # add the class names to each row

dt4 <- brick_for_prediction_norm %>% 
  raster::extract(y = ptsamp4_4) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp4_4@data] # add the class names to each row


#merge i due dataframe in un unico dataframe. Do il nome fiori e grass all'id 1 e 2
dt<-rbind(dt1, dt2, dt3, dt4)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b','c','d' ))


#inizio random forest
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(dt$class,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)


dt_train <- dt[idx_train]
dt_test <- dt[-idx_train]


# create cross-validation folds (splits the data into n random groups)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

model_rf <- caret::train(class ~ . , method = "rf", data = dt_train, #neural network o vector machine
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)


saveRDS(model_rf, file = paste0("C:/Users/Rober/Desktop/QGIS_progetto/dati_creati/modello/","model_rf_","va3",".rds")) 


predict_rf <- raster::predict(object = brick_for_prediction_norm,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/Rober/Desktop/QGIS_progetto/dati_creati/modello/","va3_classification",".tiff"),overwrite=T )
