library(terra)

list_raster<-list.files("C:/Users/Rober/Desktop/QGIS_progetto/dati_creati/modello_r10m_ritagliato/", pattern = ".tif", full.names = T)#carica le classificazioni (i raster tiff che risultano dallo script precedente)
list_raster<-list_raster[grep("aux.xml",list_raster,invert=TRUE)]#esclude i file che di informazione xml che a noi non servono
res_fiori<-matrix(ncol = 3) #crea una matrice vuota dove verranno salvati i risultati
res_fiori<-data.frame(res_fiori)#trasforma la matrice vuota in tabella (sempre vuota) dove verranno salvati i risultati
colnames(res_fiori)<-c("siepi","resto", "area")#do il nome alle colonne 

for (i in 1:length(list_raster)) {
  a<- rast(list_raster[i])
  areaname<-basename(list_raster[i])
  ncell(a)
  p <- as.polygons(a)
  count<-expanse(p)
  count[1]
  res_fiori[i,1]<-count[1]
  res_fiori[i,2]<-count[2]
  res_fiori[i,3]<-areaname
}

setwd("C:/Users/Rober/Desktop/QGIS_progetto/dati_creati/tabella_area_siepi")#scrivo il path dove il dato mi verrà salvato
write.table(res_fiori, "tabella_finale_area_siepi.csv")
