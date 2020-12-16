loadandinstall  ("raster")
loadandinstall ("sp")
loadandinstall ("dismo")
loadandinstall ("rgbif")
loadandinstall ("maptools")
loadandinstall ("rgeos")

sp<- gbif("Eretmochelys","imbricata", download = T, geo = T, sp=F)
head(name_suggest(q='Eretmochelys imbricata'))
ek<-occ_search(taxonKey= 2442207,return="all",limit=200000,hasCoordinate=TRUE)
write.csv(c(ek$data,ek$meta),file="C:/Users/fam/Downloads/Mapas_datosGBIF/datos_GBIF/Eretmochelys imbricata.csv")
datos<-cbind(ek$data,ek$meta)
dups <- duplicated(datos[,3:4])
datos2<-cbind(dups,datos)
datos3<-subset(datos,!duplicated(datos[,3:4]))
datos4<-datos3[complete.cases(datos3[,3:4]),]
datos5<-datos4[(datos4[,3:4]>0.00000 | datos4[,3:4]<0.00000),]
datos5<-datos5[complete.cases(datos5[,3:4]),]
datos51<-datos5[!is.na(datos5$decimalLatitude),]
coordinates(datos51)<- ~decimalLongitude+decimalLatitude

data(wrld_simpl)
plot(wrld_simpl)
plot(datos51[,3])
mapa<-plot(wrld_simpl,xlim=c(-150,150),ylim=c(-90,90),axes=TRUE,col="green")
puntos<-points(datos51$decimalLongitude,datos5$decimalLatitude, col="red", pch=20, cex=0.9)
title(main= "Ruta migratoria mundial de Eretmochelys imbricata", cex.main = 1, font.main = 1)

Colombia<-wrld_simpl[wrld_simpl$NAME=="Colombia", ]
proj4string(datos51)<- proj4string(Colombia)
plot(Colombia, border="grey", axes=TRUE,col="green")
Colab<-datos51[Colombia, ]
plot(Colab, add=T, col="blue", pch=20, cex=0.9)
title(main= "Ruta migratoria en Colombia Eretmochelys imbricata", cex.main = 1, font.main = 1)

###FUENTE DE LOS DATOS - Modificado de: ############
## Chamberlain, S. (2017). rgbif: Interface to the Global Biodiversity Information Facility API. R package version 0.9.8. https://CRAN.R-project.org/package=rgbif
##Hijmans, R. J., & Elith, J. (2015). Species distribution modeling with R. https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf. 
## Noguera, E. (2015). Descarga y limpieza de datos GBIF en R Cran. https://sites.google.com/site/elkalexnoguera/home/Trucos_R_Cran/descarga-y-limpieza-de-datos-gbif-en-r-cran
