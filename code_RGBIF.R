## Código para descargar datos desde GBIF usando R
## Escoger el directorio de trabajo (EJEMPLO:)
dir.principal<-"C:/Downloads/Mapas"
ruta.datos<- paste(dir.principal, "/datos/", sep="")
setwd(ruta.datos)

##Install lybraries (Instalar bibliotecas)
##Option 1
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall ("rgbif")
loadandinstall ("sp")
loadandinstall ("rgeos")
loadandinstall ("maptools")
loadandinstall ("ggplot2")
loadandinstall ("rworldmap")
loadandinstall ("rworldxtra")
loadandinstall ("RColorBrewer")
loadandinstall ("MASS")
loadandinstall ("GISTools")
loadandinstall ("rgdal")
loadandinstall ("raster")
loadandinstall ("dismo")

##Option 2
install.packages("pacman")
library("pacman")
p_load("sf", "raster", "sp", "dplyr", "ggplot2")

##Download data from GBIF (Descargar datos desde GBIF)
sp<- gbif("Buteo albigula", download = T, geo = T, sp=F)
head(name_suggest(q='Buteo albigula'))
ek<-occ_search(taxonKey= 2480535,return="all",limit=200000,hasCoordinate=TRUE)
write.csv(c(ek$data,ek$meta),file="C:/Users/fam/Downloads/Mapas/datos/Buteo_albigula.csv")
datos<-cbind(ek$data,ek$meta)
dups <- duplicated(datos[,3:4])
datos2<-cbind(dups,datos)
datos3<-subset(datos,!duplicated(datos[,3:4]))
datos4<-datos3[complete.cases(datos3[,3:4]),]
datos5<-datos4[(datos4[,3:4]>0.00000 | datos4[,3:4]<0.00000),]
datos5<-datos5[complete.cases(datos5[,3:4]),]
datos51<-datos5[!is.na(datos5$decimalLatitude),]
coordinates(datos51)<- ~decimalLongitude+decimalLatitude

##Plot the data of the chosen species on a world map (Graficar los datos de la especie escogida en un mapa mundial.)
data(wrld_simpl)
plot(wrld_simpl)
plot(datos51[,3])
mapa<-plot(wrld_simpl,xlim=c(-150,150),ylim=c(-90,90),axes=TRUE,col="green")
puntos<-points(datos51$decimalLongitude,datos5$decimalLatitude, col="red", pch=20, cex=0.9)
title(main= "Registros a nivel mundial de Buteo albigula desde GBIF", cex.main = 1, font.main = 1)
north.arrow(xb=-100, yb=100, len=3, lab="N",cex.lab=0.8,col="black")
map.scale (xc=-130, yc=-50, ft2km(280000), "10000 km", 1, 1)

##Plot the data of the chosen species on a country map, for example Colombia (Graficar los datos de la especie escogida en un mapa de un país, por ejemplo Colombia.)
Colombia<-wrld_simpl[wrld_simpl$NAME=="Colombia", ]
crs(datos51) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
crs(Colombia) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
plot(Colombia, border="grey", axes=TRUE,col="green")
Colab<-datos51[Colombia, ]
plot(Colab, add=T, col="blue", pch=20, cex=0.9)
title(main= "Registros en Colombia de Buteo albigula desde GBIF", cex.main = 1, font.main = 1)
north.arrow(xb=-78, yb=10, len=0.5, lab="N",cex.lab=0.8,col="black")
map.scale (xc=-80, yc=-3, ft2km(9000), "300 km", 1, 1)

##Download data of my specie in other country (Descargar datos de mi espeie en otro país, por ejemplo Peru, ojo en inglés el nombre.)
Peru<-wrld_simpl[wrld_simpl$NAME=="Peru", ]
crs(datos51) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
crs(Peru) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
plot(Peru, border="grey", axes=TRUE,col="green")
Perab<-datos51[Peru, ]
plot(Perab, add=T, col="blue", pch=20, cex=0.9)
title(main= "Registros en Peru de Buteo albigula desde GBIF", cex.main = 1, font.main = 1)
north.arrow(xb=-80, yb=-2, len=0.5, lab="N",cex.lab=0.8,col="black")
map.scale (xc=-80, yc=-15, ft2km(9000), "300 km", 1, 1)

### If a want plot two species in a same map (Si quiero graficar dos especies diferentes en un mismo mapa)
sp<- gbif("Cephalopterus penduliger", download = T, geo = T, sp=F)
head(name_suggest(q='Cephalopterus penduliger')) 
ek<-occ_search(taxonKey= 5959195,return="all",limit=200000,hasCoordinate=TRUE)
write.csv(c(ek$data,ek$meta),file="C:/Users/fam/Downloads/Mapas/AVES/Cephalopterus penduliger.csv")
datos<-cbind(ek$data,ek$meta)
dups <- duplicated(datos[,3:4])
datos2<-cbind(dups,datos)
datos3<-subset(datos,!duplicated(datos[,3:4]))
datos4<-datos3[complete.cases(datos3[,3:4]),]
datos5<-datos4[(datos4[,3:4]>0.00000 | datos4[,3:4]<0.00000),]
datos5<-datos5[complete.cases(datos5[,3:4]),]
datos5<-datos5[complete.cases(datos5[,3:4]),]
datos51<-datos5[!is.na(datos5$decimalLatitude),]
coordinates(datos51)<- ~decimalLongitude+decimalLatitude

sp<- gbif("Puffinus nativitatis", download = T, geo = T, sp=F)
head(name_suggest(q='Puffinus nativitatis ')) 
ek2<-occ_search(taxonKey= 5229340,return="all",limit=200000,hasCoordinate=TRUE)
write.csv(c(ek2$data,ek2$meta),file="C:/Users/fam/Downloads/Mapas/AVES/Puffinus nativitatis.csv")
datos2<-cbind(ek2$data,ek2$meta)
dups2 <- duplicated(datos2[,3:4])
datos2b<-cbind(dups2,datos2)
datos3b<-subset(datos2b,!duplicated(datos2b[,3:4]))
datos4b<-datos3b[complete.cases(datos3b[,3:4]),]
datos5b<-datos4b[(datos4b[,3:4]>0.00000 | datos4b[,3:4]<0.00000),]
datos5b<-datos5b[complete.cases(datos5b[,3:4]),]
datos5b<-datos5b[complete.cases(datos5b[,3:4]),]
datos51b<-datos5b[!is.na(datos5b$decimalLatitude),]
coordinates(datos51b)<- ~decimalLongitude+decimalLatitude

data(wrld_simpl)
plot(wrld_simpl)
plot(datos51[,3])
plot(datos51b[,3])
mapa<-plot(wrld_simpl,xlim=c(-150,150),ylim=c(-100,100),axes=TRUE,col="white")
puntos<-points(datos51$decimalLongitude,datos5$decimalLatitude, col="red", pch=20, cex=1.5)
puntos2<-points(datos51b$decimalLongitude,datos5b$decimalLatitude, col="blue", pch=20, cex=0.9)
title(main= "Registros mundiales de Cephalopterus penduliger (rojo) y Puffinus nativitatis (azul) ", cex.main = 1, font.main = 1)
north.arrow(xb=-140, yb=95, len=3, lab="N",cex.lab=0.8,col='black')
map.scale(xc=-130, yc=-30,ft2km(28000),"10000 Km", 1, 1)

##Upload specific layers, for example Colombia (Cargar una capa de Colombia con regiones especificas)
## Download layers of https://geoportal.igac.gov.co/contenido/consulta-de-planchas or https://geoportal.igac.gov.co/ (Descargar capas específicas de Colombia)
##Read the file Shapefile
col<-st-read(file.chose())
View(col)
names(col)
ggplot2:: ggplot(col)+
geom_sf()

## Extract specific layer of "departamentos" (Extraer una capa específica de departamentos)
tol <-col %>% filter(DPTO_CNMBR == "TOLIMA")
View (tol)
plot (tol)
ggplot2::ggplot(Tol)+
geom_sf()

##Extract specific layer of "municipios" (Extraer capa específica de municipio)
mun <- tol %>% filter (MPIO_CNMBR %in% c(2VILLAHERMOSA", "IBAGUE"))
ggplot2:: ggplot(mun)+
geom sf()

###FUENTE DE LOS DATOS - Modificado de: ############
### ASesoría: https://github.com/anarvaezv
## Chamberlain, S. (2017). rgbif: Interface to the Global Biodiversity Information Facility API. R package version 0.9.8. https://CRAN.R-project.org/package=rgbif
## Grajales, eduardo (2021) Seleccionar en Qgis y Filtrar en R shapes de áreas de interés https://www.youtube.com/watch?v=rWdJSg5Gu9c
##Hijmans, R. J., & Elith, J. (2015). Species distribution modeling with R. https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf. 
## Noguera, E. (2015). Descarga y limpieza de datos GBIF en R Cran. https://sites.google.com/site/elkalexnoguera/home/Trucos_R_Cran/descarga-y-limpieza-de-datos-gbif-en-r-cran
