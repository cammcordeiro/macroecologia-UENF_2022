
# Obtendo dados e criando mapas

##Instalando pacotes
#instalando pacotes
install.packages("devtools")
install.packages("rgdal")
install.packages("maptools")
install.packages("maps")
library(devtools)
install_github("macroecology/letsR",dependencies = T)
#install.packages("letsR")#o pacote do repositorio Git
#é o mais atual

#carregando pacotes usados
library(letsR)
library(maptools)
library(maps)
library(rgdal)


##Dados de peixes recifais
###Onde encontrar os dados

###Preparando e baixando o arquivo
#criar pasta
dir.create(file.path(getwd(),"/data"))


###Exemplo baixando dados da IUCN (CASO PRECISEM!!!)
# URL <- "http://bit.ly/1HSxNlK"#dados IUCN cecilias
# download.file(URL,destfile="data/GYMNOPHIONA.zip")
# unzip("data/GYMNOPHIONA.zip", exdir = "data/")

#se quiser outros dados, é só mudar o link e o nome do arquivo
# URL <- "http://bit.ly/1JTrFLL"#dadosIUCN Anura(~300Mb)

# download.file(URL,destfile="data/anura.zip")
# unzip("data/anura.zip",exdir = "data/")
rm(URL)#remove o link da área de trabalho


###Distribuição das espécies peixes borboletas
# Importanto o shapefile
borb <-readOGR (dsn = "Chaetodontidae_NewWorld.shp")

#visualizando o conteúdo dos dados
head(borb@data)

## usando cores aleatórias nos nossos mapas
colors <- rainbow(length(unique(borb@data$binomial)), alpha = 0.5)
position <- match(borb@data$binomial, unique(borb@data$binomial))
colors <- colors[position]

## Plot call
map("world",xlim=c(-150,40));box()
plot(borb, col = colors, lty = 0,add=T)


#Riqueza de espécies na América
borb_maps <- lets.presab(borb, resol = 10,
                         # xmn = -150,xmx = 40,#America range
                         # ymn = -38,ymx = 45,#America range
                         cover = 0.01)

plot(borb_maps,axes=F,main="Riqueza de Peixes Borboletas")


#extrair dados ambientais
# library(sdmpredictors)
# list_datasets()
# list_datasets(terrestrial = FALSE, marine = TRUE)
# 
# list_layers(c("Bio-ORACLE"))$layer_code
# 
# bathy <- load_layers(c("BO2_dissoxmean_ss", "BO_bathymean"))
# 
# plot(bathy$BO2_dissoxmean_ss, main = "Annual Mean Dissolved Oxigen")
# plot(bathy$BO_bathymean, main = "Mean Bathimetry")
# 
# 
# corta_ext <- extent(borb_maps$Richness_Raster)
# bathy_ext <- crop(bathy, corta_ext)
# plot(bathy_ext)
# 
# plot(bathy$BO_bathymean, main = "Mean Bathimetry")
# plot(borb_maps,axes=F,main="Riqueza de Peixes Borboletas")
# 
# projection(borb_maps$Richness_Raster) <- projection(bathy_ext$BO_bathymean)
# 
# borb_mar <- lets.addvar(borb_maps, bathy_ext, fun = mean)
# 
# 
# borb_mar <- as.data.frame(borb_mar)
# dim(borb_mar)
# names(borb_mar)
# borb_mar$riq <- rowSums(borb_mar[, -c(1:2, 32:33)])
# 
# plot(riq ~ BO_bathymean_mean, borb_mar)
# plot(riq ~ BO2_dissoxmean_ss_mean, borb_mar)





###3. Barreiras biogeograficas
library(vegan)

#criar uma tabela com dados de presenca/ausencia das especies
pa<-as.data.frame(borb_maps$Presence_and_Absence_Matrix[,-c(1:2)])
names(pa)#checar os nomes para ver se contem somente as especies

###Calculando dissimilaridade na composição de espécies
library(betapart)
beta.sim=beta.pair(pa,index.family="sorensen")$beta.sim

###Visualizando a composição de especies por meio de um NMDS
library(vegan);library(recluster)
points<-metaMDS(beta.sim,k=2,
                autotransform=F,trymax=5)$points

#Atribuindo cores de acordo com a composicao de especies (escala RedGreenBlue)
cores<-recluster.col(points)
RGB=data.frame(sites=rownames(cores),RGB=rgb(cores[,3:5],maxColorValue=255))
recluster.plot.col(cores,cex=5,cext=1)

library(maps)
#criando um novo objeto contendo o mapa da composicao
mapa_comp<-borb_maps$Richness_Raster
#Atribuindo ao mapa as cores RGB
mapa_comp[mapa_comp[]==0]<-NA
mapa_comp[mapa_comp[]!=0]<-RGB$RGB


map("world")
plot(mapa_comp,add=TRUE,axis=FALSE)
