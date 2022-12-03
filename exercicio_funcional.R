

library(FD)


trait <- read.table("/Users/thiagomendes/Desktop/trait.txt", header=T)
occur <- read.csv("/Users/thiagomendes/Desktop/chaet_dist.csv", sep=";")

chaet_trait <- droplevels(subset(trait, Family == "Chaetodontidae")) #subset apenas com os dados de Chaetodontidae
chaet_trait <- na.omit(chaet_trait) #eliminando os dados NA

chaet_trait$Atl <- occur$Atlantic[match(chaet_trait$Genus_and_species, occur$Genus_and_species)] #"pescando" as ocorrências das spp em cada uma das quatro bacias oceânicas
chaet_trait$Ind <- occur$Indian[match(chaet_trait$Genus_and_species, occur$Genus_and_species)]
chaet_trait$TEP <- occur$TEP[match(chaet_trait$Genus_and_species, occur$Genus_and_species)]
chaet_trait$Pac <- occur$Pacific[match(chaet_trait$Genus_and_species, occur$Genus_and_species)]


dist <- gowdis(chaet_trait[,14:19]) #cálculo da dissimilaridade de Gower baseado nos atributos das spp
pcoa <- cmdscale(dist, k = 4) #cálculo da análise de componentes principais


chaet_info <- cbind(pcoa, chaet_trait) #dataframe juntando todas as informações de atributos, ocorrências e coordenadas na pcoa para todas as spp


par(mfrow = c(2,2)) #divide a área de plot em quatro

#plots com:
#[1] espaço funcional global
#[2] o cálculo do número de spp por região
#[3] o polígono convexo para todas as entidades funcionais do mundo
#[4] o polígono convexo apenas para as spp que ocorrem em uma das regiões
plot(pcoa, xlab = "PC1", ylab = "PC2", main = "Atlantic")
legend("topright", legend = sum(chaet_info$Atl), bty = "n")
ordihull(pcoa, groups = chaet_info$Size_class <= 1, draw = "polygon", col = "grey", alpha=50)
ordihull(pcoa, groups= chaet_info$Atl==1, col= "orange", draw="polygon", show.group=TRUE, alpha=50)

plot(pcoa, xlab = "PC1", ylab = "PC2", main = "TEP")
legend("topright", legend = sum(chaet_info$TEP), bty = "n")
ordihull(pcoa, groups = chaet_info$Size_class <= 1, draw = "polygon", col = "grey", alpha=50)
ordihull(pcoa, groups= chaet_info$TEP==1, col= "green", draw="polygon", show.group=TRUE, alpha=50)

plot(pcoa, xlab = "PC1", ylab = "PC2", main = "Indian")
legend("topright", legend = sum(chaet_info$Ind), bty = "n")
ordihull(pcoa, groups = chaet_info$Size_class <= 1, draw = "polygon", col = "grey", alpha=50)
ordihull(pcoa, groups= chaet_info$Ind==1, col= "blue", draw="polygon", show.group=TRUE, alpha=50)

plot(pcoa, xlab = "PC1", ylab = "PC2", main = "Pacific")
legend("topright", legend = sum(chaet_info$Pac), bty = "n")
ordihull(pcoa, groups = chaet_info$Size_class <= 1, draw = "polygon", col = "grey", alpha=50)
ordihull(pcoa, groups= chaet_info$Pac==1, col= "red", draw="polygon", show.group=TRUE, alpha=50)


#cálculo dos íncices funcionais
#para o cálculo são necessárias dois data frames: 
#[1] traits - spp (linhas) x traits (colunas) - aqui os traits devem ser numéricos ou fatores
#[2] ocorrencia - regiões (linhas) x spp (colunas)

traits <- chaet_info[,18:23]
rownames(traits) <- chaet_info$Genus_and_species
traits$Diet_2012 <- as.factor(traits$Diet_2012) #transformar caracteres em fatores
traits$Home_range <- as.factor(traits$Home_range)
traits$Diet_2012 <- as.factor(traits$Diet_2012)
traits$Activity <- as.factor(traits$Activity)
traits$Schooling <- as.factor(traits$Schooling)
traits$Level <- as.factor(traits$Level)

ocorrencias <- chaet_info[,24:27]
rownames(ocorrencias) <- chaet_info$Genus_and_species

indices <- dbFD(traits, t(ocorrencias))
indices





library(FD)
library(tidyverse)

gowdis(dummy$trait) %>% 
  pcoa() %>% 
  biplot()

indices <- dbFD(dummy$trait, dummy$abun)
indices
