# ------------------------------------------- #
# ----- Ms. Cleanig stations of skarks ------ #
#  ------------Quimbayo et al --------------- #
# ------------------------------------------- #
rm(list = ls())
setwd ("/Users/quimbayojp/Dropbox/Manuscripts/1_Manuscripts_Priority/Ms_Cleaning_stations_Sharks/Database")

# Calling data 
interactions <- read.csv ("Interactions_2018_09_07.csv", header = T, sep = ";")
interactions$win_or_loose <- 'win'
inspections  <- read.csv ("Inspections_2018_09_07.csv", header = T, sep = ";")
inspections$win_or_loose <- 'loose'

# Organization of data in a single dataframe
if(!require(reshape)){install.packages('reshape'); library(reshape)}

interactions <- melt (interactions, id=c("Number_cleaners", "Clients", 'win_or_loose'))
colnames (interactions)[4] <- "Video"
colnames (interactions)[5] <- "Num_Int"

interactions$ID_Video <- paste (interactions$Number_cleaners, interactions$Clients, 
                                interactions$Video, sep="_")

inspections <- melt (inspections, id=c("Number_cleaners", "Clients", "win_or_loose"))
colnames (inspections)[4] <- "Video"
colnames (inspections)[5] <- "Time_Insp"
inspections$ID_Video <- paste (inspections$Number_cleaners, inspections$Clients, 
                               inspections$Video, sep="_")

library(dplyr)
matrix_stations <- merge (inspections, interactions[, c("ID_Video","Num_Int")],
                          "ID_Video")
matrix_stations$Cleaner <- "Labroides_dimidiatus"
matrix_stations$Family_cleaner <- "Labridae"

# Preparing dataframe for bipartite networks

matrix_int <- cast (Clients ~ Number_cleaners, value="Num_Int",
                    data=matrix_stations, fun.aggregate = sum)
rownames (matrix_int) <- matrix_int$Clients
matrix_int <- matrix_int[,-1]

matrix_insp <- cast (Clients ~ Number_cleaners, value="Time_Insp",
                    data=matrix_stations, fun.aggregate = sum)
rownames (matrix_insp) <- matrix_insp$Clients
matrix_insp <- matrix_insp[,-1]

# Bipartite networks
if(!require(bipartite)){install.packages('bipartite'); library(bipartite)}
Inter_Data <- sortweb(matrix_int, sort.order="dec")
visweb(Inter_Data)

plotweb(sortweb(Inter_Data, sort.order="dec"), 
        method="normal", 
        arrow="up", 
        labsize=1,
        col.interaction="white", 
        text.rot=90, 
        y.width.high=0.07, 
        y.width.low=0.07, 
        bor.col.high = "black", 
        bor.col.low = "black")

Inspe_Data <- sortweb(matrix_insp, sort.order="dec")
visweb(Inspe_Data)

plotweb(sortweb(Inspe_Data, sort.order="dec"), 
        method="normal", 
        arrow="up", 
        labsize=1,
        col.interaction="white", 
        text.rot=90, 
        y.width.high=0.07, 
        y.width.low=0.07, 
        bor.col.high = "black", 
        bor.col.low = "black")

# ESTIMATION OF NESTEDNESS AND NULL MODELS ------------------------

# Nestedness (NODF) Number of interactions
Inter_nodf <- nestednodf (Inter_Data, order = TRUE, weighted = FALSE, wbinary = TRUE)
Inter_nodf # interpret: what does this mean?

Inter_rand_none = permatfull(Inter_Data,  times=1000, fixedmar="none", mtype="prab")

# Calculating NODF for null matrices
nrandbin <- list()
for(j in 1:1000){ 
  nrandbin[[j]] <- nestednodf(Inter_rand_none$perm[[j]], order = T, weighted = F, wbinary = wbinary)
}

# Organizing outputs
nodfrandbin <- matrix(NA, 1000, 3); colnames(nodfrandbin) <- c("Ncol", "Nrow", "NODF")
for(j in 1:1000){ nodfrandbin[j,] <- unlist(nrandbin[[j]][3]) }

# Summary Results
nodfRes_none <- matrix(NA, 3, 3); colnames(nodfRes_none) <- c("Obs", "2.5%", "97.5%"); rownames(nodfRes_none) <- c("NODF_col", "NODF_row", "NODF_tot")
nodfRes_none[,1] <- unlist(Inter_nodf[3])
nodfRes_none[1,2:3] <-quantile(nodfrandbin[,1], probs=c(0.025,0.975), type=2)
nodfRes_none[2,2:3] <- quantile(nodfrandbin[,2], probs=c(0.025,0.975), type=2)
nodfRes_none[3,2:3] <- quantile(nodfrandbin[,3], probs=c(0.025,0.975), type=2)
NODF_Interactions <- nodfRes_none
NODF_Interactions

# Nestedness (NODF) Time of inspection
Inspe_nodf <- nestednodf (Inspe_Data, order = TRUE, weighted = FALSE)
Inspe_nodf # interpret: what does this mean?

Inspe_rand_none = permatfull(Inspe_Data,  times=1000, fixedmar="none", mtype="prab")

# Calculating NODF for null matrices
nrandbin <- list()
for(j in 1:1000){ 
  nrandbin[[j]] <- nestednodf(Inspe_rand_none$perm[[j]], order = T, weighted = F, wbinary = wbinary)
}

# Organizing outputs
nodfrandbin <- matrix(NA, 1000, 3); colnames(nodfrandbin) <- c("Ncol", "Nrow", "NODF")
for(j in 1:1000){ nodfrandbin[j,] <- unlist(nrandbin[[j]][3]) }

# Summary Results
nodfRes_none <- matrix(NA, 3, 3); colnames(nodfRes_none) <- c("Obs", "2.5%", "97.5%"); rownames(nodfRes_none) <- c("NODF_col", "NODF_row", "NODF_tot")
nodfRes_none[,1] <- unlist(Inspe_nodf[3])
nodfRes_none[1,2:3] <-quantile(nodfrandbin[,1], probs=c(0.025,0.975), type=2)
nodfRes_none[2,2:3] <- quantile(nodfrandbin[,2], probs=c(0.025,0.975), type=2)
nodfRes_none[3,2:3] <- quantile(nodfrandbin[,3], probs=c(0.025,0.975), type=2)
NODF_Inspection <- nodfRes_none
NODF_Inspection




# WEIGTHED Nestedness (WNODF) Number of interactions
Inter_nodf <- nestednodf (Inter_Data, order = TRUE, weighted = T, wbinary = F)
Inter_nodf # interpret: what does this mean?

Inter_rand_none = permatfull(Inter_Data,  times=1000, fixedmar="none", mtype="count")

# Calculating WNODF for null matrices
nrandbin <- list()
for(j in 1:1000){ 
  nrandbin[[j]] <- nestednodf(Inter_rand_none$perm[[j]], order = T, weighted = T, wbinary = F)
}

# Organizing outputs
nodfrandbin <- matrix(NA, 1000, 3); colnames(nodfrandbin) <- c("Ncol", "Nrow", "NODF")
for(j in 1:1000){ nodfrandbin[j,] <- unlist(nrandbin[[j]][3]) }

# Summary Results
nodfRes_none <- matrix(NA, 3, 3); colnames(nodfRes_none) <- c("Obs", "2.5%", "97.5%"); rownames(nodfRes_none) <- c("WNODF_col", "WNODF_row", "WNODF_tot")
nodfRes_none[,1] <- unlist(Inter_nodf[3])
nodfRes_none[1,2:3] <-quantile(nodfrandbin[,1], probs=c(0.025,0.975), type=2)
nodfRes_none[2,2:3] <- quantile(nodfrandbin[,2], probs=c(0.025,0.975), type=2)
nodfRes_none[3,2:3] <- quantile(nodfrandbin[,3], probs=c(0.025,0.975), type=2)
WNODF_Interactions <- nodfRes_none
WNODF_Interactions



# Nestedness (WNODF) Time of inspection
Inspe_nodf <- nestednodf (Inspe_Data, order = TRUE, weighted = T)
Inspe_nodf # interpret: what does this mean?

Inspe_rand_none = permatfull(Inspe_Data,  times=1000, fixedmar="none", mtype="count")

# Calculating WNODF for null matrices
nrandbin <- list()
for(j in 1:1000){ 
  nrandbin[[j]] <- nestednodf(Inspe_rand_none$perm[[j]], order = T, weighted = T)
}

# Organizing outputs
nodfrandbin <- matrix(NA, 1000, 3); colnames(nodfrandbin) <- c("Ncol", "Nrow", "WNODF")
for(j in 1:1000){ nodfrandbin[j,] <- unlist(nrandbin[[j]][3]) }

# Summary Results
nodfRes_none <- matrix(NA, 3, 3); colnames(nodfRes_none) <- c("Obs", "2.5%", "97.5%"); rownames(nodfRes_none) <- c("WNODF_col", "WNODF_row", "WNODF_tot")
nodfRes_none[,1] <- unlist(Inspe_nodf[3])
nodfRes_none[1,2:3] <-quantile(nodfrandbin[,1], probs=c(0.025,0.975), type=2)
nodfRes_none[2,2:3] <- quantile(nodfrandbin[,2], probs=c(0.025,0.975), type=2)
nodfRes_none[3,2:3] <- quantile(nodfrandbin[,3], probs=c(0.025,0.975), type=2)
WNODF_Inspection <- nodfRes_none
WNODF_Inspection






# ESTIMATION OF MODULARITY AND NULL MODELS -----------------
# calculate modularity to interactions
modall_inte = computeModules(data.matrix(matrix_int), method="Beckett", deep = FALSE, deleteOriginalFiles = TRUE, 
                        steps = 1000000, tolerance = 1e-10, experimental = FALSE, forceLPA=FALSE)

# That restrictive null model 
inter_rand_inter = permatfull(data.matrix(matrix_int),  times=10000, fixedmar="none", mtype="prab")

# Calculating Q-metric for null matrices (VERY VERY SLOW)
randmodall = numeric()
ptm <- proc.time()
for (j in 1:1000){
  aux = computeModules(inter_rand_inter$perm[[j]])
  randmodall[j] = aux@likelihood
  cat(paste("Iteration ", j, " has finished. There are ", 10-j, " left. Time spent so far:", round(((proc.time() - ptm)[3])/60, digits=2), "min", '\n'))
  
}

# results: observed modularity (Q) and  95%CI for Q-metric
# Summary modularity results
modresult = matrix(NA, 1, 3)
colnames(modresult) = c("Q_Newman", "null_2.5%", "null_97.5%")
modresult[1,1] = modall_inte@likelihood
modresult[1,2:3] = as.numeric(quantile(randmodall, probs=c(0.025, 0.975), type=2))
modul_interactions <- modresult
modul_interactions

# Visualizing modules:
par(mfrow=c(1,1), mar=c(1,1,1,1))
plotModuleWeb(modall_inte, plotModules = TRUE, 
              rank = T, weighted = F, displayAlabels = TRUE, 
              displayBlabels = TRUE, labsize = .7, xlabel = "", ylabel = "", 
              square.border = "white", fromDepth = 0, upToDepth = -1)



# calculate modularity to Inspections
modall_insp = computeModules(data.matrix(matrix_insp), method="Beckett", deep = FALSE, deleteOriginalFiles = TRUE, 
                        steps = 1000000, tolerance = 1e-10, experimental = FALSE, forceLPA=FALSE)

# That restrictive null model (VERY SLOW, let's do a few matrices for practicing)
inter_rand_insp = permatfull(data.matrix(matrix_insp),  times=10000, fixedmar="none", mtype="count")

# Calculating Q-metric for null matrices (VERY VERY SLOW)
randmodall = numeric()
ptm <- proc.time()
for (j in 1:1000){
  aux = computeModules(inter_rand_insp$perm[[j]])
  randmodall[j] = aux@likelihood
  cat(paste("Iteration ", j, " has finished. There are ", 10-j, " left. Time spent so far:", round(((proc.time() - ptm)[3])/60, digits=2), "min", '\n'))
  
}

# results: observed modularity (Q) and  95%CI for Q-metric
# Summary modularity results
modresult = matrix(NA, 1, 3)
colnames(modresult) = c("Q_Newman", "null_2.5%", "null_97.5%")
modresult[1,1] = modall_insp@likelihood
modresult[1,2:3] = as.numeric(quantile(randmodall, probs=c(0.025, 0.975), type=2))
modul_inspections <- modresult

# Visualizing modules:
par(mfrow=c(1,1), mar=c(1,1,1,1))
plotModuleWeb(modall_insp, plotModules = TRUE, 
              rank = T, weighted = F, displayAlabels = TRUE, 
              displayBlabels = TRUE, labsize = .7, xlabel = "", ylabel = "", 
              square.border = "white", fromDepth = 0, upToDepth = -1)




