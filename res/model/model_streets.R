# Random model to create a city network based on relatively realistic streets.
# 
# Vincent Labatut
# 06/2022
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/model_streets.R")
###############################################################################
# load other scripts
source("res/common/_include.R")





###############################################################################
# parameters
n <- 250
out.folder <- "out/analysis/estate/model/positions"
del.rates <- seq(0,1,by=0.1)




###############################################################################

