#############################################################################################
# Defines functions or constants used by all other scripts.
# 
# 06/2020 Vincent Labatut
#
# source("res/common/constants.R")
#############################################################################################




#############################################################################################
# folders and files
FOLDER_IN <- "in"
	FOLDER_IN_POS <- file.path(FOLDER_IN,"positioning")
	FOLDER_IN_ANAL <- file.path(FOLDER_IN,"analysis")
		FILE_IN_ANAL_LINKS <- file.path(FOLDER_IN_ANAL,"LienEntreIndividu.csv")
		FILE_IN_ANAL_NODES <- file.path(FOLDER_IN_ANAL,"DetailsIndividu.csv")
FOLDER_OUT <- "out"
	FOLDER_OUT_POS <- file.path(FOLDER_OUT,"positioning")
	FOLDER_OUT_ANAL <- file.path(FOLDER_OUT,"analysis")
FOLDER_LOG <- "log"

FILE_GRAPH <- "graph.graphml"




#############################################################################################
# table-related constants
MF_LK_LABEL <- "Label"
MF_LK_TYPE <- "typeLien"
MF_LK_TYPE_FAM <- "LienFamille"
MF_LK_TYPE_PRO <- "LienProfessionel"
MF_ND_ID <- "idIndividu"
MF_ND_SOURCE <- "Source"
MF_ND_TARGET <- "Target"




#############################################################################################
# graph-related constants
LK_TYPE <- "type"
LK_TYPE_ALL <- "Full"
LK_TYPE_FAM <- "Familial"
LK_TYPE_PRO <- "Professional"
LK_LABEL <- "label"

MAP_MF2LK <- c()
MAP_MF2LK[MF_LK_TYPE_FAM] <- LK_TYPE_FAM
MAP_MF2LK[MF_LK_TYPE_PRO] <- LK_TYPE_PRO

ND_NAME_FULL <- "identite"
ND_NAME_FIRST <- "prenom"
ND_NAME_LAST <- "nom"
