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

# relationships
MF_LK_LABEL <- "Label"
MF_LK_TYPE <- "typeLien"
MF_LK_TYPE_FAM <- "LienFamille"
MF_LK_TYPE_PRO <- "LienProfessionel"

# personal info
MF_ND_ECCL <- "chargeEccl"
MF_ND_HEALTH <- "sante"
MF_ND_HOMETOWN <- "provVille"
MF_ND_HOMEDIOC <- "provDioc"
MF_ND_GENDER <- "genre"
MF_ND_ID <- "idIndividu"
MF_ND_JOB <- "profession"
MF_ND_JOB1 <- "profession"
MF_ND_JOB2 <- "profession2"
MF_ND_NAME_FULL <- "identite"
MF_ND_NAME_NORM <- "identiteNorm"
MF_ND_NAME_FIRST <- "prenom"
MF_ND_NAME_LAST <- "nom"
MF_ND_NAME_NICK <- "surnom"
MF_ND_RESIDENCE <- "residence"
MF_ND_SOURCE <- "Source"
MF_ND_TARGET <- "Target"
MF_ND_STATUS <- "statut"
MF_ND_TITLE <- "titre"
MF_ND_TITLE1 <- "titre"
MF_ND_TITLE2 <- "titre2"




#############################################################################################
# graph-related constants

# link attributes
LK_TYPE <- "type"
	LK_TYPE_ALL <- "Full"
	LK_TYPE_FAM <- "Familial"
	LK_TYPE_PRO <- "Professional"
LK_LABEL <- "label"

# map to convert link attribute names
MAP_MF2LK <- c()
MAP_MF2LK[MF_LK_TYPE_FAM] <- LK_TYPE_FAM
MAP_MF2LK[MF_LK_TYPE_PRO] <- LK_TYPE_PRO

# node attributes
ND_ECCL <- "ecclpos"
ND_HEALTH <- "health"
ND_HOMETOWN <- "hometown"
ND_HOMEDIOC <- "homedioc"
ND_GENDER <- "gender"
ND_ID <- "name"
ND_JOB <- "occupation"
ND_NAME_FULL <- "fullname"
ND_NAME_NORM <- "normname"
ND_NAME_FIRST <- "firstname"
ND_NAME_LAST <- "lastname"
ND_NAME_NICK <- "nickname"
ND_RESIDENCE <- "residence"
ND_STATUS <- "status"
ND_TITLE <- "title"

# map to convert node attribute names
MAP_MF2ND <- c()
MAP_MF2ND[MF_ND_ECCL] <- ND_ECCL
MAP_MF2ND[MF_ND_HEALTH] <- ND_HEALTH
MAP_MF2ND[MF_ND_HOMETOWN] <- ND_HOMETOWN
MAP_MF2ND[MF_ND_HOMEDIOC] <- ND_HOMEDIOC
MAP_MF2ND[MF_ND_GENDER] <- ND_GENDER
MAP_MF2ND[MF_ND_ID] <- ND_ID
MAP_MF2ND[MF_ND_JOB] <- ND_JOB
MAP_MF2ND[MF_ND_NAME_FULL] <- ND_NAME_FULL
MAP_MF2ND[MF_ND_NAME_NORM] <- ND_NAME_NORM
MAP_MF2ND[MF_ND_NAME_FIRST] <- ND_NAME_FIRST
MAP_MF2ND[MF_ND_NAME_LAST] <- ND_NAME_LAST
MAP_MF2ND[MF_ND_NAME_NICK] <- ND_NAME_NICK
MAP_MF2ND[MF_ND_RESIDENCE] <- ND_RESIDENCE
MAP_MF2ND[MF_ND_STATUS] <- ND_STATUS
MAP_MF2ND[MF_ND_TITLE] <- ND_TITLE

# long names of node attributes
LONG_ND <- c()
LONG_ND[ND_ECCL] <- "Ecclesiastical Office"
LONG_ND[ND_HEALTH] <- "Health"
LONG_ND[ND_HOMETOWN] <- "Home Town"
LONG_ND[ND_HOMEDIOC] <- "Home Diocese"
LONG_ND[ND_GENDER] <- "Gender"
LONG_ND[ND_ID] <- "Identifier"
LONG_ND[ND_JOB] <- "Occupation"
LONG_ND[ND_NAME_FULL] <- "Full name"
LONG_ND[ND_NAME_NORM] <- "Normalized Full Name"
LONG_ND[ND_NAME_FIRST] <- "First Name"
LONG_ND[ND_NAME_LAST] <- "Last Name"
LONG_ND[ND_NAME_NICK] <- "Nick Name"
LONG_ND[ND_RESIDENCE] <- "Residence"
LONG_ND[ND_STATUS] <- "Status"
LONG_ND[ND_TITLE] <- "Title"
