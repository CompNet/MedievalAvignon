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
		FILE_IN_ANAL_PERSON_NODES <- file.path(FOLDER_IN_ANAL,"person_info.csv")
		FILE_IN_ANAL_ESTATE_NODES <- file.path(FOLDER_IN_ANAL,"estate_info.csv")
		FILE_IN_ANAL_SOCIAL_LINKS <- file.path(FOLDER_IN_ANAL,"social_links.csv")
		FILE_IN_ANAL_OWNERSHIP_LINKS <- file.path(FOLDER_IN_ANAL,"ownership_links.csv")
	FOLDER_OUT <- "out"
	FOLDER_OUT_POS <- file.path(FOLDER_OUT,"positioning")
	FOLDER_OUT_ANAL <- file.path(FOLDER_OUT,"analysis")
FOLDER_LOG <- "log"

FILE_GRAPH <- "graph.graphml"




#############################################################################################
# table-related constants

# relationships
MF_LK_SOURCE <- "Source"
MF_LK_TARGET <- "Target"
MF_LK_LABEL <- "Label"
MF_LK_TYPE <- "typeLien"
MF_LK_TYPE_FAM <- "LienFamille"
MF_LK_TYPE_PRO <- "LienProfessionel"

# personal info
MF_ND_ECCL <- "chargeEcclTN"
MF_ND_HEALTH <- "santeT"
MF_ND_HOMETOWN <- "provVilleT"
MF_ND_HOMEDIOC <- "provDiocT"
MF_ND_GENDER <- "genre"
MF_ND_ID <- "idindividu"
MF_ND_JOB1 <- "professionTN"
MF_ND_JOB2 <- "profession2TN"
MF_ND_NAME_FULL <- "identite"
	MF_ND_NAME_NORM <- "identiteNorm" # n'apparait plus dans la nouvelle version
MF_ND_NAME_FIRST <- "prenom"
MF_ND_NAME_LAST <- "nom"
MF_ND_NAME_NICK <- "surnom"
	MF_ND_RESIDENCE <- "residence" # n'apparait plus dans la nouvelle version
MF_ND_STATUS <- "statutTN"
MF_ND_TITLE1 <- "titreTN"
MF_ND_TITLE2 <- "titre2TN"




#############################################################################################
# graph-related constants

# link attributes
LK_TYPE <- "type"
	LK_TYPE_ALL <- "Full"
	LK_TYPE_FAM <- "Familial"
	LK_TYPE_PRO <- "Professional"
LK_SUBTYPE <- "subtype"

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
ND_JOB1 <- "occupation1"
ND_JOB2 <- "occupation2"
ND_NAME_FULL <- "fullname"
	ND_NAME_NORM <- "normname" # n'apparait plus dans la nouvelle version
ND_NAME_FIRST <- "firstname"
ND_NAME_LAST <- "lastname"
ND_NAME_NICK <- "nickname"
	ND_RESIDENCE <- "residence" # n'apparait plus dans la nouvelle version
ND_STATUS <- "status"
ND_TITLE <- "title"
ND_TITLE1 <- "title1"
ND_TITLE2 <- "title2"

# map to convert node attribute names
MAP_MF2ND <- c()
MAP_MF2ND[MF_ND_ECCL] <- ND_ECCL
MAP_MF2ND[MF_ND_HEALTH] <- ND_HEALTH
MAP_MF2ND[MF_ND_HOMETOWN] <- ND_HOMETOWN
MAP_MF2ND[MF_ND_HOMEDIOC] <- ND_HOMEDIOC
MAP_MF2ND[MF_ND_GENDER] <- ND_GENDER
MAP_MF2ND[MF_ND_ID] <- ND_ID
MAP_MF2ND[MF_ND_JOB1] <- ND_JOB1
MAP_MF2ND[MF_ND_JOB2] <- ND_JOB2
MAP_MF2ND[MF_ND_NAME_FULL] <- ND_NAME_FULL
MAP_MF2ND[MF_ND_NAME_NORM] <- ND_NAME_NORM
MAP_MF2ND[MF_ND_NAME_FIRST] <- ND_NAME_FIRST
MAP_MF2ND[MF_ND_NAME_LAST] <- ND_NAME_LAST
MAP_MF2ND[MF_ND_NAME_NICK] <- ND_NAME_NICK
MAP_MF2ND[MF_ND_RESIDENCE] <- ND_RESIDENCE
MAP_MF2ND[MF_ND_STATUS] <- ND_STATUS
MAP_MF2ND[MF_ND_TITLE1] <- ND_TITLE1
MAP_MF2ND[MF_ND_TITLE2] <- ND_TITLE2

# long names of node attributes
LONG_NAME <- c()
LONG_NAME[ND_ECCL] <- "Ecclesiastical Office"
LONG_NAME[ND_HEALTH] <- "Health"
LONG_NAME[ND_HOMETOWN] <- "Home Town"
LONG_NAME[ND_HOMEDIOC] <- "Home Diocese"
LONG_NAME[ND_GENDER] <- "Gender"
LONG_NAME[ND_ID] <- "Identifier"
LONG_NAME[ND_JOB] <- "Occupation"
LONG_NAME[ND_NAME_FULL] <- "Full name"
LONG_NAME[ND_NAME_NORM] <- "Normalized Full Name"
LONG_NAME[ND_NAME_FIRST] <- "First Name"
LONG_NAME[ND_NAME_LAST] <- "Last Name"
LONG_NAME[ND_NAME_NICK] <- "Nick Name"
LONG_NAME[ND_RESIDENCE] <- "Residence"
LONG_NAME[ND_STATUS] <- "Status"
LONG_NAME[ND_TITLE] <- "Title"

# values
VAL_OTHER <- "Other"
VAL_TRUE <- "True"
VAL_FALSE <- "False"
VAL_UNKNOWN <- "Unknown"
