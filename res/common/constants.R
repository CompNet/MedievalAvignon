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
		FILE_IN_ANAL_PERSON_NODES <- file.path(FOLDER_IN_ANAL,"Table_Individu.csv")
		FILE_IN_ANAL_ESTATE_NODES <- file.path(FOLDER_IN_ANAL,"Table_Bien.csv")
		FILE_IN_ANAL_SOCIAL_LINKS <- file.path(FOLDER_IN_ANAL,"Table_LienIndividu.csv")
		FILE_IN_ANAL_OWNERSHIP_LINKS <- file.path(FOLDER_IN_ANAL,"Table_BienIndividu.csv")
		FILE_IN_ANAL_ESTATE_FEE <- file.path(FOLDER_IN_ANAL,"Table_Redevance.csv")
		FOLDER_OUT <- "out"
	FOLDER_OUT_POS <- file.path(FOLDER_OUT,"positioning")
	FOLDER_OUT_ANAL <- file.path(FOLDER_OUT,"analysis")
FOLDER_LOG <- "log"

FILE_GRAPH <- "graph.graphml"




#############################################################################################
# table-related constants

# person table
	# unique id
	COL_PERS_ID <- "idIndividu"
	# names
	COL_PERS_NAME_FULL_LAT <- "identite"
	COL_PERS_NAME_FULL_NORM <- "identiteNorm"
	COL_PERS_NAME_FIRST <- "prenom"
	COL_PERS_NAME_LAST <- "nom"
	COL_PERS_NAME_NICK <- "surnom"
	COL_PERS_NAME_TYPE <- "nomDetail"
	# misc
	COL_PERS_GENDER <- "genre"
	COL_PERS_IDENTIFICATION <- "identification"
	COL_PERS_RESIDENCE <- "residence"
	# titles
	COL_PERS_TITLE_LAT1 <- "titre"
	COL_PERS_TITLE_FRE1 <- "titreT"
	COL_PERS_TITLE_NORM1 <- "titreTN"
	COL_PERS_TITLE_LAT2 <- "titre2"
	COL_PERS_TITLE_FRE2 <- "titre2T"
	COL_PERS_TITLE_NORM2 <- "titre2TN"
	# occupations
	COL_PERS_OCC_LAT1 <- "profession"
	COL_PERS_OCC_FRE1 <- "professionT"
	COL_PERS_OCC_NORM1 <- "professionTN"
	COL_PERS_OCC_LAT2 <- "profession2"
	COL_PERS_OCC_FRE2 <- "profession2T"
	COL_PERS_OCC_NORM2 <- "profession2TN"
	COL_PERS_OCC_CAT <- "professionCategorie"
	COL_PERS_OCC_THEME <- "professionTheme"
	# ecclesiastic office
	COL_PERS_ECCL_LAT <- "chargeEccl"
	COL_PERS_ECCL_FRE <- "chargeEcclT"
	COL_PERS_ECCL_NORM <- "chargeEcclTN"
	# health
	COL_PERS_HEALTH_LAT <- "sante"
	COL_PERS_HEALTH_FRE <- "santeT"
	# home city
	COL_PERS_CITY_LAT <- "provVille"
	COL_PERS_CITY_FRE <- "provVilleT"
	# home diocese
	COL_PERS_DIOC_LAT <- "provDioc"
	COL_PERS_DIOC_FRE <- "provDiocT"
	# status
	COL_PERS_STATUS_LAT <- "statut"
	COL_PERS_STATUS_FRE <- "statutT"
	COL_PERS_STATUS_NORM <- "statutTN"

# social links table
	COL_SOC_ID <- "idLienIndividu"
	COL_SOC_SRC <- "idIndividu1"
	COL_SOC_TGT <- "idIndividu2"
	COL_SOC_DESCR <- "descriptionLien"
	COL_SOC_TYPE <- "typeLien"

# estate table
	# ids
	COL_EST_ID <- "idBien"
	COL_EST_AREA_ID <- "idQuartier"
	COL_EST_STREET_ID <- "idRue"
	COL_EST_VILLAGE_ID <- "idBourg"
	COL_EST_FEE_ID <- "idRedevance"
	COL_EST_LORDSHIP_ID <- "idSeigneurie"
	COL_EST_DECLARATION_ID <- "idDeclaration"
	# misc
	COL_EST_TYPE_LAT <- "type"
	COL_EST_TYPE_FRE <- "typeT"
	COL_EST_QUALIF_LAT <- "qualif"
	COL_EST_QUALIF_NORM <- "qualifTN"
	COL_EST_DETAIL <- "detail"
	COL_EST_MEASURE <- "mesure"
	COL_EST_MATERIALS <- "materiaux"
	COL_EST_GEOMETRY <- "geom"
	# components
	COL_EST_COMP_LAB1 <- "composant1"
	COL_EST_COMP_NBR1 <- "nbrComposant1"
	COL_EST_COMP_LAB2 <- "composant2"
	COL_EST_COMP_NBR2 <- "nbrComposant2"
	COL_EST_COMP_LAB3 <- "composant3"
	COL_EST_COMP_LAB4 <- "composant4"
	COL_EST_COMP_LAB5 <- "composant5"
	COL_EST_COMP_LAB6 <- "composant6"
	# NOTE: declared estate = COL_EST_DECLARATION_ID | COL_EST_FEE_ID

# ownership table
	COL_OWN_ID <- "idBienIndividu"
	COL_OWN_PERS_ID <- "idIndividu"
	COL_OWN_EST_ID <- "idBien"
	COL_OWN_DATE1 <- "date1reconnaissance"
	COL_OWN_DATE2 <- "date2reconnaissance"
	COL_OWN_INHER_ORDER <- "ordreSuccession"
	COL_OWN_MEANS <- "moyenAcqui"
	COL_OWN_SAFE <- "sureteDate"

# estate fee table
	COL_FEE_ID <- "idRedevance"
	COL_FEE_TYPE <- "type"
	COL_FEE_CURRENCY <- "espece"
	COL_FEE_DETAIL <- "detailRedev"
	COL_FEE_AMOUNT_RAW1 <- "montant"
	COL_FEE_AMOUNT_NORM1 <- "montantN"
	COL_FEE_AMOUNT_RAW2 <- "montant2"
	COL_FEE_AMOUNT_INCR <- "echelonMontant"

# values
	# social links
	VAL_SOC_TYPE_FAMILY <- "LIFAM"
	VAL_SOC_TYPE_WORK <- "LITRA"

	
	
	
#############################################################################################
# graph-related values

# types of links 
LK_TYPE <- "type"
	LK_TYPE_ALL <- "Full"
	LK_TYPE_FAM <- "Familial"
	LK_TYPE_PRO <- "Professional"
	LK_TYPE_LST <- c(LK_TYPE_ALL, LK_TYPE_FAM, LK_TYPE_PRO)
LK_DESCR <- "description"
	
# map to convert link attribute names
MAP_TABLE2GRAPH <- c()
MAP_TABLE2GRAPH[VAL_SOC_TYPE_FAMILY] <- LK_TYPE_FAM
MAP_TABLE2GRAPH[VAL_SOC_TYPE_WORK] <- LK_TYPE_PRO

# misc
VAL_OTHER <- "Other"
VAL_TRUE <- "True"
VAL_FALSE <- "False"
VAL_UNKNOWN <- "Unknown"




#############################################################################################
# long names
LONG_NAME <- c()
LONG_NAME[COL_PERS_ID] <- "Person ID"
LONG_NAME[COL_PERS_NAME_FULL_LAT] <- "Full name (translated)"
LONG_NAME[COL_PERS_NAME_FULL_NORM] <- "Full name (normalized)"
LONG_NAME[COL_PERS_NAME_FIRST] <- "Firstname"
LONG_NAME[COL_PERS_NAME_LAST] <- "Lastname"
LONG_NAME[COL_PERS_NAME_NICK] <- "Nickname"
LONG_NAME[COL_PERS_NAME_TYPE] <- "Name type"
LONG_NAME[COL_PERS_GENDER] <- "Gender"
LONG_NAME[COL_PERS_IDENTIFICATION] <- "Identification"
LONG_NAME[COL_PERS_RESIDENCE] <- "Residence"
LONG_NAME[COL_PERS_TITLE_LAT1] <- "Title 1 (original)"
LONG_NAME[COL_PERS_TITLE_FRE1] <- "Title 1 (translated)"
LONG_NAME[COL_PERS_TITLE_NORM1] <- "Title 1 (normalized)"
LONG_NAME[COL_PERS_TITLE_LAT2] <- "Title 2 (original)"
LONG_NAME[COL_PERS_TITLE_FRE2] <- "Title 2 (translated)"
LONG_NAME[COL_PERS_TITLE_NORM2] <- "Title 2 (normalized)"
LONG_NAME[COL_PERS_OCC_LAT1] <- "Occupation 1 (original)"
LONG_NAME[COL_PERS_OCC_FRE1] <- "Occupation 1 (translated)"
LONG_NAME[COL_PERS_OCC_NORM1] <- "Occupation 1 (normalized)"
LONG_NAME[COL_PERS_OCC_LAT2] <- "Occupation 2 (original)"
LONG_NAME[COL_PERS_OCC_FRE2] <- "Occupation 2 (translated)"
LONG_NAME[COL_PERS_OCC_NORM2] <- "Occupation 2 (normalized)"
LONG_NAME[COL_PERS_OCC_CAT] <- "Occupation category"
LONG_NAME[COL_PERS_OCC_THEME] <- "Occupation theme"
LONG_NAME[COL_PERS_ECCL_LAT] <- "Ecclesiastical office (original)"
LONG_NAME[COL_PERS_ECCL_FRE] <- "Ecclesiastical office (translated)"
LONG_NAME[COL_PERS_ECCL_NORM] <- "Ecclesiastical office (normalized)"
LONG_NAME[COL_PERS_HEALTH_LAT] <- "Health (original)"
LONG_NAME[COL_PERS_HEALTH_FRE] <- "Health (translated)"
LONG_NAME[COL_PERS_CITY_LAT] <- "Home city (original)"
LONG_NAME[COL_PERS_CITY_FRE] <- "Home city (translated)"
LONG_NAME[COL_PERS_DIOC_LAT] <- "Diocese (original)"
LONG_NAME[COL_PERS_DIOC_FRE] <- "Diocese (translated)"
LONG_NAME[COL_PERS_STATUS_LAT] <- "Status (original)"
LONG_NAME[COL_PERS_STATUS_FRE] <- "Status (translated)"
LONG_NAME[COL_PERS_STATUS_NORM] <- "Statut (normalized)"
LONG_NAME[COL_SOC_ID] <- "Social link ID"
LONG_NAME[COL_SOC_SRC] <- "Source person ID"
LONG_NAME[COL_SOC_TGT] <- "Target person ID"
LONG_NAME[COL_SOC_DESCR] <- "Link description"
LONG_NAME[COL_SOC_TYPE] <- "Link type"
LONG_NAME[COL_EST_ID] <- "Estate ID"
LONG_NAME[COL_EST_AREA_ID] <- "Area ID"
LONG_NAME[COL_EST_STREET_ID] <- "Street ID"
LONG_NAME[COL_EST_VILLAGE_ID] <- "Village ID"
LONG_NAME[COL_EST_FEE_ID] <- "Fee ID"
LONG_NAME[COL_EST_LORDSHIP_ID] <- "Lordship ID"
LONG_NAME[COL_EST_DECLARATION_ID] <- "Declaration ID"
LONG_NAME[COL_EST_TYPE_LAT] <- "Estate type (original)"
LONG_NAME[COL_EST_TYPE_FRE] <- "Estate type (translated)"
LONG_NAME[COL_EST_QUALIF_LAT] <- "Estate qualification (original)"
LONG_NAME[COL_EST_QUALIF_NORM] <- "Estate qualification (normalized)"
LONG_NAME[COL_EST_DETAIL] <- "Estate detail"
LONG_NAME[COL_EST_MEASURE] <- "Estate mesure"
LONG_NAME[COL_EST_MATERIALS] <- "Estate materials"
LONG_NAME[COL_EST_GEOMETRY] <- "Estate geom"
LONG_NAME[COL_EST_COMP_LAB1] <- "Estate component 1"
LONG_NAME[COL_EST_COMP_NBR1] <- "Estate component number 1"
LONG_NAME[COL_EST_COMP_LAB2] <- "Estate component 2"
LONG_NAME[COL_EST_COMP_NBR2] <- "Estate component number 1"
LONG_NAME[COL_EST_COMP_LAB3] <- "Estate component 3"
LONG_NAME[COL_EST_COMP_LAB4] <- "Estate component 4"
LONG_NAME[COL_EST_COMP_LAB5] <- "Estate component 5"
LONG_NAME[COL_EST_COMP_LAB6] <- "Estate component 6"
LONG_NAME[COL_OWN_ID] <- "Ownership ID"
LONG_NAME[COL_OWN_PERS_ID] <- "Owner ID"
LONG_NAME[COL_OWN_EST_ID] <- "Estate ID"
LONG_NAME[COL_OWN_DATE1] <- "Reconnaissance date 1"
LONG_NAME[COL_OWN_DATE2] <- "Reconnaissance date 2"
LONG_NAME[COL_OWN_INHER_ORDER] <- "Inheritance order"
LONG_NAME[COL_OWN_MEANS] <- "Acquisition means"
LONG_NAME[COL_OWN_SAFE] <- "Safety date"
LONG_NAME[COL_FEE_ID] <- "Fee ID"
LONG_NAME[COL_FEE_TYPE] <- "Fee type"
LONG_NAME[COL_FEE_CURRENCY] <- "Fee currency"
LONG_NAME[COL_FEE_DETAIL] <- "Fee detail"
LONG_NAME[COL_FEE_AMOUNT_RAW1] <- "Fee amount 1 (raw)"
LONG_NAME[COL_FEE_AMOUNT_NORM1] <- "Fee amount 1 (normalized)"
LONG_NAME[COL_FEE_AMOUNT_RAW2] <- "Fee amount 2 (raw)"
LONG_NAME[COL_FEE_AMOUNT_INCR] <- "Fee increment"
