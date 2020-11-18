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
		FILE_IN_ANAL_ESTATE_NODES <- file.path(FOLDER_IN_ANAL,"Table_Bien.csv")
		FILE_IN_ANAL_OWNERSHIP_LINKS <- file.path(FOLDER_IN_ANAL,"Table_BienIndividu.csv")
		FILE_IN_ANAL_VILG_NODES <- file.path(FOLDER_IN_ANAL,"Table_Bourg.csv")
		FILE_IN_ANAL_EDIFICE_NODES <- file.path(FOLDER_IN_ANAL,"Table_Edifice.csv")
		FILE_IN_ANAL_PERSON_NODES <- file.path(FOLDER_IN_ANAL,"Table_Individu.csv")
		FILE_IN_ANAL_FIX_NODES <- file.path(FOLDER_IN_ANAL,"Table_Invariant_Renvoi_Table.csv")
		FILE_IN_ANAL_SOCIAL_LINKS <- file.path(FOLDER_IN_ANAL,"Table_LienIndividu.csv")
		FILE_IN_ANAL_CARD_NODES <- file.path(FOLDER_IN_ANAL,"Table_LivreeCardinalice.csv")
		FILE_IN_ANAL_CONFR_LINKS <- file.path(FOLDER_IN_ANAL,"Table_Localisation_Confront.csv")
		FILE_IN_ANAL_GATE_NODES <- file.path(FOLDER_IN_ANAL,"Table_PortePortail.csv")
		FILE_IN_ANAL_AREA_NODES <- file.path(FOLDER_IN_ANAL,"Table_QuartierParoisse.csv")
		FILE_IN_ANAL_ESTATE_FEE <- file.path(FOLDER_IN_ANAL,"Table_Redevance.csv")
		FILE_IN_ANAL_WALL_NODES <- file.path(FOLDER_IN_ANAL,"Table_Rempart.csv")
		FILE_IN_ANAL_LDMRK_NODES <- file.path(FOLDER_IN_ANAL,"Table_RepereGeo.csv")
		FILE_IN_ANAL_STREET_NODES <- file.path(FOLDER_IN_ANAL,"Table_Rue.csv")
		FILE_IN_ANAL_SRC_NODES <- file.path(FOLDER_IN_ANAL,"Table_Source.csv")
		#
		FILE_IN_ANAL_STRT_SHORT <- file.path(FOLDER_IN_ANAL,"Rue_Courte.csv")
	FOLDER_OUT <- "out"
		FOLDER_OUT_POS <- file.path(FOLDER_OUT,"positioning")
		FOLDER_OUT_ANAL <- file.path(FOLDER_OUT,"analysis")
			FOLDER_OUT_ANAL_SOC <- file.path(FOLDER_OUT_ANAL,"social")
			FOLDER_OUT_ANAL_EST <- file.path(FOLDER_OUT_ANAL,"estate")
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

# location
	COL_LOC_X <- "xcoord"
	COL_LOC_Y <- "ycoord"
	COL_LOC_ID <- "idExterne"
	
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

# fixes table
	COL_FIX_ID <- "idInvariant"
	COL_FIX_TYPE <- "type"
	COL_FIX_NAME <- "nom"
	COL_FIX_GEOMETRY <- "the_geom"
	
# edifice table
	COL_EDIF_ID <- "id"
	COL_EDIF_GEOMETRY <- "geom"
	COL_EDIF_TYPE <- "type"
	COL_EDIF_NAME <- "nom"
	COL_EDIF_STATUS <- "statut"
	COL_EDIF_DATE_FRST_OCC <- "datePremiereMention"
	COL_EDIF_DATE_BUILD_START <- "dateDebutConst"
	COL_EDIF_DATE_BUILD_END <- "dateFinConst"
	COL_EDIF_SRC <- "source"
	COL_EDIF_BIBLIO <- "biblio"
	COL_EDIF_DATE_DESTR <- "dateDestr"
	COL_EDIF_ID_SAVE <- "idsave"
	COL_EDIF_LOC <- "localisation"
	
# village table
	COL_VILG_ID <- "id"
	COL_VILG_NAME <- "nom"
	COL_VILG_GEOMETRY <- "geom"
	COL_VILG_SURF <- "area"
	COL_VILG_PERIM <- "perimeter"
	COL_VILG_TYPE <- "type"
	COL_VILG_ID_SAVE <- "idsave"
	COL_VILG_DATE_FRST_OCC <- "datePremiereMention"
	COL_VILG_DATE_CREATED <- "dateCreation"

# cardinal palace table
	COL_CARD_ID <- "id"
	COL_CARD_GEOMETRY <- "geom"
	COL_CARD_NAME <- "nom"
	COL_CARD_DASSIGN <- "dassign"
	COL_CARD_LABEL_X <- "x_etiq"
	COL_CARD_LABEL_Y <- "y_etiq"
	COL_CARD_LABEL_ANGLE <- "angle_etiq"
	COL_CARD_TYPE <- "type"
	COL_CARD_ID_SAVE <- "idsave"
	
# gate table
	COL_GATE_ID <- "id"
	COL_GATE_NAME_LAT <- "nomlatin"
	COL_GATE_NAME_FRE <- "nom"
	COL_GATE_TYPE <- "type"
	COL_GATE_GEOMETRY <- "the_geom"
	COL_GATE_COORD_X <- "xcoord"
	COL_GATE_COORD_Y <- "ycoord"
	COL_GATE_ID_SAVE <- "idsave"
	COL_GATE_DATE_FRST_OCC <- "datePremiereMention"
	COL_GATE_DATE_BUILD_START1 <- "dateDebutConst1"
	COL_GATE_DATE_BUILD_END1 <- "dateFinConst1"
	COL_GATE_DATE_DESTR1 <- "dateDestr1"
	COL_GATE_DATE_BUILD_START2 <- "dateDebutConst2"
	COL_GATE_DATE_BUILD_END2 <- "dateFinConst2"
	COL_GATE_DATE_DESTR2 <- "dateDestr2"
	
# area table
	COL_AREA_ID <- "idQuartier"
	COL_AREA_NAME_LAT <- "nom"
	COL_AREA_NAME_FRE <- "nomtrad"
	COL_AREA_SURF <- "area"
	COL_AREA_PERIM <- "perimeter"
	COL_AREA_GEOMETRY <- "the_geom"
	
# wall table
	COL_WALL_ID <- "id"
	COL_WALL_GEOMETRY <- "geom"
	COL_WALL_NAME_LAT <- "nomlatin"
	COL_WALL_NAME_FRE <- "nom"
	COL_WALL_TYPE <- "type"
	COL_WALL_ID_SAVE <- "idsave"
	COL_WALL_DATE_FRST_OCC <- "datePremiereMention"
	COL_WALL_DATE_BUILD_START1 <- "dateDebutConst1"
	COL_WALL_DATE_BUILD_END1 <- "dateFinConst1"
	COL_WALL_DATE_DESTR1 <- "dateDestr1"
	COL_WALL_DATE_BUILD_START2 <- "dateDebutConst2"
	COL_WALL_DATE_BUILD_END2 <- "dateFinConst2"
	COL_WALL_DATE_DESTR2 <- "dateDestr2"

# landmark table
	COL_LDMRK_ID <- "id"
	COL_LDMRK_GEOMETRY <- "geom"
	COL_LDMRK_TYPE <- "type"
	COL_LDMRK_NAME <- "nom"
	COL_LDMRK_DATE_DERIV <- "dateDeriv"
	COL_LDMRK_DATE_COUV <- "dateCouv"
	COL_LDMRK_SRC <- "sources"
	COL_LDMRK_BIBLIO <- "biblio"
	COL_LDMRK_DATE <- "date"
	COL_LDMRK_ID_SAVE <- "idsave"

# street table
	COL_STREET_ID <- "id"
	COL_STREET_NAME <- "nom"
	COL_STREET_NAME_CURR <- "nomActuel"
	COL_STREET_GEOMETRY <- "the_geom"
	COL_STREET_LENGTH <- "length"
	COL_STREET_TYPE <- "type"
	COL_STREET_LOC <- "detailLoc"
	
# ownership table
	COL_OWN_ID <- "idBienIndividu"
	COL_OWN_PERS_ID <- "idIndividu"
	COL_OWN_EST_ID <- "idBien"
	COL_OWN_DATE1 <- "date1reconnaissance"
	COL_OWN_DATE2 <- "date2reconnaissance"
	COL_OWN_INHER_ORDER <- "ordreSuccession"
	COL_OWN_MEANS <- "moyenAcqui"
	COL_OWN_SAFE <- "sureteDate"

# spatial links table
	COL_CONF_ID <- "idConfront"
	COL_CONF_LOC_LAT <- "localisation"
	COL_CONF_LOC_NORM <- "localisationTN"
	COL_CONF_EST1_ID <- "idBien1"
	COL_CONF_EST2_ID <- "idBien2"
	COL_CONF_FIX_ID <- "idInvariant"
	COL_CONF_AREA_ID <- "idQuartierParoisse"

# estate fee table
	COL_FEE_ID <- "idRedevance"
	COL_FEE_TYPE <- "type"
	COL_FEE_CURRENCY <- "espece"
	COL_FEE_DETAIL <- "detailRedev"
	COL_FEE_AMOUNT_RAW1 <- "montant"
	COL_FEE_AMOUNT_NORM1 <- "montantN"
	COL_FEE_AMOUNT_RAW2 <- "montant2"
	COL_FEE_AMOUNT_INCR <- "echelonMontant"

# source table
	COL_SRC_ID <- "idsource"
	COL_SRC_NATURE <- "nature"
	COL_SRC_LOC <- "lieuDeConservation"
	COL_SRC_NAME <- "nom"
	COL_SRC_SUPPORT <- "support"
	COL_SRC_DIM <- "dimension"
	COL_SRC_LEAF <- "feuillet"
	COL_SRC_AUTH <- "redacteur"
	COL_SRC_DATE_START <- "datedebut"
	COL_SRC_DATE_END <- "datefin"
	COL_SRC_COTE <- "cote"
	
# values
	# social links
	VAL_SOC_TYPE_FAMILY <- "LIFAM"
	VAL_SOC_TYPE_WORK <- "LITRA"
	#confront links
	VAL_CONF_TYPE_COTE <- "a cote de"
	VAL_CONF_TYPE_ANGLE <- "a l angle de"
	VAL_CONF_TYPE_ENTREE <- "a l entree de"
	VAL_CONF_TYPE_INTERIEUR <- "a l interieur de"
	VAL_CONF_TYPE_OPPOSE <- "a l oppose de"
	VAL_CONF_TYPE_DEBUT <- "au debut de"
#	VAL_CONF_TYPE_MILIEU <- "au milieu"
	VAL_CONF_TYPE_EST <- "confronte a l est avec"
#"confronte a l occident avec"
	VAL_CONF_TYPE_OUEST <- "confronte a l ouest avec"
	VAL_CONF_TYPE_NORD <- "confronte au nord avec"
	VAL_CONF_TYPE_SUD <- "confronte au sud avec"
	VAL_CONF_TYPE_MISC <- "confronte avec"
	VAL_CONF_TYPE_MULT2 <- "confronte de deux cotes avec"
	VAL_CONF_TYPE_MULT3 <- "confronte de trois cotes avec"
	VAL_CONF_TYPE_ARRIERE <- "confronte du cote arriere avec"
	VAL_CONF_TYPE_AVANT <- "confronte du cote avant avec"
	VAL_CONF_TYPE_INFERIEUR <- "confronte du cote inferieur avec"
	VAL_CONF_TYPE_LATERAL <- "confronte du cote lateral avec"
	VAL_CONF_TYPE_POSTERIEUR <- "confronte du cote posterieur avec"
	VAL_CONF_TYPE_POSSIBLE <- "confronte peut-etre avec"
	VAL_CONF_TYPE_CONTIGU <- "contigu"
	VAL_CONF_TYPE_DANS <- "dans"
	VAL_CONF_TYPE_DERRIERE <- "derriere"
	VAL_CONF_TYPE_DEVANT <- "devant"
	VAL_CONF_TYPE_EGALE <- "egale"
#	VAL_CONF_TYPE_ENTRE <- "entre"
	VAL_CONF_TYPE_EXTERIEUR <- "est a l exterieur de"
	VAL_CONF_TYPE_SOUS <- "sous"
	VAL_CONF_TYPE_SUR <- "sur"
	VAL_CONF_TYPE_VERS <- "vers"
	



#############################################################################################
# graph-related values

# graph attributes
GR_TYPE <- "type"
	GR_TYPE_SOC <- "Social"
	GR_TYPE_EST <- "Estate"

# node attributes
ND_NAME <- "name"

# types of links 
LK_TYPE <- "type"
	LK_TYPE_ALL <- "Full"
	# social
	LK_TYPE_FAM <- "Familial"
	LK_TYPE_PRO <- "Professional"
	LK_TYPE_SOC_LST <- c(LK_TYPE_ALL, LK_TYPE_FAM, LK_TYPE_PRO)
	# confront
# link description
LK_DESCR <- "description"

# levels
LV_ESTATE <- "lv_estate"

# map to convert link attribute values
MAP_TABLE2GRAPH <- c()
	# social links
	MAP_TABLE2GRAPH[VAL_SOC_TYPE_FAMILY] <- LK_TYPE_FAM
	MAP_TABLE2GRAPH[VAL_SOC_TYPE_WORK] <- LK_TYPE_PRO
	# confront links

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
LONG_NAME[COL_FIX_ID] <- "Fix ID"
LONG_NAME[COL_FIX_TYPE] <- "Type"
LONG_NAME[COL_FIX_NAME] <- "Name"
LONG_NAME[COL_FIX_GEOMETRY] <- "Geometry"
LONG_NAME[COL_EDIF_ID] <- "ID"
LONG_NAME[COL_EDIF_GEOMETRY] <- "Geometry"
LONG_NAME[COL_EDIF_TYPE] <- "Type"
LONG_NAME[COL_EDIF_NAME] <- "Name"
LONG_NAME[COL_EDIF_STATUS] <- "Status"
LONG_NAME[COL_EDIF_DATE_FRST_OCC] <- "First occurrence date"
LONG_NAME[COL_EDIF_DATE_BUILD_START] <- "Construction start date"
LONG_NAME[COL_EDIF_DATE_BUILD_END] <- "Construction end date"
LONG_NAME[COL_EDIF_SRC] <- "Source"
LONG_NAME[COL_EDIF_BIBLIO] <- "Biblio"
LONG_NAME[COL_EDIF_DATE_DESTR] <- "Destruction date"
LONG_NAME[COL_EDIF_ID_SAVE] <- "Save ID"
LONG_NAME[COL_EDIF_LOC] <- "Localization"
LONG_NAME[COL_VILG_ID] <- "ID"
LONG_NAME[COL_VILG_NAME] <- "Name"
LONG_NAME[COL_VILG_GEOMETRY] <- "Geometry"
LONG_NAME[COL_VILG_SURF] <- "Area"
LONG_NAME[COL_VILG_PERIM] <- "Perimeter"
LONG_NAME[COL_VILG_TYPE] <- "Type"
LONG_NAME[COL_VILG_ID_SAVE] <- "Save ID"
LONG_NAME[COL_VILG_DATE_FRST_OCC] <- "First occurrence date"
LONG_NAME[COL_VILG_DATE_CREATED] <- "Creation date"
LONG_NAME[COL_CARD_ID] <- "ID"
LONG_NAME[COL_CARD_GEOMETRY] <- "Geometry"
LONG_NAME[COL_CARD_NAME] <- "Name"
LONG_NAME[COL_CARD_DASSIGN] <- "D assign"
LONG_NAME[COL_CARD_LABEL_X] <- "X label"
LONG_NAME[COL_CARD_LABEL_Y] <- "Y label"
LONG_NAME[COL_CARD_LABEL_ANGLE] <- "Angle label"
LONG_NAME[COL_CARD_TYPE] <- "Type"
LONG_NAME[COL_CARD_ID_SAVE] <- "Save ID"
LONG_NAME[COL_GATE_ID] <- "ID"
LONG_NAME[COL_GATE_NAME_LAT] <- "Name (original)"
LONG_NAME[COL_GATE_NAME_FRE] <- "Name"
LONG_NAME[COL_GATE_TYPE] <- "Type"
LONG_NAME[COL_GATE_GEOMETRY] <- "Geometry"
LONG_NAME[COL_GATE_COORD_X] <- "X coordinate"
LONG_NAME[COL_GATE_COORD_Y] <- "Y coordinate"
LONG_NAME[COL_GATE_ID_SAVE] <- "Save ID"
LONG_NAME[COL_GATE_DATE_FRST_OCC] <- "First occurrence date"
LONG_NAME[COL_GATE_DATE_BUILD_START1] <- "Construction start date #1"
LONG_NAME[COL_GATE_DATE_BUILD_END1] <- "Construction end date #1"
LONG_NAME[COL_GATE_DATE_DESTR1] <- "Destruction date #1"
LONG_NAME[COL_GATE_DATE_BUILD_START2] <- "Construction start date #2"
LONG_NAME[COL_GATE_DATE_BUILD_END2] <- "Construction end date #2"
LONG_NAME[COL_GATE_DATE_DESTR2] <- "Destruction date #2"
LONG_NAME[COL_AREA_ID] <- "Area ID"
LONG_NAME[COL_AREA_NAME_LAT] <- "Name"
LONG_NAME[COL_AREA_NAME_FRE] <- "Name (translated)"
LONG_NAME[COL_AREA_SURF] <- "Area"
LONG_NAME[COL_AREA_PERIM] <- "Perimeter"
LONG_NAME[COL_AREA_GEOMETRY] <- "Geometry"
LONG_NAME[COL_WALL_ID] <- "ID"
LONG_NAME[COL_WALL_GEOMETRY] <- "Geometry"
LONG_NAME[COL_WALL_NAME_LAT] <- "Name (original)"
LONG_NAME[COL_WALL_NAME_FRE] <- "Name"
LONG_NAME[COL_WALL_TYPE] <- "Type"
LONG_NAME[COL_WALL_ID_SAVE] <- "Save ID"
LONG_NAME[COL_WALL_DATE_FRST_OCC] <- "First occurrence date"
LONG_NAME[COL_WALL_DATE_BUILD_START1] <- "Construction start date #1"
LONG_NAME[COL_WALL_DATE_BUILD_END1] <- "Construction end date #1"
LONG_NAME[COL_WALL_DATE_DESTR1] <- "Destruction date #1"
LONG_NAME[COL_WALL_DATE_BUILD_START2] <- "Construction start date #2"
LONG_NAME[COL_WALL_DATE_BUILD_END2] <- "Construction end date #2"
LONG_NAME[COL_WALL_DATE_DESTR2] <- "Destruction date #2"
LONG_NAME[COL_LDMRK_ID] <- "ID"
LONG_NAME[COL_LDMRK_GEOMETRY] <- "Geometry"
LONG_NAME[COL_LDMRK_TYPE] <- "Type"
LONG_NAME[COL_LDMRK_NAME] <- "Name"
LONG_NAME[COL_LDMRK_DATE_DERIV] <- "Date Deriv"
LONG_NAME[COL_LDMRK_DATE_COUV] <- "Date Couv"
LONG_NAME[COL_LDMRK_SRC] <- "Sources"
LONG_NAME[COL_LDMRK_BIBLIO] <- "Biblio"
LONG_NAME[COL_LDMRK_DATE] <- "Date"
LONG_NAME[COL_LDMRK_ID_SAVE] <- "Save ID"
LONG_NAME[COL_STREET_ID] <- "ID"
LONG_NAME[COL_STREET_NAME] <- "Name"
LONG_NAME[COL_STREET_NAME_CURR] <- "Name (current)"
LONG_NAME[COL_STREET_GEOMETRY] <- "Geometry"
LONG_NAME[COL_STREET_LENGTH] <- "Length"
LONG_NAME[COL_STREET_TYPE] <- "Type"
LONG_NAME[COL_STREET_LOC] <- "Location details"
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
