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
		FILE_IN_ANAL_CONFR_LINKS_ALL <- file.path(FOLDER_IN_ANAL,"Table_Localisation_Confront_tout.csv")
		FILE_IN_ANAL_CONFR_LINKS_14c <- file.path(FOLDER_IN_ANAL,"Table_Localisation_Confront_XIV.csv")
		FILE_IN_ANAL_GATE_NODES <- file.path(FOLDER_IN_ANAL,"Table_PortePortail.csv")
		FILE_IN_ANAL_AREA_NODES <- file.path(FOLDER_IN_ANAL,"Table_QuartierParoisse.csv")
		FILE_IN_ANAL_ESTATE_FEE <- file.path(FOLDER_IN_ANAL,"Table_Redevance.csv")
		FILE_IN_ANAL_WALL_NODES <- file.path(FOLDER_IN_ANAL,"Table_Rempart.csv")
		FILE_IN_ANAL_LDMRK_NODES <- file.path(FOLDER_IN_ANAL,"Table_RepereGeo.csv")
		FILE_IN_ANAL_STREET_NODES <- file.path(FOLDER_IN_ANAL,"Table_Rue.csv")
		FILE_IN_ANAL_SRC_NODES <- file.path(FOLDER_IN_ANAL,"Table_Source.csv")
		# split streets
		FILE_IN_ANAL_STRT_SHORT <- file.path(FOLDER_IN_ANAL,"Table_Rue_Courte.csv")
		FILE_IN_ANAL_SPLIT_FIX <- file.path(FOLDER_IN_ANAL,"Table_Coupe_Invariant.csv")
		FILE_IN_ANAL_SPLIT_CONFR <- file.path(FOLDER_IN_ANAL,"Table_Coupe_Confront.csv")
		FILE_IN_ANAL_SPLIT_CONFR_ARTIF <- file.path(FOLDER_IN_ANAL,"Table_Coupe_Confront_Artificiel.csv")
		# street confronts
		FILE_IN_ANAL_CONFR_STRT_EDIFICES <- file.path(FOLDER_IN_ANAL,"Table_Confront_Edifice_Rue.csv")
		FILE_IN_ANAL_CONFR_STRT_EDIFICES_SPLIT <- file.path(FOLDER_IN_ANAL,"Table_Confront_Edifice_Rue_Split.csv")
		FILE_IN_ANAL_CONFR_STRT_STREETS <- file.path(FOLDER_IN_ANAL,"Table_Confront_Topo_Rue.csv")
		FILE_IN_ANAL_CONFR_STRT_STREETS_SPLIT <- file.path(FOLDER_IN_ANAL,"Table_Confront_Topo_Rue_Split.csv")

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
	COL_PERS_MENTION <- "mention1"					# year of first mention in a text
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

# locations in general
	COL_LOC_ID <- "idExterne"
	COL_LOC_TYPE <- "typeExterne"
	# estimated geographic coordinates
	COL_LOC_HYP_LAT <- "latitudeHypo"	# other projection
	COL_LOC_HYP_LON <- "longitudeHypo"	# other projection
	COL_LOC_X <- "xcoord"				# lambert93 coordinates
	COL_LOC_Y <- "ycoord"				# lambert93 coordinates
	COL_LOC_INTER_X <- "lonEst"			# interpolated coordinates
	COL_LOC_INTER_Y <- "latEst"			# interpolated coordinates
	COL_LOC_INTER <- "interpolated"		# whether the coordinates were interpolated
	COL_LOC_EXCLUDE <- "distExcl"		# whether to ignore the node during distance computation
	# layout coordinates
	COL_LOC_ALGO_X <- "x2"
	COL_LOC_ALGO_Y <- "y2"
	
# estate table
	# ids
	COL_EST_ID <- "idBien"							# unique id of an estate
	COL_EST_AREA_ID <- "idQuartier"					# geographical subdivision
	COL_EST_AREA_ID_RESTR <- "idQuartierRestr"		# geographical subdivision narrowed to paroisses
	COL_EST_STREET_ID <- "idRue"					# unique street id
	COL_EST_VILLAGE_ID <- "idBourg"					# smaller geographical subdivision
	COL_EST_FEE_ID <- "idRedevance"					# tax fees
	COL_EST_LORDSHIP_ID <- "idSeigneurie"			# the estate is attached to a lordship for tax purposes
	COL_EST_DECLARATION_ID <- "idDeclaration"		# id of the associated tax declation 
	# misc
	COL_EST_TYPE_LAT <- "type"
	COL_EST_TYPE_FRE <- "typeT"
	COL_EST_QUALIF_LAT <- "qualif"
	COL_EST_QUALIF_NORM <- "qualifTN"
	COL_EST_DETAIL <- "detail"
	COL_EST_MEASURE <- "mesure"
	COL_EST_MATERIALS <- "materiaux"
	COL_EST_GEOMETRY <- "geom"
	COL_EST_MENTION <- "mention1"					# year of first mention in a text
	COL_EST_POSITION <- "loc"						# intra- vs. extra-muros
	COL_EST_POSITION_RESTR <- "locRestr"			# strictly intra- vs. extra-muros
	# components
	COL_EST_COMP_LAB1 <- "composant1"				# the estate may be constituted of several separte parts
	COL_EST_COMP_NORM1 <- "composantN1"				# normalized component name
	COL_EST_COMP_NBR1 <- "nbrComposant1"			# number of instances of the first part
	COL_EST_COMP_TYPE1 <- "typeComposant1"			# type of the first part
	COL_EST_COMP_TYPE_S1 <- "typeSimpleComposant1"	# simplified version of the above
	COL_EST_COMP_LAB2 <- "composant2"				# as above, fro the remaining parts (up to 6 distinct parts)
	COL_EST_COMP_NORM2 <- "composantN2"
	COL_EST_COMP_NBR2 <- "nbrComposant2"
	COL_EST_COMP_TYPE2 <- "typeComposant2"
	COL_EST_COMP_TYPE_S2 <- "typeSimpleComposant2"
	COL_EST_COMP_LAB3 <- "composant3"
	COL_EST_COMP_NORM3 <- "composantN3"
	COL_EST_COMP_TYPE3 <- "typeComposant3"
	COL_EST_COMP_TYPE_S3 <- "typeSimpleComposant3"
	COL_EST_COMP_LAB4 <- "composant4"
	COL_EST_COMP_NORM4 <- "composantN4"
	COL_EST_COMP_TYPE4 <- "typeComposant4"
	COL_EST_COMP_TYPE_S4 <- "typeSimpleComposant4"
	COL_EST_COMP_LAB5 <- "composant5"
	COL_EST_COMP_NORM5 <- "composantN5"
	COL_EST_COMP_TYPE5 <- "typeComposant5"
	COL_EST_COMP_TYPE_S5 <- "typeSimpleComposant5"
	COL_EST_COMP_LAB6 <- "composant6"
	COL_EST_COMP_NORM6 <- "composantN6"
	COL_EST_COMP_TYPE6 <- "typeComposant6"
	COL_EST_COMP_TYPE_S6 <- "typeSimpleComposant6"
	# NOTE: declared estate = COL_EST_DECLARATION_ID | COL_EST_FEE_ID
	COL_EST_DECLARED <- "declared"

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
	COL_STREET_ID_SPLIT <- "id_ajout"
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
	COL_CONF_LOC_NORM_ARTIF <- "lienArtificiel"
	COL_CONF_EDIF_ID <- "idEdifice"
	COL_CONF_EST1_ID <- "idBien1"
	COL_CONF_EST2_ID <- "idBien2"
	COL_CONF_FIX_ID <- "idInvariant"
	COL_CONF_FIX1_ID <- "idInvariant1"
	COL_CONF_FIX2_ID <- "idInvariant2"
	COL_CONF_FIX_ID_SPLIT <- "idInvariant_ajout"
	COL_CONF_AREA_ID <- "idQuartierParoisse"
	COL_CONF_STREET_ID <- "idRue"
	COL_CONF_STREET2_ID <- "idRue2"
	
# estate fee table
	COL_FEE_ID <- "idRedevance"
	COL_FEE_TYPE <- "type"
	COL_FEE_CURRENCY <- "espece"
	COL_FEE_DETAIL <- "detailRedev"
	COL_FEE_AMOUNT_RAW1 <- "montant"
	COL_FEE_AMOUNT_NORM1 <- "montantN"
	COL_FEE_AMOUNT_CAT1 <- "montantCat"
	COL_FEE_AMOUNT_RAW2 <- "montant2"
	COL_FEE_AMOUNT_INCR <- "echelonMontant"
	#
	FEE_BREAKS <- c(0, 34, 60, 144, 336, 624, 1296, 3600, 30000)
	FEE_CATS <- c(paste0("[",FEE_BREAKS[1],";",FEE_BREAKS[2],"]"), sapply(3:length(FEE_BREAKS), function(b) paste0("]",FEE_BREAKS[b-1],";",FEE_BREAKS[b],"]")))

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
	VAL_SOC_TYPE_ECCL <- "LIECCL"
	#confront links
	VAL_CONF_TYPE_ARTIF <- "artificial"
	VAL_CONF_TYPE_COTE <- "a cote de"
	VAL_CONF_TYPE_ANGLE <- "a l angle de"
	VAL_CONF_TYPE_ENTREE <- "a l entree de"
	VAL_CONF_TYPE_INTERIEUR <- "a l interieur de"
	VAL_CONF_TYPE_OPPOSE <- "a l oppose de"
	VAL_CONF_TYPE_DEBUT <- "au debut de"
	VAL_CONF_TYPE_DELA <- "au dela de"
	VAL_CONF_TYPE_DESSOUS <- "en dessous de"
	VAL_CONF_TYPE_DESSUS <- "au dessus de"
	VAL_CONF_TYPE_MILIEU <- "au milieu"
	VAL_CONF_TYPE_EST <- "confronte a l est avec"
	VAL_CONF_TYPE_OCC <- "confronte a l occident avec"
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
	VAL_CONF_TYPE_EGALE <- "egal"
	VAL_CONF_TYPE_ENTRE <- "entre"
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
	
# subtypes of graphs
GR_EST_FULL <- "full"					# all relations
GR_EST_FLAT_REL <- "flat_relations"		# keeps everything but the membership relations	
GR_EST_ESTATE_LEVEL <- "estate_level"	# basically only keeps real estate entities, short streets, and other punctual entities
GR_EST_FLAT_MINUS <- "flat_minus"		# stricter version of the flat net: no wall and river

# node attributes
ND_NAME <- "name"

# types of links 
LK_TYPE <- "type"
	LK_TYPE_ALL <- "Full"
	# social
	LK_TYPE_FAM <- "Familial"
	LK_TYPE_PRO <- "Professional"
	LK_TYPE_ECCL <- "Ecclesiastical"
	LK_TYPE_SOC_LST <- c(
		LK_TYPE_ALL, 
		LK_TYPE_FAM, 
		LK_TYPE_PRO, 
		LK_TYPE_ECCL
	)
	# confront
	LK_TYPE_FLATREL_VALS <- c(
		VAL_CONF_TYPE_INTERIEUR, 
		VAL_CONF_TYPE_EST, 
		VAL_CONF_TYPE_OUEST, 
		VAL_CONF_TYPE_NORD, 
		VAL_CONF_TYPE_SUD, 
		VAL_CONF_TYPE_MISC, 
		VAL_CONF_TYPE_EGALE
	)
	LK_TYPE_PLOT_VALS <- c(
		VAL_CONF_TYPE_EST, 
		VAL_CONF_TYPE_OUEST, 
		VAL_CONF_TYPE_NORD, 
		VAL_CONF_TYPE_SUD, 
		VAL_CONF_TYPE_MISC, 
		VAL_CONF_TYPE_EGALE,
		VAL_CONF_TYPE_INTERIEUR,
		VAL_CONF_TYPE_EXTERIEUR,
		VAL_CONF_TYPE_DELA,
		VAL_CONF_TYPE_ENTRE
	)

# link description
LK_DESCR <- "description"

# map to convert link attribute values
MAP_TABLE2GRAPH <- c()
	# social links
	MAP_TABLE2GRAPH[VAL_SOC_TYPE_FAMILY] <- LK_TYPE_FAM
	MAP_TABLE2GRAPH[VAL_SOC_TYPE_WORK] <- LK_TYPE_PRO
	MAP_TABLE2GRAPH[VAL_SOC_TYPE_ECCL] <- LK_TYPE_ECCL
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
LONG_NAME[paste0("Own_",COL_PERS_ID)] <- LONG_NAME[COL_PERS_ID]
LONG_NAME[COL_PERS_MENTION] <- "Mention"
LONG_NAME[paste0("Own_",COL_PERS_MENTION)] <- LONG_NAME[COL_PERS_MENTION]
LONG_NAME[COL_PERS_NAME_FULL_LAT] <- "Full name (translated)"
LONG_NAME[paste0("Own_",COL_PERS_NAME_FULL_LAT)] <- LONG_NAME[COL_PERS_NAME_FULL_LAT]
LONG_NAME[COL_PERS_NAME_FULL_NORM] <- "Full name (normalized)"
LONG_NAME[paste0("Own_",COL_PERS_NAME_FULL_NORM)] <- LONG_NAME[COL_PERS_NAME_FULL_NORM]
LONG_NAME[COL_PERS_NAME_FIRST] <- "Firstname"
LONG_NAME[paste0("Own_",COL_PERS_NAME_FIRST)] <- LONG_NAME[COL_PERS_NAME_FIRST]
LONG_NAME[COL_PERS_NAME_LAST] <- "Lastname"
LONG_NAME[paste0("Own_",COL_PERS_NAME_LAST)] <- LONG_NAME[COL_PERS_NAME_LAST]
LONG_NAME[COL_PERS_NAME_NICK] <- "Nickname"
LONG_NAME[paste0("Own_",COL_PERS_NAME_NICK)] <- LONG_NAME[COL_PERS_NAME_NICK]
LONG_NAME[COL_PERS_NAME_TYPE] <- "Name type"
LONG_NAME[paste0("Own_",COL_PERS_NAME_TYPE)] <- LONG_NAME[COL_PERS_NAME_TYPE]
LONG_NAME[COL_PERS_GENDER] <- "Gender"
LONG_NAME[paste0("Own_",COL_PERS_GENDER)] <- LONG_NAME[COL_PERS_GENDER]
LONG_NAME[COL_PERS_IDENTIFICATION] <- "Identification"
LONG_NAME[paste0("Own_",COL_PERS_IDENTIFICATION)] <- LONG_NAME[COL_PERS_IDENTIFICATION]
LONG_NAME[COL_PERS_RESIDENCE] <- "Residence"
LONG_NAME[paste0("Own_",COL_PERS_RESIDENCE)] <- LONG_NAME[COL_PERS_RESIDENCE]
LONG_NAME[COL_PERS_TITLE_LAT1] <- "Title 1 (original)"
LONG_NAME[paste0("Own_",COL_PERS_TITLE_LAT1)] <- LONG_NAME[COL_PERS_TITLE_LAT1]
LONG_NAME[COL_PERS_TITLE_FRE1] <- "Title 1 (translated)"
LONG_NAME[paste0("Own_",COL_PERS_TITLE_FRE1)] <- LONG_NAME[COL_PERS_TITLE_FRE1]
LONG_NAME[COL_PERS_TITLE_NORM1] <- "Title 1 (normalized)"
LONG_NAME[paste0("Own_",COL_PERS_TITLE_NORM1)] <- LONG_NAME[COL_PERS_TITLE_NORM1]
LONG_NAME[COL_PERS_TITLE_LAT2] <- "Title 2 (original)"
LONG_NAME[paste0("Own_",COL_PERS_TITLE_LAT2)] <- LONG_NAME[COL_PERS_TITLE_LAT2]
LONG_NAME[COL_PERS_TITLE_FRE2] <- "Title 2 (translated)"
LONG_NAME[paste0("Own_",COL_PERS_TITLE_FRE2)] <- LONG_NAME[COL_PERS_TITLE_FRE2]
LONG_NAME[COL_PERS_TITLE_NORM2] <- "Title 2 (normalized)"
LONG_NAME[paste0("Own_",COL_PERS_TITLE_NORM2)] <- LONG_NAME[COL_PERS_TITLE_NORM2]
LONG_NAME[COL_PERS_OCC_LAT1] <- "Occupation 1 (original)"
LONG_NAME[paste0("Own_",COL_PERS_OCC_LAT1)] <- LONG_NAME[COL_PERS_OCC_LAT1]
LONG_NAME[COL_PERS_OCC_FRE1] <- "Occupation 1 (translated)"
LONG_NAME[paste0("Own_",COL_PERS_OCC_FRE1)] <- LONG_NAME[COL_PERS_OCC_FRE1]
LONG_NAME[COL_PERS_OCC_NORM1] <- "Occupation 1 (normalized)"
LONG_NAME[paste0("Own_",COL_PERS_OCC_NORM1)] <- LONG_NAME[COL_PERS_OCC_NORM1]
LONG_NAME[COL_PERS_OCC_LAT2] <- "Occupation 2 (original)"
LONG_NAME[paste0("Own_",COL_PERS_OCC_LAT2)] <- LONG_NAME[COL_PERS_OCC_LAT2]
LONG_NAME[COL_PERS_OCC_FRE2] <- "Occupation 2 (translated)"
LONG_NAME[paste0("Own_",COL_PERS_OCC_FRE2)] <- LONG_NAME[COL_PERS_OCC_FRE2]
LONG_NAME[COL_PERS_OCC_NORM2] <- "Occupation 2 (normalized)"
LONG_NAME[paste0("Own_",COL_PERS_OCC_NORM2)] <- LONG_NAME[COL_PERS_OCC_NORM2]
LONG_NAME[COL_PERS_OCC_CAT] <- "Occupation category"
LONG_NAME[paste0("Own_",COL_PERS_OCC_CAT)] <- LONG_NAME[COL_PERS_OCC_CAT]
LONG_NAME[COL_PERS_OCC_THEME] <- "Occupation theme"
LONG_NAME[paste0("Own_",COL_PERS_OCC_THEME)] <- LONG_NAME[COL_PERS_OCC_THEME]
LONG_NAME[COL_PERS_ECCL_LAT] <- "Ecclesiastical office (original)"
LONG_NAME[paste0("Own_",COL_PERS_ECCL_LAT)] <- LONG_NAME[COL_PERS_ECCL_LAT]
LONG_NAME[COL_PERS_ECCL_FRE] <- "Ecclesiastical office (translated)"
LONG_NAME[paste0("Own_",COL_PERS_ECCL_FRE)] <- LONG_NAME[COL_PERS_ECCL_FRE]
LONG_NAME[COL_PERS_ECCL_NORM] <- "Ecclesiastical office (normalized)"
LONG_NAME[paste0("Own_",COL_PERS_ECCL_NORM)] <- LONG_NAME[COL_PERS_ECCL_NORM]
LONG_NAME[COL_PERS_HEALTH_LAT] <- "Health (original)"
LONG_NAME[paste0("Own_",COL_PERS_HEALTH_LAT)] <- LONG_NAME[COL_PERS_HEALTH_LAT]
LONG_NAME[COL_PERS_HEALTH_FRE] <- "Health (translated)"
LONG_NAME[paste0("Own_",COL_PERS_HEALTH_FRE)] <- LONG_NAME[COL_PERS_HEALTH_FRE]
LONG_NAME[COL_PERS_CITY_LAT] <- "Home city (original)"
LONG_NAME[paste0("Own_",COL_PERS_CITY_LAT)] <- LONG_NAME[COL_PERS_CITY_LAT]
LONG_NAME[COL_PERS_CITY_FRE] <- "Home city (translated)"
LONG_NAME[paste0("Own_",COL_PERS_CITY_FRE)] <- LONG_NAME[COL_PERS_CITY_FRE]
LONG_NAME[COL_PERS_DIOC_LAT] <- "Diocese (original)"
LONG_NAME[paste0("Own_",COL_PERS_DIOC_LAT)] <- LONG_NAME[COL_PERS_DIOC_LAT]
LONG_NAME[COL_PERS_DIOC_FRE] <- "Diocese (translated)"
LONG_NAME[paste0("Own_",COL_PERS_DIOC_FRE)] <- LONG_NAME[COL_PERS_DIOC_FRE]
LONG_NAME[COL_PERS_STATUS_LAT] <- "Status (original)"
LONG_NAME[paste0("Own_",COL_PERS_STATUS_LAT)] <- LONG_NAME[COL_PERS_STATUS_LAT]
LONG_NAME[COL_PERS_STATUS_FRE] <- "Status (translated)"
LONG_NAME[paste0("Own_",COL_PERS_STATUS_FRE)] <- LONG_NAME[COL_PERS_STATUS_FRE]
LONG_NAME[COL_PERS_STATUS_NORM] <- "Statut (normalized)"
LONG_NAME[paste0("Own_",COL_PERS_STATUS_NORM)] <- LONG_NAME[COL_PERS_STATUS_NORM]
#
LONG_NAME[COL_SOC_ID] <- "Social link ID"
LONG_NAME[COL_SOC_SRC] <- "Source person ID"
LONG_NAME[COL_SOC_TGT] <- "Target person ID"
LONG_NAME[COL_SOC_DESCR] <- "Link description"
LONG_NAME[COL_SOC_TYPE] <- "Link type"
#
LONG_NAME[COL_LOC_ID] <- "Normalized ID"
LONG_NAME[COL_LOC_TYPE] <- "Normalized type"
LONG_NAME[COL_LOC_HYP_LAT] <- "Latitude"
LONG_NAME[COL_LOC_HYP_LON] <- "Longitude"
LONG_NAME[COL_LOC_X] <- "Lambert93 latitude"
LONG_NAME[COL_LOC_Y] <- "Lambert93 longitude"
LONG_NAME[COL_LOC_INTER_X] <- "Interpolated latitude (possibly)"
LONG_NAME[COL_LOC_INTER_Y] <- "Interpolated longitude (possibly)"
LONG_NAME[COL_LOC_INTER] <- "Interpolated coordinates"
LONG_NAME[COL_LOC_ALGO_X] <- "Layout x coordinate"
LONG_NAME[COL_LOC_ALGO_Y] <- "Layout y coordinate"
#
LONG_NAME[COL_EST_ID] <- "Estate ID"
LONG_NAME[COL_EST_AREA_ID] <- "Area ID"
LONG_NAME[COL_EST_AREA_ID_RESTR] <- "Area ID (narrowed)"
LONG_NAME[COL_EST_STREET_ID] <- "Street ID"
LONG_NAME[COL_EST_VILLAGE_ID] <- "Village ID"
LONG_NAME[COL_EST_FEE_ID] <- "Fee ID"
LONG_NAME[COL_EST_LORDSHIP_ID] <- "Lordship ID"
LONG_NAME[COL_EST_DECLARATION_ID] <- "Declaration ID"
LONG_NAME[COL_EST_DECLARED] <- "Declaration status"
LONG_NAME[COL_EST_TYPE_LAT] <- "Estate type (original)"
LONG_NAME[COL_EST_TYPE_FRE] <- "Estate type (translated)"
LONG_NAME[COL_EST_QUALIF_LAT] <- "Estate qualification (original)"
LONG_NAME[COL_EST_QUALIF_NORM] <- "Estate qualification (normalized)"
LONG_NAME[COL_EST_DETAIL] <- "Estate detail"
LONG_NAME[COL_EST_MEASURE] <- "Estate mesure"
LONG_NAME[COL_EST_MENTION] <- "Earliest mention (year)"
LONG_NAME[COL_EST_MATERIALS] <- "Estate materials"
LONG_NAME[COL_EST_GEOMETRY] <- "Estate geom"
LONG_NAME[COL_EST_POSITION] <- "Wall-wise position"
LONG_NAME[COL_EST_POSITION_RESTR] <- "Wall-wise position (narrowed)"
LONG_NAME[COL_EST_COMP_LAB1] <- "Estate component 1"
LONG_NAME[COL_EST_COMP_NORM1] <- "Estate component 1 (normalized)"
LONG_NAME[COL_EST_COMP_NBR1] <- "Estate component 1 number"
LONG_NAME[COL_EST_COMP_TYPE1] <- "Estate component 1 type"
LONG_NAME[COL_EST_COMP_TYPE_S1] <- "Estate component 1 simple type"
LONG_NAME[COL_EST_COMP_LAB2] <- "Estate component 2"
LONG_NAME[COL_EST_COMP_NORM2] <- "Estate component 2 (normalized)"
LONG_NAME[COL_EST_COMP_NBR2] <- "Estate component 2 number"
LONG_NAME[COL_EST_COMP_TYPE2] <- "Estate component 2 type"
LONG_NAME[COL_EST_COMP_TYPE_S2] <- "Estate component 2 simple type"
LONG_NAME[COL_EST_COMP_LAB3] <- "Estate component 3"
LONG_NAME[COL_EST_COMP_NORM3] <- "Estate component 3 (normalized)"
LONG_NAME[COL_EST_COMP_TYPE3] <- "Estate component 3 type"
LONG_NAME[COL_EST_COMP_TYPE_S3] <- "Estate component 3 simple type"
LONG_NAME[COL_EST_COMP_LAB4] <- "Estate component 4"
LONG_NAME[COL_EST_COMP_NORM4] <- "Estate component 4 (normalized)"
LONG_NAME[COL_EST_COMP_TYPE4] <- "Estate component 4 type"
LONG_NAME[COL_EST_COMP_TYPE_S4] <- "Estate component 4 simple type"
LONG_NAME[COL_EST_COMP_LAB5] <- "Estate component 5"
LONG_NAME[COL_EST_COMP_NORM5] <- "Estate component 5 (normalized)"
LONG_NAME[COL_EST_COMP_TYPE5] <- "Estate component 5 type"
LONG_NAME[COL_EST_COMP_TYPE_S5] <- "Estate component 5 simple type"
LONG_NAME[COL_EST_COMP_LAB6] <- "Estate component 6"
LONG_NAME[COL_EST_COMP_NORM6] <- "Estate component 6 (normalized)"
LONG_NAME[COL_EST_COMP_TYPE6] <- "Estate component 6 type"
LONG_NAME[COL_EST_COMP_TYPE_S6] <- "Estate component 6 simple type"
#
LONG_NAME[COL_FIX_ID] <- "Fix ID"
LONG_NAME[COL_FIX_TYPE] <- "Type"
LONG_NAME[COL_FIX_NAME] <- "Name"
LONG_NAME[COL_FIX_GEOMETRY] <- "Geometry"
#
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
#
LONG_NAME[COL_VILG_ID] <- "ID"
LONG_NAME[COL_VILG_NAME] <- "Name"
LONG_NAME[COL_VILG_GEOMETRY] <- "Geometry"
LONG_NAME[COL_VILG_SURF] <- "Area"
LONG_NAME[COL_VILG_PERIM] <- "Perimeter"
LONG_NAME[COL_VILG_TYPE] <- "Type"
LONG_NAME[COL_VILG_ID_SAVE] <- "Save ID"
LONG_NAME[COL_VILG_DATE_FRST_OCC] <- "First occurrence date"
LONG_NAME[COL_VILG_DATE_CREATED] <- "Creation date"
#
LONG_NAME[COL_CARD_ID] <- "ID"
LONG_NAME[COL_CARD_GEOMETRY] <- "Geometry"
LONG_NAME[COL_CARD_NAME] <- "Name"
LONG_NAME[COL_CARD_DASSIGN] <- "D assign"
LONG_NAME[COL_CARD_LABEL_X] <- "X label"
LONG_NAME[COL_CARD_LABEL_Y] <- "Y label"
LONG_NAME[COL_CARD_LABEL_ANGLE] <- "Angle label"
LONG_NAME[COL_CARD_TYPE] <- "Type"
LONG_NAME[COL_CARD_ID_SAVE] <- "Save ID"
#
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
#
LONG_NAME[COL_AREA_ID] <- "Area ID"
LONG_NAME[COL_AREA_NAME_LAT] <- "Name"
LONG_NAME[COL_AREA_NAME_FRE] <- "Name (translated)"
LONG_NAME[COL_AREA_SURF] <- "Area"
LONG_NAME[COL_AREA_PERIM] <- "Perimeter"
LONG_NAME[COL_AREA_GEOMETRY] <- "Geometry"
#
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
#
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
#
LONG_NAME[COL_STREET_ID] <- "ID"
LONG_NAME[COL_STREET_NAME] <- "Name"
LONG_NAME[COL_STREET_NAME_CURR] <- "Name (current)"
LONG_NAME[COL_STREET_GEOMETRY] <- "Geometry"
LONG_NAME[COL_STREET_LENGTH] <- "Length"
LONG_NAME[COL_STREET_TYPE] <- "Type"
LONG_NAME[COL_STREET_LOC] <- "Location details"
#
LONG_NAME[COL_OWN_ID] <- "Ownership ID"
LONG_NAME[COL_OWN_PERS_ID] <- "Owner ID"
LONG_NAME[COL_OWN_EST_ID] <- "Estate ID"
LONG_NAME[COL_OWN_DATE1] <- "Reconnaissance date 1"
LONG_NAME[COL_OWN_DATE2] <- "Reconnaissance date 2"
LONG_NAME[COL_OWN_INHER_ORDER] <- "Inheritance order"
LONG_NAME[COL_OWN_MEANS] <- "Acquisition means"
LONG_NAME[COL_OWN_SAFE] <- "Safety date"
#
LONG_NAME[COL_FEE_ID] <- "Fee ID"
LONG_NAME[COL_FEE_TYPE] <- "Fee type"
LONG_NAME[COL_FEE_CURRENCY] <- "Fee currency"
LONG_NAME[COL_FEE_DETAIL] <- "Fee detail"
LONG_NAME[COL_FEE_AMOUNT_RAW1] <- "Fee amount 1 (raw)"
LONG_NAME[COL_FEE_AMOUNT_NORM1] <- "Fee amount 1 (normalized)"
LONG_NAME[COL_FEE_AMOUNT_CAT1] <- "Fee amount 1 (binned)"
LONG_NAME[COL_FEE_AMOUNT_RAW2] <- "Fee amount 2 (raw)"
LONG_NAME[COL_FEE_AMOUNT_INCR] <- "Fee increment"

# social links
LONG_NAME[VAL_SOC_TYPE_FAMILY] <- "Family"
LONG_NAME[VAL_SOC_TYPE_WORK] <- "Work"
LONG_NAME[VAL_SOC_TYPE_ECCL] <- "Ecclesiastic"
#confront links
LONG_NAME[VAL_CONF_TYPE_ARTIF] <- "Artificial confront"
LONG_NAME[VAL_CONF_TYPE_COTE] <- "Next to"
LONG_NAME[VAL_CONF_TYPE_ANGLE] <- "Corner of"
LONG_NAME[VAL_CONF_TYPE_ENTREE] <- "Entrance of"
LONG_NAME[VAL_CONF_TYPE_INTERIEUR] <- "Inside of"
LONG_NAME[VAL_CONF_TYPE_OPPOSE] <- "In front of"
LONG_NAME[VAL_CONF_TYPE_DEBUT] <- "Start of"
LONG_NAME[VAL_CONF_TYPE_DELA] <- "Beyond"
LONG_NAME[VAL_CONF_TYPE_DESSOUS] <- "Below"
LONG_NAME[VAL_CONF_TYPE_DESSUS] <- "Above"
LONG_NAME[VAL_CONF_TYPE_MILIEU] <- "In the middle of"
LONG_NAME[VAL_CONF_TYPE_EST] <- "East confront"
LONG_NAME[VAL_CONF_TYPE_OCC] <- "Occident of"
LONG_NAME[VAL_CONF_TYPE_OUEST] <- "West confront"
LONG_NAME[VAL_CONF_TYPE_NORD] <- "North confront"
LONG_NAME[VAL_CONF_TYPE_SUD] <- "South confront"
LONG_NAME[VAL_CONF_TYPE_MISC] <- "Confront"
LONG_NAME[VAL_CONF_TYPE_MULT2] <- "Double confront"
LONG_NAME[VAL_CONF_TYPE_MULT3] <- "Triple confront"
LONG_NAME[VAL_CONF_TYPE_ARRIERE] <- "Behind confront"
LONG_NAME[VAL_CONF_TYPE_AVANT] <- "Side confront"
LONG_NAME[VAL_CONF_TYPE_INFERIEUR] <- "Lower side confront"
LONG_NAME[VAL_CONF_TYPE_LATERAL] <- "Lateral side confront"
LONG_NAME[VAL_CONF_TYPE_POSTERIEUR] <- "Posterior side confront"
LONG_NAME[VAL_CONF_TYPE_POSSIBLE] <- "Potential confront"
LONG_NAME[VAL_CONF_TYPE_CONTIGU] <- "Contiguous"
LONG_NAME[VAL_CONF_TYPE_DANS] <- "Inside"
LONG_NAME[VAL_CONF_TYPE_DERRIERE] <- "Behind"
LONG_NAME[VAL_CONF_TYPE_DEVANT] <- "Front"
LONG_NAME[VAL_CONF_TYPE_EGALE] <- "Equal"
LONG_NAME[VAL_CONF_TYPE_ENTRE] <- "Between"
LONG_NAME[VAL_CONF_TYPE_EXTERIEUR] <- "Out of"
LONG_NAME[VAL_CONF_TYPE_SOUS] <- "Under"
LONG_NAME[VAL_CONF_TYPE_SUR] <- "Over"
LONG_NAME[VAL_CONF_TYPE_VERS] <- "Towards"




#############################################################################################
# list of social attributes used in the estate graph
COL_SOC_SELECT <- c(
		COL_PERS_ID, 
		COL_PERS_NAME_FULL_NORM, COL_PERS_NAME_FIRST, COL_PERS_NAME_LAST, COL_PERS_NAME_NICK, COL_PERS_NAME_TYPE,
		COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2,
		COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2,
		COL_PERS_ECCL_NORM,
		COL_PERS_DIOC_FRE, COL_PERS_CITY_FRE,
		COL_PERS_HEALTH_FRE,
		COL_PERS_STATUS_NORM,
		COL_PERS_MENTION,
		COL_PERS_OCC_CAT, COL_PERS_OCC_THEME,
		COL_PERS_IDENTIFICATION,
		COL_PERS_GENDER,
		COL_PERS_RESIDENCE
)

# list of categorical attributes
COL_CAT <- c(
	# social
	COL_PERS_NAME_FIRST, COL_PERS_NAME_LAST, COL_PERS_NAME_NICK, COL_PERS_NAME_TYPE,
	COL_PERS_GENDER, COL_PERS_IDENTIFICATION, COL_PERS_RESIDENCE,
	COL_PERS_ECCL_NORM, #COL_PERS_ECCL_LAT, COL_PERS_ECCL_FRE,
	COL_PERS_HEALTH_FRE, #COL_PERS_HEALTH_LAT,
	COL_PERS_CITY_FRE, #COL_PERS_CITY_LAT,
	COL_PERS_DIOC_FRE, #COL_PERS_DIOC_LAT,
	COL_PERS_OCC_CAT, COL_PERS_OCC_THEME,
	COL_PERS_STATUS_NORM, #COL_PERS_STATUS_LAT, COL_PERS_STATUS_FRE,
	
	# estate
	COL_EST_AREA_ID, COL_EST_AREA_ID_RESTR,
	COL_EST_STREET_ID, COL_EST_VILLAGE_ID, COL_EST_LORDSHIP_ID,
	COL_LOC_TYPE, COL_EST_TYPE_FRE, #COL_EST_TYPE_LAT
	COL_EST_QUALIF_NORM, #COL_EST_QUALIF_LAT
	#COL_EST_DETAIL, COL_EST_MEASURE, COL_EST_MATERIALS,
	COL_EST_DECLARED,
	COL_EST_POSITION, COL_EST_POSITION_RESTR,
	COL_LOC_INTER,
	# edifices
	COL_EDIF_TYPE, COL_EDIF_STATUS, COL_EDIF_LOC, #COL_EDIF_SRC,
	# villages
	COL_VILG_TYPE,
	# palaces
	COL_CARD_TYPE,
	# gates
	COL_GATE_NAME_FRE, #COL_GATE_NAME_LAT, 
	COL_GATE_TYPE,
	# areas
	COL_AREA_NAME_FRE, #COL_AREA_NAME_LAT,
	# walls
	COL_WALL_NAME_FRE, #COL_WALL_NAME_LAT, 
	COL_WALL_TYPE, 
	# landmarks
	COL_LDMRK_TYPE,
	# streets
	COL_STREET_TYPE,
	# social
	paste0("Own_", c(
		COL_PERS_ID, COL_PERS_NAME_FULL_NORM,
		COL_PERS_NAME_FIRST, COL_PERS_NAME_LAST, COL_PERS_NAME_NICK, COL_PERS_NAME_TYPE,
		COL_PERS_GENDER, COL_PERS_IDENTIFICATION, COL_PERS_RESIDENCE,
		COL_PERS_ECCL_NORM,
		COL_PERS_HEALTH_FRE,
		COL_PERS_CITY_FRE,
		COL_PERS_DIOC_FRE, 
		COL_PERS_OCC_CAT, COL_PERS_OCC_THEME,
		COL_PERS_STATUS_NORM
	))
)
# only those selected for the analysis
COL_CAT_SELECT <- c(
	# social
	COL_PERS_NAME_FIRST, COL_PERS_NAME_LAST, COL_PERS_NAME_NICK, COL_PERS_NAME_TYPE,
	COL_PERS_GENDER, COL_PERS_IDENTIFICATION, COL_PERS_RESIDENCE,
	COL_PERS_ECCL_NORM,
	COL_PERS_HEALTH_FRE,
	COL_PERS_CITY_FRE,
	COL_PERS_DIOC_FRE, 
	COL_PERS_OCC_CAT, COL_PERS_OCC_THEME,
	COL_PERS_STATUS_NORM,
				
	# estates
	COL_EST_AREA_ID, COL_EST_AREA_ID_RESTR,
	COL_EST_VILLAGE_ID, COL_EST_STREET_ID, COL_EST_LORDSHIP_ID,
	COL_LOC_TYPE, 
	COL_EST_DECLARED,
	COL_FEE_AMOUNT_CAT1,
	COL_EST_POSITION, COL_EST_POSITION_RESTR,
	# social
	paste0("Own_", c(
		COL_PERS_ID, COL_PERS_NAME_FULL_NORM,
		COL_PERS_NAME_FIRST, COL_PERS_NAME_LAST, COL_PERS_NAME_NICK, COL_PERS_NAME_TYPE,
		COL_PERS_GENDER, COL_PERS_IDENTIFICATION, COL_PERS_RESIDENCE,
		COL_PERS_ECCL_NORM,
		COL_PERS_HEALTH_FRE,
		COL_PERS_CITY_FRE,
		COL_PERS_DIOC_FRE, 
		COL_PERS_OCC_CAT, COL_PERS_OCC_THEME,
		COL_PERS_STATUS_NORM
	))
)

# list of tag-like attributes
COL_TAG <- list()
	# social
#	COL_TAG[[COL_PERS_TITLE_LAT1]] <- c(COL_PERS_TITLE_LAT1, COL_PERS_TITLE_LAT2)
#	COL_TAG[[COL_PERS_TITLE_FRE1]] <- c(COL_PERS_TITLE_FRE1, COL_PERS_TITLE_FRE2)
	COL_TAG[[COL_PERS_TITLE_NORM1]] <- c(COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2)
#	COL_TAG[[COL_PERS_OCC_LAT1]] <- c(COL_PERS_OCC_LAT1, COL_PERS_OCC_LAT2)
#	COL_TAG[[COL_PERS_OCC_FRE1]] <- c(COL_PERS_OCC_FRE1, COL_PERS_OCC_FRE2)
	COL_TAG[[COL_PERS_OCC_NORM1]] <- c(COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2)
	
	# estate
	COL_TAG[[COL_EST_COMP_LAB1]] <- c(COL_EST_COMP_LAB1, COL_EST_COMP_LAB2, COL_EST_COMP_LAB3, COL_EST_COMP_LAB4, COL_EST_COMP_LAB5, COL_EST_COMP_LAB6)
	COL_TAG[[COL_EST_COMP_NORM1]] <- c(COL_EST_COMP_NORM1, COL_EST_COMP_NORM2, COL_EST_COMP_NORM3, COL_EST_COMP_NORM4, COL_EST_COMP_NORM5, COL_EST_COMP_NORM6)
	COL_TAG[[COL_EST_COMP_TYPE1]] <- c(COL_EST_COMP_TYPE1, COL_EST_COMP_TYPE2, COL_EST_COMP_TYPE3, COL_EST_COMP_TYPE4, COL_EST_COMP_TYPE5, COL_EST_COMP_TYPE6)
	COL_TAG[[COL_EST_COMP_TYPE_S1]] <- c(COL_EST_COMP_TYPE_S1, COL_EST_COMP_TYPE_S2, COL_EST_COMP_TYPE_S3, COL_EST_COMP_TYPE_S4, COL_EST_COMP_TYPE_S5, COL_EST_COMP_TYPE_S6)
COL_TAG_SELECT <- list()
	# social
	COL_TAG_SELECT[[COL_PERS_TITLE_NORM1]] <- c(COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2)
	COL_TAG_SELECT[[COL_PERS_OCC_NORM1]] <- c(COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2)
	
	# estate
	COL_TAG_SELECT[[COL_EST_COMP_NORM1]] <- c(COL_EST_COMP_NORM1, COL_EST_COMP_NORM2, COL_EST_COMP_NORM3, COL_EST_COMP_NORM4, COL_EST_COMP_NORM5, COL_EST_COMP_NORM6)
	COL_TAG_SELECT[[COL_EST_COMP_TYPE1]] <- c(COL_EST_COMP_TYPE1, COL_EST_COMP_TYPE2, COL_EST_COMP_TYPE3, COL_EST_COMP_TYPE4, COL_EST_COMP_TYPE5, COL_EST_COMP_TYPE6)
	COL_TAG_SELECT[[COL_EST_COMP_TYPE_S1]] <- c(COL_EST_COMP_TYPE_S1, COL_EST_COMP_TYPE_S2, COL_EST_COMP_TYPE_S3, COL_EST_COMP_TYPE_S4, COL_EST_COMP_TYPE_S5, COL_EST_COMP_TYPE_S6)
	# social
	COL_TAG_SELECT[[paste0("Own_", COL_PERS_TITLE_NORM1)]] <- paste0("Own_", c(COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2))
	COL_TAG_SELECT[[paste0("Own_", COL_PERS_OCC_NORM1)]] <- paste0("Own_", c(COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2))
	
# list of numerical attributes
COL_NUM <- c(
	# social
	COL_PERS_MENTION,
	
	# locations
	COL_LOC_X, COL_LOC_Y,
	COL_LOC_INTER_X, COL_LOC_INTER_Y, 
	COL_LOC_ALGO_X, COL_LOC_ALGO_Y,
	# estates
	COL_EST_COMP_NBR1, COL_EST_COMP_NBR2, COL_FEE_AMOUNT_NORM1, COL_EST_MENTION,
	# edifices
	COL_EDIF_DATE_FRST_OCC, COL_EDIF_DATE_BUILD_START, COL_EDIF_DATE_BUILD_END, COL_EDIF_DATE_DESTR,
	# villages
	COL_VILG_SURF, COL_VILG_PERIM, COL_VILG_DATE_FRST_OCC, #COL_VILG_DATE_CREATED,
	# gates
	COL_GATE_DATE_FRST_OCC, COL_GATE_DATE_BUILD_START1, COL_GATE_DATE_BUILD_END1, COL_GATE_DATE_DESTR1, 
	COL_GATE_DATE_BUILD_START2, COL_GATE_DATE_BUILD_END2, COL_GATE_DATE_DESTR2,
	# areas
	COL_AREA_SURF, COL_AREA_PERIM,
	# walls
	COL_WALL_DATE_FRST_OCC, COL_WALL_DATE_BUILD_START1, COL_WALL_DATE_BUILD_END1, COL_WALL_DATE_DESTR1, 
	COL_WALL_DATE_BUILD_START2, COL_WALL_DATE_BUILD_END2, COL_WALL_DATE_DESTR2, 
	# landmarks
	COL_LDMRK_DATE_DERIV, COL_LDMRK_DATE_COUV, COL_LDMRK_DATE,
	# streets
	COL_STREET_LENGTH,
	# social
	paste0("Own_", c(
		COL_PERS_MENTION
	))
)
# only those selected for the analysis
COL_NUM_SELECT <- c(
	# social
	COL_PERS_MENTION,
		
	# estate
	COL_FEE_AMOUNT_NORM1, COL_EST_MENTION,
	COL_LOC_X, COL_LOC_Y,
	# social
	paste0("Own_", c(
		COL_PERS_MENTION
	))
)
