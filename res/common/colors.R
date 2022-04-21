#############################################################################################
# Defines functions and constants related to colors, for plots.
# 
# 11/2019 Vincent Labatut
#
# source("res/common/colors.R")
#############################################################################################




###############################################################################
# Colors used in the plots.
###############################################################################
CAT_COLORS_8 <- c(							# basic color brewer palette, see http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
	rgb(228,26,28,maxColorValue=255),		# red
	rgb(55,126,184,maxColorValue=255),		# blue
	rgb(77,175,74,maxColorValue=255),		# green
	rgb(152,78,163,maxColorValue=255),		# purple
	rgb(255,127,0,maxColorValue=255),		# orange
	rgb(166,86,40,maxColorValue=255),		# brown
	rgb(247,129,191,maxColorValue=255),		# pink
	rgb(255,255,51,maxColorValue=255)		# yellow
)

# variant with more colors
CAT_COLORS_12 <- c(							# manually extended color brewer palette
	rgb(228,26,28,maxColorValue=255),		# red
	rgb(55,126,184,maxColorValue=255),		# light blue
	rgb(113,219,110,maxColorValue=255),		# light green
	rgb(152,78,163,maxColorValue=255),		# purple
	rgb(255,127,0,maxColorValue=255),		# orange
	rgb(166,86,40,maxColorValue=255),		# brown
	rgb(247,129,191,maxColorValue=255),		# pink
	rgb(153,153,153,maxColorValue=255),		# light grey
	rgb(23,89,143,maxColorValue=255),		# dark blue
	rgb(16,125,12,maxColorValue=255),		# dark green
	rgb(30,30,30,maxColorValue=255),		# dark grey
	rgb(255,255,51,maxColorValue=255)		# yellow
)
CAT_COLORS_18 <- c(							# manual extension of the color brewer palette
	rgb(228,26,28,maxColorValue=255),		# red
	rgb(55,126,184,maxColorValue=255),		# light blue
	rgb(113,219,110,maxColorValue=255),		# light green
	rgb(152,78,163,maxColorValue=255),		# purple
	rgb(255,127,0,maxColorValue=255),		# orange
	rgb(166,86,40,maxColorValue=255),		# brown
	rgb(247,129,191,maxColorValue=255),		# pink
	rgb(153,153,153,maxColorValue=255),		# light grey
	rgb(23,89,143,maxColorValue=255),		# dark blue
	rgb(16,125,12,maxColorValue=255),		# dark green
	rgb(30,30,30,maxColorValue=255),		# dark grey
	rgb(255,255,51,maxColorValue=255),		# yellow
	rgb(143,11,13,maxColorValue=255),		# dark red
	rgb(0,255,255,maxColorValue=255),		# cyan
	rgb(14,161,161,maxColorValue=255),		# dark cyan
	rgb(255,187,120,maxColorValue=255),		# light orange
	rgb(0,0,255,maxColorValue=255),			# straight blue
	rgb(0,255,0,maxColorValue=255)			# straight green
)
CAT_COLORS_22 <- c(	# kelly.colors(22) from package Polychrome
	"#B0B0B0", 		# lightgray 
	"#222222", 		# black 
	"#f3c300", 		# yellow 
	"#875692", 		# purple 
	"#f38400", 		# orange 
	"#a1caf1", 		# lightblue 
	"#be0032", 		# red 
	"#c2b280", 		# buff 
	"#808080", 		# gray 
	"#008856", 		# green 
	"#e68fac", 		# purplishpink 
	"#0067a5", 		# blue 
	"#f99379", 		# yellowishpink 
	"#604e97", 		# violet 
	"#f6a600", 		# orangeyellow 
	"#b3446c", 		# purplishred 
	"#dcd300", 		# greenishyellow 
	"#882d17", 		# reddishbrown 
	"#8db600", 		# yellowgreen 
	"#654522", 		# yellowishbrown 
	"#e25822", 		# reddishorange 
	"#2b3d26"		# olivegreen
)
CAT_COLORS_26 <- c(	# green.armytage.colors(26) from package Polychrome
	"#F0A3FF",		# amethyst 
	"#0075DC",		# blue
	"#993F00",		# caramel
	"#4C005C",		# damson
	"#191919",		# ebony
	"#005C31",		# forest
	"#2BCE48",		# green
	"#FFCC99", 		# honeydew
	"#808080",		# iron
	"#94FFB5",		# jade
	"#8F7C00",		# khaki
	"#9DCC00",		# lime
	"#C20088",		# mallow
	"#003380",		# navy
	"#19A405",		# orpiment
	"#FFA8BB", 		# pink
	"#426600",		# quagmire 
	"#FF0010",		# red
	"#5EF1F2",		# sky
	"#00998F",		# turquoise
	"#E0FF66",		# uranium
	"#100AFF",		# violet
	"#990000",		# wine
	"#FFFF80", 		# xanthin
	"#FFE100",		# yellow
	"#FF5000"		# zinnia
)
CAT_COLORS_32 <- c(	# glasbey.colors(32) from package Polychrome
	"#FFFFFF", "#0000FF", "#FF0000", "#00FF00", "#000033", "#FF00B6", "#005300", "#FFD300", 
	"#009FFF", "#9A4D42", "#00FFBE", "#783FC1", "#1F9698", "#FFACFD", "#B1CC71", "#F1085C", 
	"#FE8F42", "#DD00FF", "#201A01", "#720055", "#766C95", "#02AD24", "#C8FF00", "#886C00", 
	"#FFB79F", "#858567", "#A10300", "#14F9FF", "#00479E", "#DC5E93", "#93D4FF", "#004CFF"
)

COLS_ATT <- list()
# colors for normalized component
COLS_ATT[[COL_EST_COMP_NORM1]] <- c(
	"anniversaire"="#FFFFD4",
	"arc-gallerie"="#FFF7EC",
	"arriere-cour"="#FD8D3C",
	"atelier-auvent"="#BF8100",
	"bistour"="#BDBDBD",
	"boucherie"="#DCAA61",
	"bourg"="#FA9FB5",
	"boutique"="#C59956",
	"cave-cellier"="#8C510A",
	"cimetiere"="#252525",
	"cloitre"="#FFFF56",
	"conduit eau"="#41B6C4",
	"edifice"="#FED02B",
	"emplacement bat"="#BD0026",
	"forge"="#642D04",
	"four"="#5AB4AC",
	"grange-grenier"="#944800",
	"hopital"="#FFFF56",
	"jardin"="#74C476",
	"loge"="#D8B365",
	"magasin"="#C09E70",
	"maison"="#FED02B",
	"masure-salle-piece"="#FED200",
	"part de fosse"="#525252",
	"part de lices"="#969696",
	"part de moulin"="#9BC2E6",
	"part de rempart"="#737373",
	"part de sorgue"="#0868AC",
	"parvis eglise"="#FFFFE9",
	"place-angle-plan"="#FEE8C8",
	"place-cour"="#FD8D3C",
	"portail"="#D9D9D9",
	"puit"="#5AB4AC",
	"table"="#BF812D",
	"taverne"="#642D04",
	"terre-pre"="#005A32",
	"tinel"="#FFD966",
	"toit"="#FFEB88",
	"tour"="#FFB166",
	"verger"="#41AB5D",
	"vigne"="#238B45",
	"voie-traverse-passage-entree"="#FFF7EC"
)
# colors for component type attribute
COLS_ATT[[COL_EST_COMP_TYPE1]] <- c(
"ancien systeme defensif"="#636363",
	"bourg"="#FA9FB5",
	"cimetiere"="#252525",
	"communication voie d acces"="#FFF7EC",
	"cour"="#FD8D3C",
	"edifice commercial atelier professionnel"="#D8B365",
	"edifice de stockage"="#8C510A",
	"edifice religieux"="#FFFF56",
	"edifice residentiel"="#FED02B",
	"four puit"="#5AB4AC",
	"infrastructure hydraulique"="#0868AC",
	"messe"="#FFFFD4",
	"moulin"="#9BC2E6",
	"terrain agricol"="#31A800",
	"terrain a batir"="#BD0026"
)
# colors for simple component type attribute
COLS_ATT[[COL_EST_COMP_TYPE_S1]] <- c(
"hydrau"="#0868AC",
	"moulin"="#9BC2E6",
	"non bati"="#BD0026",
	"bati"="#FED02B",
	"immateriel"="#FFFFD4"
)




#############################################################
# Returns the appropriate number of colors
# 
# values: number of distinct values to plot.
#
# returns: an appropriate palette, for categorical values.
#############################################################
get.palette <- function(values)
{	if(values<=8)
		result <- CAT_COLORS_8
	else if(values<=12)
		result <- CAT_COLORS_12
	else if(values<=18)
		result <- CAT_COLORS_18
	else if(values<=22)
		result <- CAT_COLORS_22
	else #if(values<=26)
		result <- CAT_COLORS_26
#	else
#		result <- CAT_COLORS_32
	
	return(result)
}




#############################################################
# Receives a solid color and makes it partially transparent by
# adding an alpha channel.
#
# color: original color.
# transparency: alpha level (percent).
#				100 means completely transparent.
#
# returns: partially transparent color.
#############################################################
make.color.transparent <- function(color, transparency=50)
{	# convert to RGB triplet
	rgb.val <- col2rgb(color)
	
	# create new color using specified transparency
	res <- rgb(
			rgb.val[1], rgb.val[2], rgb.val[3],
			max=255,
			alpha=(100-transparency)*255 / 100
	)
	
	return(res)
}
