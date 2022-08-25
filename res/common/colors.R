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
COLOR_NA <- "#F0F0F0"								# almost white
COLOR_INF <- "#575757"								# dark grey
COLOR_OTHERS <- rgb(153,153,153,maxColorValue=255)	# light grey


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
#	rgb(153,153,153,maxColorValue=255),		# light grey
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
#	rgb(153,153,153,maxColorValue=255),		# light grey
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
#	"#B0B0B0", 		# lightgray 
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
#	"#FFFFFF", 
	"#0000FF", 
	"#FF0000", 
	"#00FF00", 
	"#000033", 
	"#FF00B6", 
	"#005300", 
	"#FFD300", 
	"#009FFF", 
	"#9A4D42", 
	"#00FFBE", 
	"#783FC1", 
	"#1F9698", 
	"#FFACFD", 
	"#B1CC71", 
	"#F1085C", 
	"#FE8F42", 
	"#DD00FF", 
	"#201A01", 
	"#720055", 
	"#766C95", 
	"#02AD24", 
	"#C8FF00", 
	"#886C00", 
	"#FFB79F", 
	"#858567", 
	"#A10300", 
	"#14F9FF", 
	"#00479E", 
	"#DC5E93", 
	"#93D4FF", 
	"#004CFF"
)
CAT_COLORS_26_PLUS <- c(	# green.armytage.colors(26) from package Polychrome + additional colors from Margot
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
	"#FF5000",		# zinnia
	"#9D54A8",		# violet
	"#79AAB5",		# cyan-grey
	"#A77E66",		# light brown
	"#723B7f",		# depp purple
	"#D87C48",		# orange
	"#B7D3AB"		# pale green
			
)

########
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
	"mur"="#0D0D0D",
	"part de fosse"="#525252",
	"part de lices"="#969696",
	"part de moulin"="#9BC2E6",
	"part de rempart"="#737373",
	"part de sorgue"="#0868AC",
	"parvis eglise"="#FFFFE9",
	"place-angle-plan"="#FEE8C8",
	"place-cour"="#FD8D3C",
	"pont"="#FEC0C8",
	"portail"="#D9D9D9",
	"porte rempart"="#942193",
	"puit"="#5AB4AC",
	"table"="#BF812D",
	"taverne"="#642D04",
	"terre-pre"="#005A32",
	"tinel"="#FFD966",
	"toit"="#FFEB88",
	"tour"="#FFB166",
	"verger"="#41AB5D",
	"vigne"="#238B45",
	"voie-traverse-passage-entree"="#DAADCC"
)
# colors for component type attribute
COLS_ATT[[COL_EST_COMP_TYPE1]] <- c(
	"ancien systeme defensif"="#636363",
	"bourg"="#FA9FB5",
	"cimetiere"="#252525",
	"communication voie d acces"="#FFEDD4",
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
# color for sex
COLS_ATT[[COL_PERS_GENDER]] <- c(
	"homme"="#19A0AA",		# turquoise
	"femme"="#F15F36"		# salmon
)
COLS_ATT[[paste0("Own_",COL_PERS_GENDER)]] <- COLS_ATT[[COL_PERS_GENDER]]
# colors for fee categories
cols <- viridis(n=length(FEE_CATS))
names(cols) <- FEE_CATS
COLS_ATT[[COL_FEE_AMOUNT_CAT1]] <- cols
# colors for role categories
MEAS_ROLE_CAT <- "role-cat"
COLS_ATT[[MEAS_ROLE_CAT]] <- c(
	"Ultra-peripheral Non-hubs"="lightgrey", 
	"Peripheral Non-hubs"="red", 
	"Connnector Non-hubs"="green", 
	"Kinless Non-hubs"="blue", 
	"Provincial Hubs"="yellow", 
	"Connector Hubs"="magenta", 
	"Kinless Hub"="#444444"
)




#############################################################
# Returns the appropriate number of colors
# 
# val.nbr: number of distinct values to plot.
#
# returns: an appropriate palette, for categorical values.
#############################################################
get.palette <- function(val.nbr)
{	# pick a predefined palette
	if(val.nbr<=length(CAT_COLORS_8))
		result <- CAT_COLORS_8
	else if(val.nbr<=length(CAT_COLORS_12))
		result <- CAT_COLORS_12
	else if(val.nbr<=length(CAT_COLORS_18))
		result <- CAT_COLORS_18
	else if(val.nbr<=length(CAT_COLORS_22))
		result <- CAT_COLORS_22
	else if(val.nbr<=length(CAT_COLORS_26))
		result <- CAT_COLORS_26
#	else
#		result <- CAT_COLORS_32
	else
		result <- CAT_COLORS_26_PLUS
	
	# adjust length
	if(length(result)>val.nbr)
		result <- result[1:val.nbr]
	else if(length(result)<val.nbr)
		result <- rep(result, length.out=val.nbr)
	
	return(result)
}




#############################################################
# Returns the appropriate colors for the specified categorical values.
# In particular, one color is specifically reserved for NA, and
# another one for "Other" (if needed).
# 
# values: the series of categorical values to consider.
# attr: name of the concerned attribute.
#
# returns: a list containing the palette, the strings associated to
#          each color in the palette, and the colors associated to
#          each value in the specified input vector.
#############################################################
retrieve.palette.cat <- function(values, attr=NA)
{	# get value names
	vals <- values[!is.na(values) & values!=VAL_OTHER]
	if(is.logical(vals) || any(is.na(suppressWarnings(as.integer(vals)))))
		lv <- as.character(levels(factor(vals)))
	else
		lv <- as.character(levels(factor(as.integer(vals))))
	factors <- as.character(factor(values))
	
	# try to use predefined palette
	if(is.na(attr))
		pal.cols <- NULL
	else
		pal.cols <- COLS_ATT[[attr]]
	
	# if no predefined palette, build one
	if(is.null(pal.cols))
	{	pal.cols <- get.palette(val.nbr=length(lv))
		pal.txts <- lv
	}
	else
		pal.txts <- names(pal.cols)
	
	# possibly handle NA values
	if(any(is.na(values)))
	{	pal.cols <- c(pal.cols, COLOR_NA)
		pal.txts <- c(pal.txts, "NA")
	}
	
	# possibly handle "Other" values
	if(any(values[!is.na(values)]==VAL_OTHER))
	{	# update palette
		pal.cols <- c(pal.cols, COLOR_OTHERS)
		pal.txts <- c(pal.txts, VAL_OTHER)
		
		# possibly remove empty values
		vals <- values; vals[is.na(vals)] <- "NA"
		vals <- factor(vals, levels=pal.txts)
		tt <- table(vals)
		idx <- which(tt==0)
		if(length(idx)>0)
		{	pal.cols <- pal.cols[-idx]
			pal.txts <- pal.txts[-idx]
		}
	}
	
	# add names to colors
	names(pal.cols) <- pal.txts
	
	# set output colors
	val.cols <- rep(NA, length(values))
	idx <- which(!is.na(values))
	if(length(idx)>0)
		val.cols[idx] <- pal.cols[factors[idx]]
	idx <- which(is.na(values))
	if(length(idx)>0)
		val.cols[idx] <- rep(pal.cols["NA"], length(idx))
	
	res <- list(pal.cols=pal.cols, pal.txts=pal.txts, val.cols=val.cols)
	return(res)
}




#############################################################
# Returns the appropriate colors for the specified tag values.
# In particular, one color is specifically reserved for NA.
# 
# values: a matrix containing the tag values to consider.
# attr: name of the concerned attribute.
#
# returns: a list containing the palette, the strings associated to
#          each color in the palette, and the possibly reordered
#          data matrix.
#############################################################
retrieve.palette.tag <- function(values, attr=NA)
{	# possibly remove "NA" column (re-inserted later)
	nas <- NA
	col <- which(colnames(values)=="NA" | is.na(colnames(values)))
	if(length(col)>0)
	{	nas <- values[,col]
		values <- values[,-col,drop=FALSE]
	}
	
	# possibly remove "Other" column (re-inserted later)
	others <- NA
	if(VAL_OTHER %in% colnames(values))
	{	others <- values[,VAL_OTHER]
		values <- values[,-which(colnames(values)==VAL_OTHER),drop=FALSE]
	}
	
	# get value names
	pal.txts <- colnames(values)
	
	# if no attribute: must set palette later
	if(is.na(attr))
		pal.cols <- NULL
	# otherwise, check whether attribute is numerical
	else if(attr %in% COL_NUM)
		pal.cols <- viridis(ncol(values),direction=-1)
	# otherwise, use palette predefined for the categorical attribute
	else
		pal.cols <- COLS_ATT[[attr]]
	
	# if no palette as of now, build one
	if(is.null(pal.cols))
		pal.cols <- get.palette(val.nbr=length(pal.txts))
	# otherwise, if categorical attribute: check values 
	else if(!(attr %in% COL_NUM))
	{	# if additional values in the data: problem
		superfluous <- setdiff(pal.txts,names(pal.cols))
		if(length(superfluous)>0)
			stop("Superfluous values in the matrix: ",paste0(superfluous,collapse=", "))
		# if values missing from the data: add empty columns (provided there is no "Other" value)
		missing <- setdiff(names(pal.cols), pal.txts)
		if(length(missing)>0 && all(is.na(others)))
		{	tmp <- matrix(0, nrow=nrow(values), ncol=length(missing))
			colnames(tmp) <- missing
			values <- cbind(values, tmp)
		}
		# sort the data matrix columns to match the predefined palette
		pal.txts <- intersect(names(pal.cols), pal.txts)
		pal.cols <- pal.cols[pal.txts]
		values <- values[,pal.txts,drop=FALSE]
	}
	
	# possibly handle NA values
	if(any(apply(values,1,function(r) all(r==0))) || !all(is.na(nas)))
	{	pal.cols <- c(pal.cols, COLOR_NA)
		pal.txts <- c(pal.txts, "NA")
		if(!all(is.na(nas)))
		{	values <- cbind(values, nas)
			colnames(values)[ncol(values)] <- "NA"
		}
	}
	
	# possibly handle "Other" values
	if(!all(is.na(others)))
	{	pal.cols <- c(pal.cols, COLOR_OTHERS)
		pal.txts <- c(pal.txts, VAL_OTHER)
		values <- cbind(values, others)
	}
	
	# add names to colors
	names(pal.cols) <- pal.txts
	
	res <- list(pal.cols=pal.cols, pal.txts=pal.txts, values=values)
	return(res)
}




#############################################################
# Returns the appropriate colors for the specified numerical values.
# In particular, one color is specifically reserved for NA, and another
# for infinite values.
# This function uses code from:
# 	https://stackoverflow.com/questions/27004167/coloring-vertexes-according-to-their-centrality
# 
# values: the series of numerical values to consider.
#
# returns: a list containing the palette,  and the colors associated to
#          each value in the specified input vector.
#############################################################
retrieve.palette.num <- function(values)
{	# identify finite values
	infinite <- is.infinite(values) & !is.na(values)
	finite <- !is.infinite(values) & !is.na(values) 
	nas <- is.na(values)
	
	# init color vector
	val.cols <- rep(COLOR_NA, length(values))
	val.cols[infinite] <- rep(COLOR_INF, length(which(infinite)))
	
	# only one value
	#if(length(unique(values))==1)			# does not work when values are too close
	if(isTRUE(all.equal(values[finite],		# more efficient way to compare close values
					rep(values[finite][1],length(values[finite])))))
	{	val.cols[finite] <- "YELLOW"
		pal.grad <- "YELLOW"
		pal.cols <- pal.grad
		pal.vals <- unique(values[finite])[1]
	}
	# several distinct values
	else
	{	fine <- 500 						# granularity of the color gradient
		#pal <- colorRampPalette(c("YELLOW","RED"))
		#val.cols[finite] <- pal(fine)[as.numeric(cut(values[finite],breaks=fine))]
		#leg.pal <- pal(25)
		val.cols[finite] <- viridis(fine,direction=-1)[as.numeric(cut(values[finite],breaks=fine))]
		pal.grad <- viridis(25,direction=-1)
		pal.vals <- sort(unique(values[finite]))
		pal.cols <- val.cols[match(pal.vals,values)]
	}
	names(pal.cols) <- as.character(pal.vals)
	
	# handle NA values
	if(any(nas))
	{	pal.vals <- c(pal.vals,NA)
		pal.cols <- c(pal.cols,COLOR_NA)
		names(pal.cols)[length(pal.cols)] <- "NA"
	}
	
	res <- list(pal.cols=pal.cols, pal.vals=pal.vals, val.cols=val.cols, pal.grad=pal.grad)
	return(res)
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
