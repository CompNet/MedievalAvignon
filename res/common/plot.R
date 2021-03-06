#############################################################################################
# Functions used to plot graphs and figures.
# 
# 09/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/common/plot.R")
#############################################################################################




#############################################################
# constants
FORMAT <- c("png")#,"pdf")	# plot file format: pdf png




#############################################################
# Displays the specified graph in an appropraite way, taking
# into account the previously set link and node attributes.
#
# g: graph to plot.
# paths: (optional) paths to highlight while plotting. This parameter
# 		 is either a list of integer vectors (node sequences), or
# 		 an integer vector if there is only one path to plot.
# col.att: (optional) name of a vertex attribute, used to determine node color.
#          It is also possible to pass several comparable attributes, in which 
#		   case the plot will use piecharts to represent nodes.
# cat.att: (optional) if there is a vertex attribute, indicates whether
#		   it is categorical or not.
# size.att: (optional) name of a vertex attribute used to determine
#		    node size. It has to be numerical, cannot be categorical. 
# v.hl: vertices to highlight (these are represented as squares).
# e.hl: edges to highlight (these are represented as thick lines).
# color.isolates: force isolates to be colored (by default they are not)
# col.att.cap: caption of the node colors, to be used for certain pie-charts. 
# file: (optional) file name, to record the plot.
# ...: parameters sent directly to the plot function.
#############################################################
custom.gplot <- function(g, paths, col.att, col.att.cap, size.att, cat.att=FALSE, v.hl, e.hl, color.isolates=FALSE, file, ...)
{	pie.values <- NA
	lgd.col <- NA
	
	# vertex shapes
	vshapes <- rep("circle",gorder(g))
	if(hasArg(v.hl))
		vshapes[v.hl] <- "csquare"
	# vertex outline color
	outline.cols <- rep("BLACK",gorder(g))
	
	# set edge colors
	ecols <- rep("BLACK", gsize(g))						# default color
	nature <- edge_attr(g, LK_TYPE)
	if(length(nature)>0)
	{	if(graph_attr(g, GR_TYPE)==GR_TYPE_SOC)
		{	ecols[nature==LK_TYPE_PRO] <- "#1A8F39"			# green
			ecols[nature==LK_TYPE_FAM] <- "#9C1699"			# purple
#			ecols[nature==LK_TYPE_XXX] <- "#C27604"			# orange
#			ecols[nature==LK_TYPE_UNK] <- "#222222"			# dark grey
		}
		else if(graph_attr(g, GR_TYPE)==GR_TYPE_EST)
		{	nats <- sort(unique(nature))
			epal <- get.palette(length(nats))
			for(i in 1:length(nats))
				ecols[nature==nats[i]] <- epal[i]
		}
	}
	# set edge width
	if(is.null(E(g)$weight))							# if no weight:
		E(g)$weight <- rep(1,gsize(g))					# same edge width
	ewidth <- E(g)$weight
	# set edge line type
	elty <- rep(1,gsize(g))								# solid
	# set edge transparency
#	idx <- as.integer(E(g)[from(1)])	# edges attached to Trajan
#	if(length(idx)>0)
#	{	ecols[idx] <- sapply(ecols[idx],function(color) 
#		{	vals <- col2rgb(color)
#			rgb(vals[1],vals[2],vals[3],alpha=50,maxColorValue=255)
#		})
#	}
	
	# possibly change the color of the highlighted path
	if(hasArg(paths))
	{	if(!is.list(paths))
			paths <- list(paths)
		for(path in paths)
		{	v <- NA
			for(n in path)
			{	if(is.na(v))
				{	v <- n
					outline.cols[v] <- "RED"
					vshapes[v] <- "csquare"
				}
				else
				{	u <- v
					v <- n
					outline.cols[v] <- "RED"
					idx <- as.integer(E(g)[u %--% v])
					ecols[idx] <- "RED"
					ewidth[idx] <- 2*E(g)$weight[idx]
				}
			}
			outline.cols[v] <- "RED"
			vshapes[v] <- "csquare"
		}
	}
	
	# possibly highlight certain links
	if(hasArg(e.hl))
	{	if(length(e.hl)>0)
			ewidth[e.hl] <- E(g)$weight[e.hl]*3
	}
	
	# vertex color
	if(hasArg(col.att))
	{	# isolates have no color
		vcols <- rep("WHITE",gorder(g))
		if(color.isolates)
			connected <- rep(TRUE, gorder(g))
		else
			connected <- igraph::degree(g)>0
		
		# get the attribute values
		vvals <- get.vertex.attribute(graph=g, name=col.att[1])
		if(!all(!connected))
		{	leg.cap <- LONG_NAME[col.att[1]]
			
			# just one attribute
			if(length(col.att)==1)
			{	# check if all the attribute values are not NA
				if(!all(is.na(vvals)))
				{	# categorical attribute
					if(cat.att)
					{	tmp <- factor(vvals[connected])
						lgd.txt <- levels(tmp)
						colcols <- get.palette(length(lgd.txt))
						vcols[connected] <- colcols[(as.integer(tmp)-1) %% length(colcols) + 1]
						lgd.col <- colcols[(1:length(lgd.txt)-1) %% length(colcols) + 1]
					}
					# numerical attribute
					else
					{	if(any(!is.na(vvals)))
						{	fine = 500 									# granularity of the color gradient
							pal = colorRampPalette(c("yellow",'red'))	# extreme colors of the gradient
							finite <- !is.infinite(vvals)
							vcols[connected & finite] <- 
								pal(fine)[as.numeric(cut(vvals[connected & finite],breaks=fine))]
							vcols[connected & !finite] <- "#575757"		# infinite values are grey
							# see https://stackoverflow.com/questions/27004167/coloring-vertexes-according-to-their-centrality
						}
					}
				}
			}
			# several attributes
			else
			{	cat.att <- TRUE
				if(hasArg(col.att.cap))
					leg.cap <- col.att.cap
				# get attribute values as a matrix
				m <- sapply(col.att, function(att) vertex_attr(g, att))
				
				# several boolean attributes, to combine (like multiple tags)
				if(!is.numeric(vvals))
				{	are.nas <- apply(m,1,function(r) all(is.na(r)))					# detect individuals with only NAs
					are.pie <- apply(m,1,function(r) length(r[!is.na(r)])>1)		# detect individuals with several non-NA values
					uvals <- sort(unique(c(m)))										# get unique attribute values
					pie.matrix <- NA
					for(uval in uvals)												# build a column for each of them
					{	vals <- as.integer(apply(m, 1, function(v) uval %in% v[!is.na(v)]))
						if(all(is.na(pie.matrix)))
							pie.matrix <- as.matrix(vals, ncol=1)
						else
							pie.matrix <- cbind(pie.matrix, vals)
						colnames(pie.matrix)[ncol(pie.matrix)] <- uval
					}
					lgd.txt <- colnames(pie.matrix)
					colcols <- get.palette(length(lgd.txt))
					lgd.col <- colcols[(1:length(lgd.txt)-1) %% length(colcols) + 1]
					pie.values <- unlist(apply(pie.matrix, 1, function(v) list(v)), recursive=FALSE)
					pie.values[!are.pie | !connected] <- NA
					vshapes[are.pie & connected] <- rep("pie",length(which(are.pie & connected)))
					vcols[are.pie & connected] <- NA
					vcols[!are.nas & !are.pie & connected] <- apply(pie.matrix[!are.nas & !are.pie & connected,,drop=FALSE], 1, 
							function(v) lgd.col[which(v>0)])
				}
				# counts of categorical attributes inside a community
				else
				{	are.pie <- apply(m,1,function(r) length(which(r>0))>1)				# detect individuals with several non-zero values
					lgd.txt <- col.att
					colcols <- get.palette(length(lgd.txt))
					lgd.col <- colcols[(1:length(lgd.txt)-1) %% length(colcols) + 1]
					lgd.col[which(is.na(lgd.txt) | lgd.txt=="NA")] <- "#F0F0F0"			# force NA to white
					pie.values <- split(m,1:nrow(m))
					pie.values[!are.pie | !connected] <- NA
					vshapes[are.pie & connected] <- rep("pie",length(which(are.pie & connected)))
					vcols[are.pie & connected] <- NA
					vcols[!are.pie & connected] <- apply(m[!are.pie & connected,,drop=FALSE], 1, 
							function(v) lgd.col[which(v>0)])
				}
			}
		}
	}
	else
		vcols <- rep("GREY",gorder(g))
	
	# vertex size
	if(hasArg(size.att))
	{	# one size fits all
		if(length(size.att)==1 && is.numeric(size.att))
			vsizes <- size.att
		
		# attribute name
		else
		{	# get the attribute values
			vvals <- get.vertex.attribute(graph=g, name=size.att)
			
			# init limit sizes
			vsizes <- rep(NA, gorder(g))
			min.size <- if(min(vvals,na.rm=TRUE)==0) 0 else 2
			max.size <- 20
			cut.nbr <- 4
			
			# define cuts
			must.round <- all(vvals%%1==0)	# check if values are integers
			step <- (max(vvals,na.rm=TRUE)-min(vvals,na.rm=TRUE))/cut.nbr
			if(must.round)
				step <- ceiling(step)
			cuts <- seq(from=step, to=cut.nbr*step, by=step)
			
			# NA, NaN, Inf are set to a zero size
			nosize <- is.infinite(vvals) | is.nan(vvals) | is.na(vvals)
			vsizes[nosize] <- 0
			
			# TODO how to deal with isolates?
			
			# regular values
			tmp <- vvals[!nosize]
			vsizes[!nosize] <- (tmp-min(tmp))/(max(tmp)-min(tmp))*(max.size-min.size)+min.size
			cuts.scale <- (cuts-min(tmp))/(max(tmp)-min(tmp))*(max.size-min.size)+min.size
		}
	}
	else
		vsizes <- 3
	
	# main plot
	for(fformat in FORMAT)
	{	if(hasArg(file))
		{	if(fformat=="pdf")
			{	if(graph_attr(g, GR_TYPE)==GR_TYPE_SOC)
					pdf(paste0(file,".pdf"), width=25, height=25)
				else
					pdf(paste0(file,".pdf"), width=50, height=25)
			}
			else if(fformat=="png")
			{	if(graph_attr(g, GR_TYPE)==GR_TYPE_SOC)
					png(paste0(file,".png"), width=1024, height=1024)
				else
					png(paste0(file,".png"), width=2048, height=1024)
			}
		}
		if(graph_attr(g, GR_TYPE)==GR_TYPE_SOC)
		{	par(mar=c(5,4,4,2)+.1)						# 5, 4, 4, 2 (B L T R)
			arrow.param <- 0.75
		}
		else
		{	par(mar=c(0,7,0,7)+.1)						# 5, 4, 4, 2 (B L T R)
			arrow.param <- 0.5
		}
		plot(g,										# graph to plot
			#axes=TRUE,								# whether to draw axes or not
			layout=cbind(V(g)$x,V(g)$y),			# predefined layout
			vertex.size=vsizes,						# node size
			vertex.color=vcols,						# node color
			vertex.pie=pie.values,					# node pie proportions
			vertex.pie.color=list(lgd.col),			# node pie colors
			vertex.shape=vshapes,					# node shape
			vertex.frame.color=outline.cols,		# node border color
			#vertex.label=V(g)$label0,				# node short labels
			vertex.label=V(g)$label,				# node long labels
			vertex.label.cex=1.2,					# label size
			vertex.label.family="sans",				# font type
			vertex.label.font=1,					# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
			vertex.label.label.dist=0,				# label distance to node center (0=center)
			vertex.label.color="BLACK",				# label color
			edge.color=ecols,						# link color
			edge.lty=elty,							# link type
			edge.width=ewidth,						# link thickness
			edge.arrow.size=arrow.param,			# arrow size
			edge.arrow.width=arrow.param,			# arrow size
			frame=FALSE, 							# frame around the plot (useful when debugging)
			...										# other parameters
		)
		if(length(nature)>0)
		{	if(graph_attr(g, GR_TYPE)==GR_TYPE_SOC)
			{	legend(
					title="Link type",								# title of the legend box
					x="topright",									# position
					legend=c(LK_TYPE_FAM, LK_TYPE_PRO),				# text of the legend
					col=c("#9C1699","#1A8F39"),						# color of the lines
					lty=1,											# type of lines
					lwd=4,											# line thickness
					bty="n",										# no box around the legend
					cex=0.8
				)
			}
			else
			{	legend(
					title="Link type",								# title of the legend box
					x="topright",									# position
					legend=nats,									# text of the legend
					col=epal,										# color of the lines
					lty=1,											# type of lines
					lwd=4,											# line thickness
					bty="n",										# no box around the legend
					cex=0.8
				)
			}
		}
		if(hasArg(col.att))
		{	if(!all(!connected) && !all(is.na(vvals)))
			{	# categorical attributes
				if(cat.att)
				{	legend(
						title=leg.cap,							# title of the legend box
						x="bottomleft",							# position
						legend=lgd.txt,							# text of the legend
						fill=lgd.col,							# color of the nodes
						bty="n",								# no box around the legend
						cex=0.8									# size of the text in the legend
					)
				}
				# numerical attributes
				else
				{	width <- 0.05
					height <- 0.3
					x1 <- -1.25
					x2 <- x1 + width
					y2 <- -1
					y1 <- y2 + height
					leg.loc <- cbind(x=c(x1, x2, x2, x1), y=c(y1, y1, y2, y2))
					legend.gradient(
						pnts=leg.loc,
						cols=pal(25),
						#limits=format(range(vvals[connected],na.rm=TRUE), digits=2, nsmall=2),	# pb: uses scientific notation when numbers too small
						limits=sprintf("%.2f", range(vvals[connected & finite],na.rm=TRUE)),
						title=leg.cap, 
						cex=0.8
					)
				}
			}
		}
		# legend for vertex sizes, if required: 
		# https://stackoverflow.com/questions/38451431/add-legend-in-igraph-to-annotate-difference-vertices-size
		if(hasArg(size.att))
		{	legend.bubble(
				x="topleft",					# position of the legend
				title=LONG_NAME[size.att],		# title of the legend box
				z=max(cuts),					# largest size
				maxradius=max(cuts.scale/200),	# scaled radius of the largest bubble
				n=cut.nbr,						# number of bubbles
				round=if(must.round) 0 else 2,	# number of decimal places
				bty="n",						# box (o=default, n=none)
				mab=1.2,						# margin between largest bubble and box
				bg=NULL, 						# background color of the box
				inset=0, 						# inset distance from margin
				pch=21, 						# symbol used to plot
				pt.bg=NULL, 					# symbol background color
				txt.cex=0.5, 					# text size
				txt.col=NULL, 					# text color
				font = NULL						# text font
			)
		}
		if(hasArg(file))
			dev.off()
	}
}




#############################################################
# Custom histogram.
#
# vals: raw values.
# name: name of the values (used for the x-axis label).
# file: (optional) file name, to record the histogram plot.
#############################################################
custom.hist <- function(vals, name, file)
{	vals <- vals[!is.na(vals)]
	if(length(vals)>0)
	{	for(fformat in FORMAT)
		{	if(hasArg(file))
			{	if(fformat=="pdf")
					pdf(paste0(file,".pdf"), width=25, height=25)
				else if(fformat=="png")
					png(paste0(file,".png"), width=1024, height=1024)
			}
#			par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
			par(mar=c(5.1, 4.1, 4.1, 2.1))
			hist(
					vals,			# data
					col="#ffd6d6",	# bar color
					main=NA,		# no main title
					prob=TRUE,		# frenquency density
					breaks=20,		# number of bars
					xlab=name,		# x-axis label
					ylab="Densite"	# y-axis label
			)
			lines(
					density(vals), 	# density estimate
					lwd=2, 			# line thickness
					col="RED"		# line color
			)
			stripchart(
					vals, 			# data
					at=0.02, 		# central position of points (y)
					pch=21, 		# point shape
					col="BLACK", 	# point color
					method="jitter",# noise to avoid overlaps
					jitter=0.02, 	# noise magnitude
					add=TRUE		# add to current plot
			)
			if(hasArg(file))
				dev.off()
		}
	}
}




#############################################################
# Custom barplot.
#
# vals: raw values.
# text: name of the bars.
# xlab: label of the x-axis.
# ylab: label of the y-axis.
# file: (optional) file name, to record the histogram plot.
# ...: additional parameters, fetched to the barplot function.
#############################################################
custom.barplot <- function(vals, text, xlab, ylab, file, ...)
{	idx <- which(is.na(text))
	if(length(idx)>0)
		text[idx] <- VAL_UNKNOWN
	wide <- length(text) > 8
	for(fformat in FORMAT)
	{	if(hasArg(file))
		{	if(fformat=="pdf")
				pdf(paste0(file,".pdf"), width=25, height=25)
			else if(fformat=="png")
				png(paste0(file,".png"), width=1024, height=1024)
		}
#		par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
		if(wide)
			par(mar=c(9, 4, 1, 0)+0.1)
		else
			par(mar=c(5, 4, 1, 0)+0.1)
		if(length(dim(vals))<=1)
		{	barplot(
				height=vals,				# data
				names.arg=text,				# bar names
				col="#ffd6d6",				# bar color
				main=NA,					# no main title
				xlab=if(wide) NA else xlab,	# x-axis label
				ylab=ylab,					# y-axis label
				las=if(wide) 2 else 0,		# vertical label if too many bars
				...
			)
		}
		else
		{	barcols <- CAT_COLORS_8[(1:nrow(vals)-1) %% length(CAT_COLORS_8)+1]
			barplot(
				height=vals,				# data
				names.arg=text,				# bar names
				beside=TRUE,				# grouped bars
				col=barcols,				# bar colors
				main=NA,					# no main title
				xlab=if(wide) NA else xlab,	# x-axis label
				ylab=ylab,					# y-axis label
				las=if(wide) 2 else 0,		# vertical label if too many bars
				...
			)
			text2 <- rownames(vals)
			idx <- which(is.na(text2))
			if(length(idx)>0)
				text2[idx] <- VAL_UNKNOWN
			legend(
				x="topleft",
				fill=barcols,
				title=names(dimnames(vals))[1],
				legend=text2
			)
		}
		if(hasArg(file))
			dev.off()
	}
}




#############################################################
# Graph plot using a circular layout.
#
# g: the signed graph to plot.
# att: node attribute corresponding to the membership
# sign.order: whether or not to place the links depending on
#             their sign: first negative, then positive.
# alt: if TRUE, nodes are plotted as points and links as lines,
#      instead of bands of various lenghts and rubans, respectively.
# file: name of the file to plot in (if none, plot on screen).
#############################################################
plot.circos <- function(g, att, sign.order=FALSE, alt=FALSE, file)
{	# remove isolates
	connected <- which(igraph::degree(g)>0)
	isg <- induced_subgraph(g, connected)
	
	# get adjacency matrix
	if(alt)
	{	# if alternative mode, force node degree to be all 2s
		# in order to have the same width for each node in the plot
		adj <- matrix(0,nrow=gorder(isg),ncol=gorder(isg))
		a <- 1:gorder(isg)
		b <- c(2:gorder(isg),1)
		for(i in 1:length(a))
		{	adj[a[i],b[i]] <- 1
			adj[b[i],a[i]] <- 1
		}
	}
	else
		# just the regular adjacency matrix
		adj <- as.matrix(get.adjacency(isg,attr="sign"))
	
	# order nodes
	membership <- factor(vertex_attr(isg,att))
	norder <- order(membership)
	
	# node names
	disp.names <- V(isg)$label0		# label label0 name 
	colnames(adj) <- disp.names
	rownames(adj) <- disp.names
	
	# node colors
	vcols <- c()
	vcols[disp.names] <- CAT_COLORS_8[(as.integer(membership)-1) %% length(CAT_COLORS_8) + 1]
	
	# compute link colors
	ecols <- adj
	ecols[which(ecols<0)] <- alpha("#E41A1C",0.5)
	ecols[which(ecols>0)] <- alpha("#1A8F39",0.5)
	ecols[which(ecols==0)] <- NA
	
	for(fformat in FORMAT)
	{	if(hasArg(file))
		{	if(fformat=="pdf")
				pdf(paste0(file,".pdf"), width=25, height=25)
			else if(fformat=="png")
				png(paste0(file,".png"), width=1024, height=1024)
		}
		
		# build the base graph
		chordDiagram(abs(adj),													# adjacency matrix
			symmetric=TRUE,
			#transparency=0.5, 													# link transparency (doesn't work)
			col=ecols,		 													# link color
			#link.sort=TRUE,													# order (position) of the link, but no control...
			#link.rank=lranks,													# order of the links, but in z
			link.visible=!sign.order && !alt,									# hide the link (draw them later)
			order=disp.names[norder],											# order of the node
			grid.col=vcols,														# node colors
			annotationTrack=c("grid"),											# just display the node colors (no names or ticks)
			preAllocateTracks=list(track.height=max(strwidth(disp.names)))		# allocate room for names (first track)
		)
		
		# add the node names
		circos.track(track.index=1,												# add the names (first track) 
			panel.fun=function(x, y)
			{	circos.text(CELL_META$xcenter, CELL_META$ylim[1], 
						CELL_META$sector.index, facing="clockwise", 
						niceFacing=TRUE, adj=c(0,0.5))
			}, 
			bg.border = NA
		)
		
		if(sign.order || alt)
		{	# order links
			el <- as_edgelist(isg, names=FALSE)
			lranks <- order(E(isg)$sign, apply(el,1,min), apply(el,1,max))
			el <- el[lranks,]
			
			# add each edge one by one (only way to control order)
			count <- rep(0,length(connected))
			for(e in 1:nrow(el))
			{	v1 <- el[e,1]
				v2 <- el[e,2]
#				cat(disp.names[v1]," .. ",adj[v1,v2]," .. ",disp.names[v2],"\n")
				if(E(isg)[v1 %--% v2]$sign>0)
					ecol <- alpha("#1A8F39",0.25)
				else
					ecol <- alpha("#E41A1C",0.50)
				if(!alt)
				{	circos.link(sector.index1=disp.names[v1], point1=c(count[v1],count[v1]+1), 
						sector.index2=disp.names[v2], point2=c(count[v2],count[v2]+1),
						col=ecol, border=ecol)
					count[v1] <- count[v1] + 1
					count[v2] <- count[v2] + 1
				}
				else
					circos.link(sector.index1=disp.names[v1], point1=c(0.9,1.1), 
						sector.index2=disp.names[v2], point2=c(0.9,1.1),
						col=ecol, border=ecol)
#				readline()
			}
		}
		
		if(hasArg(file))
			dev.off()
	}
}




#############################################################################################
# Function taken from
# https://rdrr.io/github/AtlanticR/bio.utilities/src/R/legend.bubble.r
#############################################################################################
legend.bubble  <- function (x, y = NULL, title, z, maxradius = 1, n = 3, round = 0, bty = "o", 
		mab = 1.2, bg = NULL, inset = 0, pch = 21, pt.bg = NULL, 
		txt.cex = 1, txt.col = NULL, font = NULL, ...) 
{
	if (length(z) == 1) 
		legend <- round((seq(0, sqrt(z), length.out = n + 1)^2)[-1], 
				round)
	else legend <- round(sort(z), round)
	radius <- maxradius * sqrt(legend)/sqrt(max(legend))
	cex <- 2 * radius/par("cxy")[2]/0.375
	box <- legend.box(x, y, maxradius, mab, inset)
	if (bty == "o") 
		rect(box[1], box[2], box[3], box[4], col = bg)
	x <- (box[1] + box[3])/2
	if(hasArg(title))
		text(x=x, y=box[2]+0.03, labels=title, col=txt.col, font=font)
	y <- box[2] - mab * maxradius + maxradius
	for (i in length(radius):1) {
		ri <- radius[i]
		cex <- 2 * ri/par("cxy")[2]/0.375
		points(x, y - ri, cex = cex, pch = pch, bg = pt.bg, ...)
		text(x, y - ri * 2, legend[i], adj = c(0.5, -0.5), cex = txt.cex, 
				col = txt.col, font = font)
	}
}




#############################################################################################
# Function taken from
# https://rdrr.io/github/AtlanticR/bio.utilities/src/R/legend.box.r
#############################################################################################
legend.box <- function (x, y = NULL, maxradius, mab = 1.2, inset = 0, double = F) 
{
	auto <- if (is.character(x)) 
				match.arg(x, c("bottomright", "bottom", "bottomleft", 
								"left", "topleft", "top", "topright", "right", "center"))
			else NA
	asp <- get.asp()
	h <- mab * 2 * maxradius
	w <- h * asp
	if (double) 
		h <- h * 2
	usr <- par("usr")
	inset <- rep(inset, length.out = 2)
	if (!is.na(auto)) {
		insetx <- inset[1L] * (usr[2L] - usr[1L])
		left <- switch(auto, bottomright = , topright = , right = usr[2L] - 
						w - insetx, bottomleft = , left = , topleft = usr[1L] + 
						insetx, bottom = , top = , center = (usr[1L] + usr[2L] - 
							w)/2)
		insety <- inset[2L] * (usr[4L] - usr[3L])
		top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
						h + insety, topleft = , top = , topright = usr[4L] - 
						insety, left = , right = , center = (usr[3L] + usr[4L] + 
							h)/2)
	}
	else {
		left <- x - 1.2 * asp * maxradius
		top <- y + 1.2 * maxradius
	}
	return(c(left, top, left + w, top - h))
}




#############################################################################################
# Function taken from
# https://rdrr.io/github/AtlanticR/bio.utilities/src/R/get.asp.r
#############################################################################################
get.asp <- function() 
{	pin <- par("pin")
	usr <- par("usr")
	asp <- (pin[2]/(usr[4] - usr[3]))/(pin[1]/(usr[2] - usr[1]))
	return(asp)
}
