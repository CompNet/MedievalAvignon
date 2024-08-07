# Ad hoc scripts to perform various post-analyses.
# 
# Author: Vincent Labatut
#
# source("res/analysis/ad_hoc.R")
###############################################################################




#############################################################################################
ESTATE_NBR <- 2693 	# total number of properties (real estate) in the database




#############################################################################################
# Loads the previously computed stats (for whole graphs) and put them all in a global file,
# located in the root folder (=parameter folder).
#
# graph.names: names of the graphs to compare.
# folder: root folder to find the graph and record the plots.
#
# returns: the table of combined stats.
#############################################################################################
merge.stats <- function(graph.names, folder)
{	# init stats table
	tab <- data.frame(matrix(nrow=length(graph.names),ncol=0))
	rownames(tab) <- graph.names
	tab.file <- file.path(folder, "stats_comparison.csv")
	
	# loop over graphs
	for(i in 1:length(graph.names))
	{	graph.folder <- file.path(folder, graph.names[i])
		
		# get the stat table
		stat.file <- file.path(graph.folder, "stats.csv")
		tlog(4,"Loading stats '",stat.file,"'")
		stats <- retrieve.stats(stat.file)
		
		# add to main table
		for(r in 1:nrow(stats))
		{	if(is.na(stats[r,"Value"]))
				col <- "Mean"
			else
				col <- "Value"
			
			tab[graph.names[i],rownames(stats)[r]] <- stats[r,col]
		}
		
		# get specific stats and add to main table
		attr.folder <- file.path(graph.folder, MEAS_ATTRIBUTES)
		# number of estates
		att.file <- file.path(attr.folder, COL_LOC_TYPE, paste0(COL_LOC_TYPE,"_vals.csv"))
		tt <- read.csv(file=att.file, header=TRUE)
		tab[graph.names[i],"estate_nbr"] <- tt[which(tt[,"Value"]=="Bien"),"Frequency"]
		# distance correlation values
		dist.file <- file.path(graph.folder, "distance", "undirected", "comparison", "distance_correlations.csv")
		tt <- read.csv(file=dist.file, header=TRUE)
		tab[graph.names[i],"PearsonFin_DB"] <- tt[which(tt[,"Coordinates"]=="Database"), "PearsonFiniteCoef"]
		tab[graph.names[i],"SpearmanFin_DB"] <- tt[which(tt[,"Coordinates"]=="Database"), "SpearmanFiniteCoef"]
		tab[graph.names[i],"SpearmanInf_DB"] <- tt[which(tt[,"Coordinates"]=="Database"), "SpearmanInfiniteCoef"]
		tab[graph.names[i],"KendallFin_DB"] <- tt[which(tt[,"Coordinates"]=="Database"), "KendallFiniteCoef"]
		tab[graph.names[i],"KendallInf_DB"] <- tt[which(tt[,"Coordinates"]=="Database"), "KendallInfiniteCoef"]
		
		# record updated table
		tlog(4,"Update stat file '",tab.file,"'")
		write.csv(tab, file=tab.file, row.names=TRUE)
	}
	
	return(tab)
}




#############################################################################################
# Loads previously computed stats and plot the distance correlation vs. the number of estate 
# nodes, as a Pareto plot.
#############################################################################################
plot.stats.comparison <- function()
{	tlog(2, "Producing decision figure")
	
	# load overall stats
	tab.file <- file.path(FOLDER_OUT_ANAL_EST, "stats_comparison.csv")
	tlog(4, "Reading table '",tab.file,"'")
	tab <- read.csv(file=tab.file, header=TRUE, row.names=1)
	stats <- tab[,c("estate_nbr", "SpearmanInf_DB", "KendallInf_DB")]
	
	# remove redundant graphs
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_311"),]
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_311_filtered"),]
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_303"),]
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_303_filtered"),]
	stats <- stats[-which(rownames(stats)=="split_raw/flat_minus_311"),]
	stats <- stats[-which(rownames(stats)=="split_raw/flat_minus_311_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_7"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_7_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_9"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_9_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_raw/flat_minus_6"),]
	stats <- stats[-which(rownames(stats)=="whole_raw/flat_minus_6_filtered"),]
	
	# set up colors
	col <- 1
	col.names <- "Basic"
	cols <- rep(col,nrow(stats))
	
	# load street removal stats
	for(graph.type in c("whole_raw","whole_ext","split_raw","split_ext"))
	{	for(filt.txt in c("","_filtered"))
		{	tab.file <- file.path(FOLDER_OUT_ANAL_EST,graph.type,paste0("flat_minus",filt.txt),"_removed_streets","stats.csv")
			tlog(4, "Reading table '",tab.file,"'")
			tab <- read.csv(file=tab.file, header=TRUE)
			tmp <- tab[,c("estate_nbr", "distance.cor.spearman.infinite", "distance.cor.kendall.infinite")]
			colnames(tmp) <- c("estate_nbr", "SpearmanInf_DB", "KendallInf_DB")
			rownames(tmp) <- paste0(graph.type,"/flat_minus",filt.txt,"_",tab[,"NumberDeletedStreets"],"(",floor(tab[,"LastDeletedStreetLength"]),")")
			stats <- rbind(stats, tmp)
			col <- col + 1
			col.names <- c(col.names, paste0(graph.type,"/flat_minus",filt.txt,"_X"))
			cols <- c(cols, rep(col,nrow(tmp)))
		}
	}
	
	# record as csv file
	tab.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("pareto-plot.csv"))
	write.csv(stats, file=tab.file, row.names=TRUE)
	
	# create plots
	pal <- get.palette(max(cols))
	for(corr.txt in c("spearman","kendall"))
	{	plot.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("pareto-plot_",corr.txt))
		if(corr.txt=="spearman")
			vals <- stats[,"SpearmanInf_DB"]
		else
			vals <- stats[,"KendallInf_DB"]
		tlog(4, "Creating plots '",plot.file,"'")
		for(fformat in c("png","pdf"))	# FORMAT
		{	if(fformat=="pdf")
				pdf(paste0(plot.file,".pdf"))
			else if(fformat=="png")
				png(paste0(plot.file,".png"))
			# draw all values
			plot(
				x=stats[,"estate_nbr"],
				y=vals,
				xlab="Number of estate vertices",
				ylab="Distance correlation",
				col=pal[cols]
			)
			# draw Pareto front
			df <- data.frame(x=stats[,"estate_nbr"], y=vals)
			pref <- high(x)*high(y)
			sky <- psel(df=df, pref=pref)
			plot_front(df=df, pref=pref, col="GREY", lty=2)
			#points(df[,"x"], df[,"y"], lwd=3)
			# add point names
			text(
				x=stats[,"estate_nbr"],
				y=vals,
				labels=rownames(stats),
				cex=0.1,
				col=pal[cols]
			)
			# add legend
			legend(
				"bottomleft",
				fill=pal,
				legend=col.names
			)
			dev.off()
		}
	}
}




#############################################################################################
# Loads previously computed stats and plot the distance correlation vs. the number of estate 
# nodes. This modified version produces the plots from the article.
#
# norm.est: whether to express coverage as the proportion (FALSE) of estate properties remaning
#           in the graph, instead of their absolute number (TRUE).
#############################################################################################
plot.stats.comparison2 <- function(norm.est=FALSE)
{	tlog(2, "Producing decision figure")
	
	# load overall stats
	tab.file <- file.path(FOLDER_OUT_ANAL_EST, "stats_comparison.csv")
	tlog(4, "Reading table '",tab.file,"'")
	tab <- read.csv(file=tab.file, header=TRUE, row.names=1)
	stats <- tab[,c("estate_nbr", "SpearmanInf_DB", "KendallInf_DB")]
	
	# remove redundant graphs (with street removal stats)
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_311"),]
#	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_311_filtered"),]
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_303"),]
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus_303_filtered"),]
	stats <- stats[-which(rownames(stats)=="split_raw/flat_minus_311"),]
#	stats <- stats[-which(rownames(stats)=="split_raw/flat_minus_311_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_7"),]
#	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_7_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_9"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus_9_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_raw/flat_minus_6"),]
#	stats <- stats[-which(rownames(stats)=="whole_raw/flat_minus_6_filtered"),]
	
	# remove non-filtered graphs
	stats <- stats[-which(rownames(stats)=="split_ext/full"),]
	stats <- stats[-which(rownames(stats)=="split_ext/flat_minus"),]
	stats <- stats[-which(rownames(stats)=="split_ext/flat_relations"),]
	stats <- stats[-which(rownames(stats)=="split_raw/full"),]
	stats <- stats[-which(rownames(stats)=="split_raw/flat_minus"),]
	stats <- stats[-which(rownames(stats)=="split_raw/flat_relations"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/full"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/estate_level"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/estate_level_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_minus"),]
	stats <- stats[-which(rownames(stats)=="whole_ext/flat_relations"),]
	stats <- stats[-which(rownames(stats)=="whole_raw/estate_level"),]
	stats <- stats[-which(rownames(stats)=="whole_raw/estate_level_filtered"),]
	stats <- stats[-which(rownames(stats)=="whole_raw/flat_minus"),]
	stats <- stats[-which(rownames(stats)=="whole_raw/flat_relations"),]
	
	# set up article names
	point.names <- c(
		"whole_raw/full"="Full",
		"whole_raw/full_filtered"="RHW_all", "whole_raw/flat_relations_filtered"="RFW_all", "whole_raw/flat_minus_filtered"="RFW_streets", "whole_raw/flat_minus_6_filtered"="RFW_k",
		"whole_ext/full_filtered"="EHW_all", "whole_ext/flat_relations_filtered"="EFW_all", "whole_ext/flat_minus_filtered"="EFW_streets", "whole_ext/flat_minus_7_filtered"="EFW_k",
		"split_raw/full_filtered"="RHS_all", "split_raw/flat_relations_filtered"="RFS_all", "split_raw/flat_minus_filtered"="RFS_streets", "split_raw/flat_minus_311_filtered"="RFS_k",
		"split_ext/full_filtered"="EHS_all", "split_ext/flat_relations_filtered"="EFS_all", "split_ext/flat_minus_filtered"="EFS_streets", "split_ext/flat_minus_311_filtered"="EFS_k"
	)
	
	# set up colors
	cols <- c(
		"whole_raw/full"=1,
		"whole_raw/full_filtered"=2, "whole_raw/flat_relations_filtered"=2, "whole_raw/flat_minus_filtered"=2, "whole_raw/flat_minus_6_filtered"=2,
		"whole_ext/full_filtered"=3, "whole_ext/flat_relations_filtered"=3, "whole_ext/flat_minus_filtered"=3, "whole_ext/flat_minus_7_filtered"=3,
		"split_raw/full_filtered"=4, "split_raw/flat_relations_filtered"=4, "split_raw/flat_minus_filtered"=4, "split_raw/flat_minus_311_filtered"=4,
		"split_ext/full_filtered"=5, "split_ext/flat_relations_filtered"=5, "split_ext/flat_minus_filtered"=5, "split_ext/flat_minus_311_filtered"=5
	)
	pal.names <- c("Full graph", "Raw data & Whole vertices (R.W_.)", "Extended data & Whole vertices (E.W_.)", "Raw data & Split vertices (R.S_.)", "Extended data & Split vertices (E.S_.)")
	pal.cols <- c("BLACK", CAT_COLORS_8[1:4])
			
#	# load street removal stats
#	for(graph.type in c("whole_raw","whole_ext","split_raw","split_ext"))
#	{	for(filt.txt in c("","_filtered"))
#		{	tab.file <- file.path(FOLDER_OUT_ANAL_EST,graph.type,paste0("flat_minus",filt.txt),"_removed_streets","stats.csv")
#			tlog(4, "Reading table '",tab.file,"'")
#			tab <- read.csv(file=tab.file, header=TRUE)
#			tmp <- tab[,c("estate_nbr", "distance.cor.spearman.infinite", "distance.cor.kendall.infinite")]
#			colnames(tmp) <- c("estate_nbr", "SpearmanInf_DB", "KendallInf_DB")
#			rownames(tmp) <- paste0(graph.type,"/flat_minus",filt.txt,"_",tab[,"NumberDeletedStreets"],"(",floor(tab[,"LastDeletedStreetLength"]),")")
#			stats <- rbind(stats, tmp)
#			col <- col + 1
#			col.names <- c(col.names, paste0(graph.type,"/flat_minus",filt.txt,"_X"))
#			cols <- c(cols, rep(col,nrow(tmp)))
#		}
#	}
	
	# record as csv file
	tab.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("pareto-plot_2.csv"))
	write.csv(stats, file=tab.file, row.names=TRUE)
	
	# set x values
	xs <- stats[,"estate_nbr"]
	xlab <- "Number of property vertices"
	if(norm.est)
	{	xs <- xs/ESTATE_NBR*100
		xlab <- "Proportion of property vertices (%)"
	}
	
	# create plots
	for(corr.txt in c("spearman","kendall"))
	{	plot.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("pareto-plot_",corr.txt,"_2"))
		if(corr.txt=="spearman")
			vals <- stats[,"SpearmanInf_DB"]
		else
			vals <- stats[,"KendallInf_DB"]
		tlog(4, "Creating plots '",plot.file,"'")
		for(fformat in c("png","pdf"))	# FORMAT
		{	if(fformat=="pdf")
				pdf(paste0(plot.file,".pdf"))
			else if(fformat=="png")
				png(paste0(plot.file,".png"))
			# draw all values
			par(mar=c(4,4,0,0)+0.1) 	# remove the title space -- Bottom Left Top Right
			plot(
				x=xs,
				y=vals,
#				xlab="Number of estate vertices",
				xlab=xlab,		
				ylab="Distance correlation",
				col=pal.cols[cols[rownames(stats)]]
#				xlim=c(0,100), ylim=c(0,1)
			)
			# draw Pareto front
			df <- data.frame(x=xs, y=vals)
			pref <- high(x)*high(y)
			sky <- psel(df=df, pref=pref)
			plot_front(df=df, pref=pref, col="GREY", lty=2)
			# add point names
			text(
				x=xs,
				y=vals,
				labels=point.names[rownames(stats)],
				cex=0.1,
				col=pal.cols[cols[rownames(stats)]]
			)
			# add legend
			legend(
				"bottomleft",
				fill=pal.cols,
				legend=pal.names
			)
			dev.off()
		}
	}
}




#############################################################################################
# Extracts the specified attributes for the specified graphs.
#
# graph.names: names of the graphs to process.
# folder: root folder to find the graph and record the files.
# attributes: names of the attributes to extract from the graphs.
#############################################################################################
extract.vertex.attributes <- function(graph.names, folder, attributes=NA)
{	tlog(0,"Retrieving vertex attributes in graphs")
	
	# loop over graphs
	for(i in 1:length(graph.names))
	{	tlog(2,"Processing graph ",graph.names[i]," (",i,"/",length(graph.names),")")
		
		# read graph
		graph.folder <- file.path(folder, graph.names[i])
		graph.file <- file.path(graph.folder, FILE_GRAPH)
		tlog(4,"Reading file '",graph.file,"'")
		g <- load.graphml.file(file=graph.file)
		
		# init table containing attribute values
		df <- data.frame(vertex_attr(g, COL_LOC_ID), get.names(g))
		colnames(df) <- c("Id","Name") 
		
		# possibly init attribute list
		if(all(is.na(attributes)))
			attributes <- vertex_attr_names(g)
		
		# loop over attributes
		tlog(4,"Looping over vertex attributes")
		for(a in 1:length(attributes))
		{	tlog(6,"Processing vertex attribute '",attributes[a],"' (",a,"/",length(attributes),")")
			if(attributes[a] %in% vertex_attr_names(g))
			{	# complete table
				vals <- vertex_attr(graph=g, name=attributes[a])
				df <- cbind(df, vals)
				colnames(df)[ncol(df)] <- attributes[a]
			}
			else
				tlog(8,"ERROR: did not find this vertex attribute in this graph")
		}
		
		# record table as CSV
		tab.file <- file.path(folder,paste0("vertex-attr_",gsub("/", "__", graph.names[i]),".csv"))
#		tab.file <- file.path(folder,paste0("distances_",gsub("/", "__", graph.names[i]),".csv"))
		write.csv(df, file=tab.file, row.names=FALSE)
	}
}
#extract.vertex.attributes(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)
#extract.vertex.attributes(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST, attributes=c(COL_LOC_X,COL_LOC_Y,COL_LOC_INTER_X,COL_LOC_INTER_Y,COL_LOC_INTER))
#extract.vertex.attributes(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST, attributes=c("SpatialDist_database-arith_mean","SpatialDist_database-harmo_mean","SpatialDist_interpolation-arith_mean","SpatialDist_interpolation-harmo_mean","distance_undirected-arith_avg","distance_undirected-harmo_avg"))




#############################################################################################
# Record all graphs as edgelists.
#
# graph.names: names of the graphs to process.
# folder: root folder to find the graph and record the files.
#############################################################################################
export.graphs.as.edgelists <- function(graph.names, folder)
{	tlog(0,"Exporting graphs as edgelists")
	
	# loop over graphs
	for(i in 1:length(graph.names))
	{	tlog(2,"Processing graph ",graph.names[i]," (",i,"/",length(graph.names),")")
		
		# read graph
		graph.folder <- file.path(folder, graph.names[i])
		graph.file <- file.path(graph.folder, FILE_GRAPH)
		tlog(4,"Reading file '",graph.file,"'")
		g <- load.graphml.file(file=graph.file)
		
		# get edgelist
		el <- as_edgelist(graph=g, names=TRUE)
		el <- cbind(el, E(g)$type)
		colnames(el) <- c("Vertex1","Vertex2","EdgeType")
		
		# record table as CSV
		tab.file <- file.path(folder,paste0("edgelist_",gsub("/", "__", graph.names[i]),".csv"))
		tlog(4,"Recording in file '",tab.file,"'")
		write.csv(el, file=tab.file, row.names=FALSE)
	}
}
#export.graphs.as.edgelists(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)




#############################################################################################
# Compute inconsistencies in the relationships (only for N-S-E-W relations).
#############################################################################################
compute.inconsistencies <- function()
{	tlog(0,"Computing inconsistencies in the full graph, for N-S-E-W relations")
	
	# load full graph
	graph.file <- file.path(FOLDER_OUT_ANAL_EST,"whole_raw","full",FILE_GRAPH)
	g <- load.graphml.file(graph.file)
	
	# identify NSEW relationships
	idx <- which(E(g)$type %in% c(VAL_CONF_TYPE_NORD, VAL_CONF_TYPE_SUD, VAL_CONF_TYPE_EST, VAL_CONF_TYPE_OUEST))
	el <- as_edgelist(graph=g, names=TRUE)[idx,]
	types <- E(g)$type[idx]
	
	# conversion map
	conv.opp <- c()
	conv.opp[VAL_CONF_TYPE_NORD] <- VAL_CONF_TYPE_SUD
	conv.opp[VAL_CONF_TYPE_SUD] <- VAL_CONF_TYPE_NORD
	conv.opp[VAL_CONF_TYPE_EST] <- VAL_CONF_TYPE_OUEST
	conv.opp[VAL_CONF_TYPE_OUEST] <- VAL_CONF_TYPE_EST
	#
	conv.unr <- matrix(NA, nrow=4, ncol=2)
	rownames(conv.unr) <- c(VAL_CONF_TYPE_NORD, VAL_CONF_TYPE_SUD, VAL_CONF_TYPE_EST, VAL_CONF_TYPE_OUEST)
	conv.unr[VAL_CONF_TYPE_NORD,] <- conv.unr[VAL_CONF_TYPE_SUD,] <- c(VAL_CONF_TYPE_EST, VAL_CONF_TYPE_OUEST)
	conv.unr[VAL_CONF_TYPE_EST,] <- conv.unr[VAL_CONF_TYPE_OUEST,] <- c(VAL_CONF_TYPE_NORD, VAL_CONF_TYPE_SUD)
	
	tlog(2,"Total number of NSEW edges: ",nrow(el))
	
	# detect pairs of redundant relationships
	vect.red <- rep(NA,nrow(el))
	for(e in 1:nrow(el))
		vect.red[e] <- length(setdiff(which(el[,1]==el[e,1] & el[,2]==el[e,2] & types==types[e]),e))
	count.red <- sum(vect.red)/2
	tlog(2,"Number of pairs of parallel edges connecting the same vertices with the same types: ",count.red)
	
	# detect pairs of inconsistent relationships
	vect.inc <- rep(NA,nrow(el))
	for(e in 1:nrow(el))
		vect.inc[e] <- length(which(el[,1]==el[e,1] & el[,2]==el[e,2] & types==conv.opp[types[e]]))
	count.inc <- sum(vect.inc)/2
	tlog(2,"Number of pairs of parallel edges connecting the same vertices with opposed types (N vs. S, E vs.W): ",count.inc)
	
	# detect pairs of unrelated relationships
	vect.unr <- rep(NA,nrow(el))
	for(e in 1:nrow(el))
		vect.unr[e] <- length(which(el[,1]==el[e,1] & el[,2]==el[e,2] & types %in% conv.unr[types[e]]))
	count.unr <- sum(vect.unr)/2
	tlog(2,"Number of pairs of parallel edges connecting the same vertices with unrelated types: ",count.unr)
	
	####
	
	# detect pairs of symmetric relationships
	vect.sym <- rep(NA,nrow(el))
	for(e in 1:nrow(el))
		vect.sym[e] <- length(which(el[,1]==el[e,2] & el[,2]==el[e,1] & types==conv.opp[types[e]]))
	count.sym <- sum(vect.sym)/2
	tlog(2,"Number of pairs of reciprocal edges connecting the same vertices with opposed types (N vs. S, E vs.W): ",count.sym)
	
	# detect pairs of inconsistent relationships
	vect.equ <- rep(NA,nrow(el))
	for(e in 1:nrow(el))
		vect.equ[e] <- length(which(el[,1]==el[e,2] & el[,2]==el[e,1] & types==types[e]))
	count.equ <- sum(vect.equ)/2
	tlog(2,"Number of pairs of reciprocal edges connecting the same vertices with the same types: ",count.equ)
	
	# detect pairs of unrelated relationships
	vect.unr <- rep(NA,nrow(el))
	for(e in 1:nrow(el))
		vect.unr[e] <- length(which(el[,1]==el[e,2] & el[,2]==el[e,1] & types %in% conv.unr[types[e]]))
	count.unr <- sum(vect.unr)/2
	tlog(2,"Number of pairs of reciprocal edges connecting the same vertices with unrelated types: ",count.unr)
}




#############################################################################################
# Redraws the distance-distance plots so that they all use the same ranges for both distances.
#
# graph.types: list of graph types to consider.
# mode: undirected (default) or directed.
# sep.legend: whether to plot the legend in a separate file in the global plot.
#############################################################################################
normalize.distance.plots <- function(graph.types, mode=MEAS_MODE_UNDIR, sep.legend=FALSE)
{	tlog(0,"Redrawing distance-distance plots to get a fixed range on the axes")
	
	# init min/max values
	g.min <- NA
	g.max <- NA
	s.min <- NA
	s.max <- NA
	env.min <- NA
	env.max <- NA
	
	# read the previously computed results
	tlog(2,"Reading previously computed distance values")
	for(gt in graph.types)
	{	for(sdist in c("database","interpolation"))
		{	# read data
			tab.file <- file.path(FOLDER_OUT_ANAL_EST, gt, MEAS_DISTANCE, mode, "comparison", paste0("geodesic_vs_spatial-",sdist,"_values.csv"))
			tlog(4,"Reading file ",tab.file)
			vals <- read.csv(file=tab.file, header=TRUE)
			# update boundaries
			idx <- !is.infinite(vals[,"Geodesic"])
			g.min <- min(g.min, vals[idx,"Geodesic"], na.rm=TRUE)
			g.max <- max(g.max, vals[idx,"Geodesic"], na.rm=TRUE)
			s.min <- min(s.min, vals[,"Spatial"], na.rm=TRUE)
			s.max <- max(s.max, vals[,"Spatial"], na.rm=TRUE)
			
			# read stats
			tab.file <- file.path(FOLDER_OUT_ANAL_EST, gt, MEAS_DISTANCE, mode, "comparison", paste0("geodesic_vs_spatial-",sdist,"_avg-std.csv"))
			tlog(4,"Reading file ",tab.file)
			vals <- read.csv(file=tab.file, header=TRUE)
			# update boundaries
			env.min <- min(env.min, vals[,"SpatialAvg"]-vals[,"SpatialStdev"], na.rm=TRUE)
			env.max <- max(env.max, vals[,"SpatialAvg"]+vals[,"SpatialStdev"], na.rm=TRUE)
		}
	}
	
	# set gap used when plotting infinite distances
	tlog(2,"Detected range: geodesic: [",g.min,";",g.max,"] - spatial: [",s.min,";",s.max,"] - envelope: [",env.min,";",env.max,"]")
	gap <- round(0.070*(g.max-g.min))
	infinite.val <- g.max + gap + 1
	
	# init lists
	series.xs <- list(database=list(), interpolation=list())
	series.avgs <- list(database=list(), interpolation=list())
	series.sds <- list(database=list(), interpolation=list())
	s <- 1
	
	# generate all plots again
	tlog(2,"Recreating graph-specific plots, but using the same range")
	for(gt in graph.types)
	{	tlog(4, "Dealing with graph ",gt)
		
		for(sdist in c("database","interpolation"))
		{	# read data
			tab.file <- file.path(FOLDER_OUT_ANAL_EST, gt, MEAS_DISTANCE, mode, "comparison", paste0("geodesic_vs_spatial-",sdist,"_values.csv"))
			tlog(4,"Reading file ",tab.file)
			vals <- read.csv(file=tab.file, header=TRUE)
			gvals <- vals[,"Geodesic"]
			svals <- vals[,"Spatial"]
			all.finite <- !any(is.infinite(gvals))
#if(all.finite)
#	stop("No infinite distance at all, is that even possible? (",gt,")")
#	cat(">>>>>>>>>>>>>>>>>>>>>>>>>> No infinite distance at all, is that even possible? (",gt,")\n")
			idx <- !is.infinite(gvals)
			gvals0 <- gvals
			svals0 <- svals
			gvals2 <- gvals0; gvals2[which(is.infinite(gvals0))] <- rep(infinite.val, length(which(is.infinite(gvals0))))
			svals2 <- svals0
			gvals <- gvals[idx]
			svals <- svals[idx]
			
			# read stats
			tab.file <- file.path(FOLDER_OUT_ANAL_EST, gt, MEAS_DISTANCE, mode, "comparison", paste0("geodesic_vs_spatial-",sdist,"_avg-std.csv"))
			tlog(4,"Reading file ",tab.file)
			vals <- read.csv(file=tab.file, header=TRUE)
			xs0 <- vals[,"Geodesic"]
			xs <- xs0[!is.infinite(xs0)]
			xs2 <- sort(unique(gvals2))
			avg.dist0 <- vals[,"SpatialAvg"]
			avg.dist <- avg.dist0[!is.infinite(xs0)]
			avg.dist2 <- sapply(xs2, function(x) mean(svals2[gvals2==x],na.rm=TRUE))
			stdev.dist0 <- vals[,"SpatialStdev"]
			stdev.dist <- stdev.dist0[!is.infinite(xs0)]
			stdev.dist2 <- sapply(xs2, function(x) sd(svals2[gvals2==x],na.rm=TRUE))
			stdev.dist2[is.na(stdev.dist2)] <- 0
			
			# add to lists
			series.xs[[sdist]][[s]] <- xs0
			series.avgs[[sdist]][[s]] <- avg.dist0
			series.sds[[sdist]][[s]] <- stdev.dist0
			
			# plot both distances as a binned scatterplot
			plot.file <- file.path(FOLDER_OUT_ANAL_EST, gt, MEAS_DISTANCE, mode, "comparison", paste0("geodesic_vs_spatial-",sdist,"_binned_fixed-range"))
			tlog(4,"Plot binned version in '",plot.file,"'")
			for(fformat in FORMAT)
			{	if(fformat=="pdf")
					pdf(paste0(plot.file,".pdf"))
				else if(fformat=="png")
					png(paste0(plot.file,".png"))
				par(mar=c(4,4,0,0)+0.1) 	# remove the title space -- Bottom Left Top Right
				plot(
					NULL,
					xlab="Undirected geodesic distance", ylab="Spatial distance",
					las=1, #log="xy", 
					xaxt="n",
					ylim=c(env.min, env.max),
					xlim=c(g.min, infinite.val)
				)
				polygon(
					x=c(xs,rev(xs)), y=c(avg.dist-stdev.dist,rev(avg.dist+stdev.dist)), 
					col=make.color.transparent("RED",85), border=NA
				)
				lines(
					x=xs, y=avg.dist, 
					col="RED", pch=19
				)
				if(!all.finite)
				{	last <- length(xs2)
					points(x=xs2[last], y=avg.dist2[last], col="RED")
					arrows(
						x0=xs2[last], y0=avg.dist2[last]-stdev.dist2[last], 
						x1=xs2[last], y1=avg.dist2[last]+stdev.dist2[last], 
						code=3, angle=90, length=0.05, 
						col=make.color.transparent("RED",85), lwd=2
					)
				}
				axis(side=1, at=c(seq(0,g.max,10),infinite.val), labels=c(seq(0,g.max,10),expression(+infinity)))
				axis.break(axis=1,breakpos=infinite.val-gap,style="gap",brw=0.02)
				dev.off()
			}
		}
		s <- s + 1
	}
	
	tlog(2,"Creating overall plot")
	ns <- which(!grepl("_filtered", graph.types, fixed=TRUE))
	pal <- get.palette(length(ns))
	
	for(sdist in c("database","interpolation"))
	{	plot.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("geodesic_vs_spatial-",sdist,"_binned_fixed-range"))
		tlog(4,"Plot binned version in '",plot.file,"'")
		for(fformat in FORMAT)
		{	if(fformat=="pdf")
				pdf(paste0(plot.file,".pdf"))
			else if(fformat=="png")
				png(paste0(plot.file,".png"))
			# init plot
			par(mar=c(4,4,0,0)+0.1) 	# remove the title space -- Bottom Left Top Right
			plot(
				NULL,
				xlab="Undirected geodesic distance", ylab="Spatial distance",
				las=1, #log="xy", 
				xaxt="n",
				ylim=c(env.min, env.max),
				xlim=c(g.min, infinite.val)
			)
			# add each series separately
			cols <- c()
			ltys <- c()
			for(s in 1:length(graph.types))
			{	# retrieve stats
				xs0 <- series.xs[[sdist]][[s]]
				xs <- xs0[!is.infinite(xs0)]
				xs2 <- xs0; xs2[is.infinite(xs0)] <- infinite.val
				avg.dist0 <- series.avgs[[sdist]][[s]]
				avg.dist <- avg.dist0[!is.infinite(xs0)]
				avg.dist2 <- avg.dist0
				stdev.dist0 <- series.sds[[sdist]][[s]]
				stdev.dist <- stdev.dist0[!is.infinite(xs0)]
				stdev.dist2 <- stdev.dist0
				
				# update graphic params
				if(grepl("_filtered", graph.types[s], fixed=TRUE))
				{	gt <- gsub("_filtered", replacement="", x=graph.types[s], fixed=TRUE)
					lty <- 2	# 2 (dashed) or 3 (dotted)
				}
				else
				{	gt <- graph.types[s]
					lty <- 1
				}
				col <- pal[graph.types[ns]==gt]
				cols <- c(cols, col)
				ltys <- c(ltys, lty)
				# draw envelope
#				polygon(
#					x=c(xs,rev(xs)), y=c(avg.dist-stdev.dist,rev(avg.dist+stdev.dist)), 
#					col=make.color.transparent(col,90), border=NA
#				)
				# draw average
				lines(
					x=xs, y=avg.dist, 
					col=col, lty=lty
				)
				#if(!all.finite)
				{	last <- length(xs2)
					points(x=xs2[last], y=avg.dist2[last], col=col)
#					arrows(
#						x0=xs2[last], y0=avg.dist2[last]-stdev.dist2[last], 
#						x1=xs2[last], y1=avg.dist2[last]+stdev.dist2[last], 
#						code=3, angle=90, length=0.05, 
#						col=make.color.transparent(col,85), lwd=2
#					)
				}
			}
			axis(side=1, at=c(seq(0,g.max,10),infinite.val), labels=c(seq(0,g.max,10),expression(+infinity)))
			axis.break(axis=1, breakpos=infinite.val-gap, style="gap", brw=0.02)
			if(sep.legend)
			{	dev.off()
				if(fformat=="pdf")
					pdf(paste0(plot.file,"_legend.pdf"), width=10, height=20)
				else if(fformat=="png")
					png(paste0(plot.file,"_legend.png"), width=512, height=1024)
				plot(NULL, xaxt="n", yaxt="n", bty="n", ylab="", xlab="", xlim=0:1, ylim=0:1)
			}
			legend(
				x="topleft",
				col=cols, lty=ltys,
				title="Graphs",
				legend=graph.types
			)
			dev.off()
		}
	}
}
#normalize.distance.plots(graph.types=graph.types, mode=MEAS_MODE_UNDIR)




#############################################################################################
# Plots a few additional plots specifically for the paper.
#############################################################################################
plot.additional.stats <- function()
{	# evolution of reliability and coverage as functions of street removal, on the same plot
	stat.texts <- c("Proportion of properties among vertices (%)", "Distance correlation")
	stat.codes <- c("estate_nbr", "distance-cor-spearman-infinite") 
	in.folders <- c("whole_raw", "whole_ext", "split_raw", "split_ext")
	in.cols <- c(CAT_COLORS_8[1], CAT_COLORS_8[2], CAT_COLORS_8[1], CAT_COLORS_8[2])
	in.ltys <- c(1, 1, 2, 2)
	in.names <- c("RFW_k", "EFW_k", "RFS_k", "EFS_k")
	for(m in 1:length(stat.texts))
	{	stat.text <- stat.texts[m]
		tlog(2,"..Dealing with ",stat.text)
		
		# load the series
		ys <- list()
		xs <- list()
		for(in.folder in in.folders)
		{	# load the original value (unchanged network)
			if(m==1)
			{	attr.folder <- file.path(FOLDER_OUT_ANAL_EST, in.folder, "flat_minus_filtered", MEAS_ATTRIBUTES)
				att.file <- file.path(attr.folder, COL_LOC_TYPE, paste0(COL_LOC_TYPE,"_vals.csv"))
				tt <- read.csv(file=att.file, header=TRUE)
				vals <- tt[which(tt[,"Value"]=="Bien"),"Frequency"]
			}
			else
			{	dist.file <- file.path(FOLDER_OUT_ANAL_EST, in.folder, "flat_minus_filtered", "distance", "undirected", "comparison", "distance_correlations.csv")
				tt <- read.csv(file=dist.file, header=TRUE)
				vals <- tt[which(tt[,"Coordinates"]=="Database"), "SpearmanInfiniteCoef"]
			}
			
			# load the values resulting from street ablation
			tab.file <- file.path(FOLDER_OUT_ANAL_EST, in.folder, "flat_minus_filtered", "_removed_streets", "stats.csv")
			tab <- read.csv(file=tab.file, header=TRUE, row.names=1, check.names=FALSE)
			vals <- c(vals, tab[,stat.codes[m]])
			if(m==1)
				vals <- vals / ESTATE_NBR * 100
			ys[[in.folder]] <- vals
			xs[[in.folder]] <- (1:length(vals)-1) / (length(vals)-1) * 100
		}
		
		# set bounds
		xlim <- c(0,100)
		if(m==1)
			ylim <- c(0,100)
		else
			ylim <- c(0,1)
		
		# generate the plot
		plot.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("street-ablation_",stat.codes[m]))
		tlog(4, "Plotting \"",plot.file,"\"")
		for(fformat in FORMAT)
		{	if(fformat=="pdf")
				pdf(paste0(plot.file,".pdf"))
			else if(fformat=="png")
				png(paste0(plot.file,".png"))
			par(mar=c(4,4,0,0)+0.1) 	# remove the title space -- Bottom Left Top Right
			plot(
				NULL,
				xlab="Proportion of removed/split streets (%)", ylab=stat.text,
				#las=1, #log="xy", 
				xlim=xlim, ylim=ylim
			)
			for(s in 1:length(in.folders))
			{	lines(
					x=xs[[s]], y=ys[[s]], 
					col=in.cols[s], lty=in.ltys[s]#, pch=19
				)
			}
			legend(
				x="bottomright",
				col=in.cols, lty=in.ltys,
				title="Method",
				legend=in.names
			)
			dev.off()
		}
	}
}




#############################################################################################
# Produces the Pareto plot for all the variants of a network undergoing iterative street removal.
#
# graph.type: folder containing the targeted graph.
#############################################################################################
plot.pareto.streetrem <- function(graph.type)
{	tlog(0, "Producing Pareto plot for graph.type=\"",graph.type,"\"")
	main.folder <- file.path(FOLDER_OUT_ANAL_EST, graph.type, "_removed_streets")
	
	# load stats
	tab.file <- file.path(main.folder,"stats.csv")
	tlog(2, "Reading table '",tab.file,"'")
	tmp <- read.csv(file=tab.file, header=TRUE)
	tab <- tmp[,c("estate_nbr", "distance.cor.spearman.infinite", "distance.cor.kendall.infinite")]
	colnames(tab) <- c("estate_nbr", "SpearmanInf_DB", "KendallInf_DB")
	rownames(tab) <- paste0("k=",tmp[,"NumberDeletedStreets"])
	
	# record as csv file
	tab.file <- file.path(main.folder, "pareto-plot.csv")
	write.csv(tab, file=tab.file, row.names=TRUE)
	
	# create plots
	cols <- viridis(n=nrow(tab))
	for(corr.txt in c("spearman","kendall"))
	{	plot.file <- file.path(main.folder, paste0("pareto-plot_",corr.txt))
		if(corr.txt=="spearman")
			vals <- tab[,"SpearmanInf_DB"]
		else
			vals <- tab[,"KendallInf_DB"]
		tlog(4, "Creating plots '",plot.file,"'")
		for(fformat in c("png","pdf"))	# FORMAT
		{	if(fformat=="pdf")
				pdf(paste0(plot.file,".pdf"))
			else if(fformat=="png")
				png(paste0(plot.file,".png"))
			# draw all values
			plot(
				x=tab[,"estate_nbr"],
				y=vals,
				xlab="Number of property vertices",
				ylab="Distance correlation",
				col=cols
			)
			# draw Pareto front
			df <- data.frame(x=tab[,"estate_nbr"], y=vals)
			pref <- high(x)*high(y)
			sky <- psel(df=df, pref=pref)
			idx <- psel.indices(df=df, pref=pref)
			plot_front(df=df, pref=pref, col="GREY", lty=2)
			#points(df[,"x"], df[,"y"], lwd=3)
			# add point names
			text(
				x=tab[idx,"estate_nbr"],
				y=vals[idx],
				pos=4,
				labels=rownames(tab)[idx],
				cex=1,
				col=cols[idx]
			)
			# add legend
			gradientLegend(valRange=range(tmp[,"NumberDeletedStreets"]), color=cols, inside=TRUE, side=2, labels=rownames(tab))
			dev.off()
		}
	}
}




#############################################################################################
# Retries and renames the distance comparison for the paper.
#############################################################################################
collect.dist.comp.plots <- function()
{	# used to name the files
	plot.names <- c(
		"whole_raw/full"="Full",
		"whole_raw/full_filtered"="RHW_all", "whole_raw/flat_relations_filtered"="RFW_all", "whole_raw/flat_minus_filtered"="RFW_streets", "whole_raw/flat_minus_6_filtered"="RFW_k",
		"whole_ext/full_filtered"="EHW_all", "whole_ext/flat_relations_filtered"="EFW_all", "whole_ext/flat_minus_filtered"="EFW_streets", "whole_ext/flat_minus_7_filtered"="EFW_k",
		"split_raw/full_filtered"="RHS_all", "split_raw/flat_relations_filtered"="RFS_all", "split_raw/flat_minus_filtered"="RFS_streets", "split_raw/flat_minus_311_filtered"="RFS_k",
		"split_ext/full_filtered"="EHS_all", "split_ext/flat_relations_filtered"="EFS_all", "split_ext/flat_minus_filtered"="EFS_streets", "split_ext/flat_minus_311_filtered"="EFS_k"
	)
	
	for(plot in names(plot.names))
	{	name <- plot.names[plot]
		
		from.file <- file.path("out/analysis/estate",plot,"distance/undirected/comparison/geodesic_vs_spatial-database_binned_fixed-range.pdf")
		to.file <- file.path("out/analysis",paste0(name,"__comp-dist.pdf"))
		
		cat("Copying",from.file,">>>>",to.file,"\n")
		
		file.copy(
			from=from.file, 
			to=to.file,
			overwrite=TRUE
		)
	}
}
#collect.dist.comp.plots()
