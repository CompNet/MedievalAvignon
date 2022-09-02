# Ad hoc scripts to perform various post-analyses.
# 
# Author: Vincent Labatut
#
# source("res/analysis/ad_hoc.R")
###############################################################################




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
# nodes.
#############################################################################################
plot.stats.comparison <- function()
{	tlog(2, "Producing decision figure")
	
	# load overall stats
	tab.file <- file.path(FOLDER_OUT_ANAL_EST, "stats_comparison.csv")
	tlog(4, "Reading table '",tab.file,"'")
	tab <- read.csv(file=tab.file, header=TRUE, row.names=1)
	stats <- tab[,c("estate_nbr", "SpearmanInf_DB", "KendallInf_DB")]
	
	# load street removal stats
	for(graph.type in c("whole_raw","whole_ext"))#,"split_raw","split_ext"))
	{	for(filt.txt in c("","_filtered"))
		{	tab.file <- file.path(FOLDER_OUT_ANAL_EST,graph.type,paste0("flat_minus",filt.txt),"_removed_streets","stats.csv")
			tlog(4, "Reading table '",tab.file,"'")
			tab <- read.csv(file=tab.file, header=TRUE)
			tmp <- tab[,c("estate_nbr", "distance.cor.spearman.infinite", "distance.cor.kendall.infinite")]
			colnames(tmp) <- c("estate_nbr", "SpearmanInf_DB", "KendallInf_DB")
			rownames(tmp) <- paste0(graph.type,"/flat_minus",filt.txt,"_",tab[,"NumberDeletedStreets"],"(",floor(tab[,"LastDeletedStreetLength"]),")")
			stats <- rbind(stats, tmp)
		}
	}
	
	# record as csv file
	tab.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("pareto-plot.csv"))
	write.csv(stats, file=tab.file, row.names=TRUE)
	
	# create plots
	for(corr.txt in c("spearman","kendall"))
	{	plot.file <- file.path(FOLDER_OUT_ANAL_EST, paste0("pareto-plot_",corr.txt))
		if(corr.txt=="")
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
				ylab="Distance correlation"
			)
			# draw Pareto front
			df <- data.frame(x=stats[,"estate_nbr"], y=vals)
			pref <- high(x)*high(y)
			sky <- psel(df=df, pref=pref)
			plot_front(df=df, pref=pref, col="RED")
			#points(df[,"x"], df[,"y"], lwd=3)
			# add point names
			text(
				x=stats[,"estate_nbr"],
				y=vals,
				labels=rownames(stats),
				cex=0.1
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
extract.vertex.attributes <- function(graph.names, folder, attributes)
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
		write.csv(df, file=tab.file, row.names=FALSE)
	}
}
#extract.vertex.attributes(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST, attributes=c(COL_LOC_X,COL_LOC_Y,COL_LOC_INTER_X,COL_LOC_INTER_Y,COL_LOC_INTER))




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
