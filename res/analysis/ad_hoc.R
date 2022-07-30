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
		tab.file <- file.path(folder,paste0(gsub("/", "__", graph.names[i]),".csv"))
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
		colnames(el) <- c("Vertex1","Vertex2","EdgeType")
		
		# record table as CSV
		tab.file <- file.path(folder,paste0(gsub("/", "__", graph.names[i]),".csv"))
		write.csv(el, file=tab.file, row.names=FALSE)
	}
}
