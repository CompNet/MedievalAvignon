#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_EIGENCENTR <- "eigencentrality"
MEAS_LONG_NAMES[MEAS_EIGENCENTR] <- "Eigen-centrality"




#############################################################
# Computes Eigencentrality and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.eigencentrality <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing Eigencentrality: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_EIGENCENTR,"_",mode)
		eigen.folder <- file.path(out.folder, g$name, MEAS_EIGENCENTR)
		dir.create(path=eigen.folder, showWarnings=FALSE, recursive=TRUE)
		
		# Eigencentrality distribution
		if(mode==MEAS_MODE_DIR && is_dag(g))
			vals <- rep(0, gorder(g))
		else
			vals <- eigen_centrality(graph=g, scale=FALSE, directed=mode==MEAS_MODE_DIR)$vector
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_EIGENCENTR]), file=file.path(eigen.folder,paste0(fname,"_histo")))
		
		# export CSV with Eigencentrality
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(eigen.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for Eigencentrality
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(eigen.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname)
	}
	
	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
