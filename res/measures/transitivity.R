#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_TRANSITIVITY <- "transitivity"
MEAS_LONG_NAMES[MEAS_TRANSITIVITY] <- "Transitivity"




#############################################################
# Computes transitivity and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.transitivity <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	tlog(2,"Computing transitivity")
	# possibly create folder
	fname <- MEAS_TRANSITIVITY
	transitivity.folder <- file.path(out.folder, g$name, MEAS_TRANSITIVITY)
	dir.create(path=transitivity.folder, showWarnings=FALSE, recursive=TRUE)
	
	# transitivity distribution
	vals <- transitivity(graph=g, type="localundirected", isolates="zero")
	custom.hist(vals, name=paste("Local",MEAS_LONG_NAMES[MEAS_TRANSITIVITY]), file=file.path(transitivity.folder,paste0(fname,"_histo")))
	global <- transitivity(graph=g, type="globalundirected", isolates="zero")
	
	# export CSV with transitivity
	df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
	colnames(df) <- c("Id","Name",fname) 
	write.csv(df, file=file.path(transitivity.folder,paste0(fname,"_values.csv")), row.names=FALSE)
	
	# add results to the graph (as attributes) and stats table
	g <- set_vertex_attr(graph=g, name=fname, value=vals)
	g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
	g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
	g <- set_graph_attr(graph=g, name=paste0(fname,"_global"), value=global)
	stats[paste0(fname,"_local"), ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	stats[paste0(fname,"global"), ] <- list(Value=global, Mean=NA, Stdv=NA)
	
	# plot graph using color for transitivity
	g <- update.node.labels(g, vals)
	custom.gplot(g=g, col.att=fname, file=file.path(transitivity.folder,paste0(fname,"_graph")))
	#custom.gplot(g=g, col.att=fname)
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
