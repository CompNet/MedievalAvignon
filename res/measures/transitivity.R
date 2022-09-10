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
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.transitivity <- function(g, out.folder, fast)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	tlog(2,"Computing transitivity")
	# possibly create folder
	fname <- MEAS_TRANSITIVITY
	transitivity.folder <- file.path(out.folder, g$name, MEAS_TRANSITIVITY)
	dir.create(path=transitivity.folder, showWarnings=FALSE, recursive=TRUE)
	
	# transitivity distribution
	plot.file <- file.path(transitivity.folder,paste0(fname,"_histo"))
	tlog(4,"Plotting distribution in '",plot.file,"'")
	vals <- transitivity(graph=g, type="localundirected", isolates="zero")
	custom.hist(vals, name=paste("Local",MEAS_LONG_NAMES[MEAS_TRANSITIVITY]), file=plot.file)
	global <- transitivity(graph=g, type="globalundirected", isolates="zero")
	
	# export CSV with transitivity
	tab.file <- file.path(transitivity.folder,paste0(fname,"_values.csv"))
	tlog(4,"Exporting as CSV in '",tab.file,"'")
	df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
	colnames(df) <- c("Id","Name",fname) 
	write.csv(df, file=tab.file, row.names=FALSE)
	
	# add results to the graph (as attributes) and stats table
	g <- set_vertex_attr(graph=g, name=fname, value=vals)
	g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
	g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
	g <- set_graph_attr(graph=g, name=paste0(fname,"_global"), value=global)
	stats[paste0(fname,"_local"), ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	stats[paste0(fname,"global"), ] <- list(Value=global, Mean=NA, Stdv=NA)
	
	# plot graph using color for transitivity
	plot.file <- file.path(transitivity.folder,paste0(fname,"_graph"))
	tlog(4,"Plotting graph in '",plot.file,"'")
	#g <- update.node.labels(g, vals)
	V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
	g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1); g1 <- rescale.coordinates(g1)
	custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_TRANSITIVITY], file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
	g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
	custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_TRANSITIVITY], file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
	
	# export CSV with average degree
	tlog(2,"Updating stat file '",stat.file,"'")
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	tlog(2,"Updating graph file '",graph.file,"'")
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
