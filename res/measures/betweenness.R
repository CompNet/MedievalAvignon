#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_BETWEENNESS <- "betweenness"
MEAS_LONG_NAMES[MEAS_BETWEENNESS] <- "Betweenness"




#############################################################
# Computes betweenness and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.betweenness <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing betweenness: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_BETWEENNESS,"_",mode)
		betweenness.folder <- file.path(out.folder,g$name,MEAS_BETWEENNESS)
		dir.create(path=betweenness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# betweenness distribution
		vals <- betweenness(graph=g, normalized=FALSE, directed=mode=="directed")
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_BETWEENNESS]), file=file.path(betweenness.folder,paste0(fname,"_histo")))
		
		# export CSV with betweenness
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(betweenness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		mval <- mean(vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mval)
		sdval <- sd(vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sdval)
		centr <- centr_betw(graph=g, directed=mode=="directed", normalized=TRUE)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_centr"), value=centr)
		stats[fname, ] <- list(Value=centr, Mean=mval, Stdv=sdval)
		
		# plot graph using color for betweenness
		#g <- update.node.labels(g, vals)
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, file=file.path(betweenness.folder,paste0(fname,"_graph_lambert")), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, file=file.path(betweenness.folder,paste0(fname,"_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1)
	}
	
	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
