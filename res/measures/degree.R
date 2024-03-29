#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_DEGREE <- "degree"
MEAS_LONG_NAMES[MEAS_DEGREE] <- "Degree"




#############################################################
# Computes the degree and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.degree <- function(g, out.folder, fast)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing degree: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_DEGREE,"_",mode)
		degree.folder <- file.path(out.folder, g$name, MEAS_DEGREE)
		dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
		
		# degree distribution
		plot.file <- file.path(degree.folder,paste0(fname,"_histo"))
		tlog(4,"Plotting distribution in '",plot.file,"'")
		vals <- igraph::degree(g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, loops=TRUE, normalized=FALSE)
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_DEGREE]), file=plot.file)
			
		# export CSV with degree
		tab.file <- file.path(degree.folder,paste0(fname,"_values.csv"))
		tlog(4,"Exporting as CSV in '",tab.file,"'")
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=tab.file, row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		mval <- mean(vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mval)
		sdval <- sd(vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sdval)
		centr <- centr_degree(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, loops=TRUE, normalized=TRUE)$centralization
		stats[fname, ] <- list(Value=centr, Mean=mval, Stdv=sdval)
		
		# plot graph using color for degree
		plot.file <- file.path(degree.folder,paste0(fname,"_graph"))
		tlog(4,"Plotting graph in '",plot.file,"'")
		#g <- update.node.labels(g, vals)
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1); g1 <- rescale.coordinates(g1)
		custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_DEGREE], file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_DEGREE], file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
	}
	
	# export CSV with average degree
	tlog(2,"Updating stat file '",stat.file,"'")
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	tlog(2,"Updating graph file '",graph.file,"'")
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
