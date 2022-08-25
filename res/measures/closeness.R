#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_CLOSENESS <- "closeness"
MEAS_CLOSENESS_HARMO <- "closeness_harmo"
MEAS_LONG_NAMES[MEAS_CLOSENESS] <- "Closeness"
MEAS_LONG_NAMES[MEAS_CLOSENESS_HARMO] <- "Harmonic Closeness"




#############################################################
# Computes closeness and generates plots and CSV files.
#
# If the graph has several components, the closeness is computed
# only for the largest one.
#
# g: original graph to process.
# out.folder: main output folder.
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.closeness <- function(g, out.folder, fast)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing closeness: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_CLOSENESS,"_",mode)
		closeness.folder <- file.path(out.folder,g$name,MEAS_CLOSENESS)
		dir.create(path=closeness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# retrieve giant component, do not compute measure for the rest of the graph
		components <- components(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "weak" else "strong")
		giant.comp.id <- which.max(components$csize)
		giant.comp.nodes <- which(components$membership==giant.comp.id)
		g.comp <- induced_subgraph(graph=g, giant.comp.nodes)
		
		# closeness distribution: only giant component
		plot.file <- file.path(closeness.folder,paste0(fname,"_histo"))
		tlog(4,"Plotting distribution in '",plot.file,"'")
		vals <- rep(NA, vcount(g))
		vals[giant.comp.nodes] <- closeness(graph=g.comp, normalized=TRUE, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
		vals[is.nan(vals)] <- NA
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_CLOSENESS]), file=plot.file)
		
		# export CSV with closeness
		tab.file <- file.path(closeness.folder,paste0(fname,"_values.csv"))
		tlog(4,"Exporting as CSV in '",tab.file,"'")
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=tab.file, row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		mval <- mean(vals,na.rm=TRUE)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mval)
		sdval <- sd(vals,na.rm=TRUE)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sdval)
		centr <- centr_clo(graph=g.comp, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, normalized=TRUE)$centralization
		g <- set_graph_attr(graph=g, name=paste0(fname,"_centr"), value=centr)
		stats[fname, ] <- list(Value=centr, Mean=mval, Stdv=sdval)
		
		# plot graph using color for closeness
		plot.file <- file.path(closeness.folder,paste0(fname,"_graph"))
		tlog(4,"Plotting graph in '",plot.file,"'")
		#g <- update.node.labels(g, vals)
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_CLOSENESS], file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_CLOSENESS], file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
		if(all(is.na(vals)))
			tlog(4,"WARNING: all values are NA, so no color in the plot")
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




#############################################################
# Computes harmonic closeness and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.harmonic.closeness <- function(g, out.folder, fast)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing harmonic closeness: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_CLOSENESS_HARMO,"_",mode)
		closeness.folder <- file.path(out.folder,g$name,MEAS_CLOSENESS_HARMO)
		dir.create(path=closeness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# harmonic closeness distribution: only giant component
		plot.file <- file.path(closeness.folder,paste0(fname,"_histo"))
		tlog(4,"Plotting distribution in '",plot.file,"'")
		vals <- rep(NA, vcount(g))
		vals <- harmonic_centrality(x=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)/(length(which(igraph::degree(g)>0))-1)
		vals[is.nan(vals)] <- NA
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_CLOSENESS_HARMO]), file=plot.file)
		
		# export CSV with harmonic closeness
		tab.file <- file.path(closeness.folder,paste0(fname,"_values.csv"))
		tlog(4,"Exporting as CSV in '",tab.file,"'")
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=tab.file, row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals,na.rm=TRUE))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals,na.rm=TRUE))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals,na.rm=TRUE), Stdv=sd(vals,na.rm=TRUE))
		
		# plot graph using color for harmonic closeness
		plot.file <- file.path(closeness.folder,paste0(fname,"_graph"))
		tlog(4,"Plotting graph in '",plot.file,"'")
		g <- update.node.labels(g, vals)
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_CLOSENESS_HARMO], file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_CLOSENESS_HARMO], file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
		if(all(is.na(vals)))
			tlog(4,"WARNING: all values are NA, so no color in the plot")
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
