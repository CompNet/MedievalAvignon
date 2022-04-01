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
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.closeness <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
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
		vals <- rep(NA, vcount(g))
		vals[giant.comp.nodes] <- closeness(graph=g.comp, normalized=TRUE, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
		vals[is.nan(vals)] <- NA
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_CLOSENESS]), file=file.path(closeness.folder,paste0(fname,"_histo")))
		
		# export CSV with closeness
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(closeness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals,na.rm=TRUE))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals,na.rm=TRUE))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals,na.rm=TRUE), Stdv=sd(vals,na.rm=TRUE))
		
		# plot graph using color for closeness
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(closeness.folder,paste0(fname,"_graph")), size.att=2)
		#custom.gplot(g=g, col.att=fname)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2
		custom.gplot(g=g1, col.att=fname, file=file.path(closeness.folder,paste0(fname,"_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y))
		if(all(is.na(vals)))
			tlog(4,"WARNING: all values are NA, so no color in the plot")
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Computes harmonic closeness and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.harmonic.closeness <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing harmonic closeness: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_CLOSENESS_HARMO,"_",mode)
		closeness.folder <- file.path(out.folder,g$name,MEAS_CLOSENESS_HARMO)
		dir.create(path=closeness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# harmonic closeness distribution: only giant component
		vals <- rep(NA, vcount(g))
		vals <- harmonic_centrality(x=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)/(length(which(igraph::degree(g)>0))-1)
		vals[is.nan(vals)] <- NA
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_CLOSENESS_HARMO]), file=file.path(closeness.folder,paste0(fname,"_histo")))
		
		# export CSV with harmonic closeness
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(closeness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals,na.rm=TRUE))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals,na.rm=TRUE))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals,na.rm=TRUE), Stdv=sd(vals,na.rm=TRUE))
		
		# plot graph using color for harmonic closeness
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(closeness.folder,paste0(fname,"_graph")), size.att=2)
		#custom.gplot(g=g, col.att=fname)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2
		custom.gplot(g=g1, col.att=fname, file=file.path(closeness.folder,paste0(fname,"_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y))
		if(all(is.na(vals)))
			tlog(4,"WARNING: all values are NA, so no color in the plot")
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
