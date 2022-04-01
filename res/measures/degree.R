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
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.degree <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing degree: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_DEGREE,"_",mode)
		degree.folder <- file.path(out.folder, g$name, MEAS_DEGREE)
		dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
		
		# degree distribution
		vals <- igraph::degree(g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_DEGREE]), file=file.path(degree.folder,paste0(fname,"_histo")))
			
		# export CSV with degree
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(degree.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for degree
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(degree.folder,paste0(fname,"_graph")), size.att=2)
		#custom.gplot(g=g, col.att=fname)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2
		custom.gplot(g=g1, col.att=fname, file=file.path(degree.folder,paste0(fname,"_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y))
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
