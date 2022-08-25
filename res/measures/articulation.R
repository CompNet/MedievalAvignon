#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_ARTICULATION <- "articulation"
MEAS_LONG_NAMES[MEAS_ARTICULATION] <- "Articulation point levels"




#############################################################
# Recursively computes articulation points.
#
# An articulation point is a vertex whose removal makes the
# graph disconnected (i.e. it contains several components).
# This function recursively identifies them, i.e. it looks for
# first level articulation points, removes them, then look 
# for articulation points in the resulting components to get
# second level ones, and so on until no more articulation
# point is detected. Edge directions are ignored by igraph here.
#
# g: original graph to process.
# out.folder: main output folder.
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.articulation <- function(g, out.folder, fast)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	# init 
	tlog(2,"Computing articulation points")
	g1 <- g
	level <- 1
	art <- articulation_points(g1)
	art0 <- length(art)
	
	if(art0==0)
		tlog(4,"No articulation point detected")
	else
	{	# repeat until no more articulation point
		while(length(art)>0)
		{	tlog(4,"Level ",level)
			# disconnect the articulation nodes
			g1 <- disconnect.nodes(g1, nodes=art)
			# mark them
			vals <- apply(cbind(rep(level,length(art)),V(g1)$Articulation[art]),1,function(v) min(v,na.rm=TRUE))
			V(g1)[art]$Articulation <- vals
			# proceed with the next level
			art <- articulation_points(g1)
			level <- level + 1
		}
		V(g1)$Articulation[is.na(V(g1)$Articulation)] <- level
		vals <- V(g1)$Articulation
		
		# possibly create folder
		articulation.folder <- file.path(out.folder,g$name,MEAS_ARTICULATION)
		dir.create(path=articulation.folder, showWarnings=FALSE, recursive=TRUE)
		
		# plot distribution
		plot.file <- file.path(articulation.folder,paste0(MEAS_ARTICULATION,"_histo"))
		tlog(4,"Plotting distribution in '",plot.file,"'")
		custom.hist(vals, name=MEAS_LONG_NAMES[MEAS_ARTICULATION], file=plot.file)
		
		# export CSV with articulation points
		tab.file <- file.path(articulation.folder,paste0(MEAS_ARTICULATION,"_values.csv"))
		tlog(4,"Exporting as CSV in '",tab.file,"'")
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",MEAS_ARTICULATION) 
		write.csv(df, file=tab.file, row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		g <- set_vertex_attr(graph=g, name=MEAS_ARTICULATION, value=vals)
		g <- set_graph_attr(graph=g, name=MEAS_ARTICULATION, value=art0)
		stats[MEAS_ARTICULATION, ] <- list(Value=art0, Mean=NA, Stdv=NA)
		
		# plot graph using color for articulation level
		plot.file <- file.path(articulation.folder,paste0(MEAS_ARTICULATION,"_graph"))
		tlog(4,"Plotting graph in '",plot.file,"'")
		#g <- update.node.labels(g, vals, best.low=TRUE)
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=MEAS_ARTICULATION, col.att.cap=MEAS_LONG_NAMES[MEAS_ARTICULATION], file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=MEAS_ARTICULATION, col.att.cap=MEAS_LONG_NAMES[MEAS_ARTICULATION], file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
		
		# export CSV with number of articulation points
		tlog(4,"Updating stat file '",stat.file,"'")
		write.csv(stats, file=stat.file, row.names=TRUE)
	
		# record graph
		graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
		tlog(4,"Updating graph file '",graph.file,"'")
		write.graphml.file(g=g, file=graph.file)
	}
	
	return(g)
}
