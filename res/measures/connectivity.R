#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_CONNECTIVITY <- "connectivity"
MEAS_LONG_NAMES[MEAS_CONNECTIVITY] <- "Connectivity"




#############################################################
# Computes vertex connectivity and generates plots and CSV files.
#
# The connectivity of two vertices is the minimal number of vertices
# that should be removed in order to make the vertices disconnected.
#
# g: original graph to process.
# out.folder: main output folder.
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.connectivity <- function(g, out.folder, fast)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	for(mode in modes)
	{	tlog(2,"Computing vertex connectivity: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_CONNECTIVITY,"_",mode)
		connectivity.folder <- file.path(out.folder,g$name,MEAS_CONNECTIVITY)
		dir.create(path=connectivity.folder, showWarnings=FALSE, recursive=TRUE)
		
		# compute connectivity
		vals <- matrix(NA, nrow=gorder(g), ncol=gorder(g))
		g2 <- if(mode==MEAS_MODE_DIR) g else as.undirected(g)
		for(n in 1:(gorder(g2)-1))
		{	vals[n,n] <- 0
			neigh <- neighbors(graph=g2, v=n)
			for(n2 in (n+1):gorder(g2))
			{	if(n2 %in% neigh)
					tmp <- 1	# direct connexion: should be Inf, I guess? but igraph throws an error
				else
					tmp <- vertex_connectivity(
							graph=g2,
							source=n, target=n2)
				vals[n,n2] <- tmp
				vals[n2,n] <- vals[n,n2]
			}
		}
		plot.file <- file.path(connectivity.folder,paste0(fname,"_histo"))
		tlog(4,"Plotting distribution in '",plot.file,"'")
		vals[gorder(g),gorder(g)] <- 0
		flat.vals <- vals[upper.tri(vals)]
		custom.hist(vals=flat.vals, name=MEAS_LONG_NAMES[MEAS_CONNECTIVITY], file=plot.file)
		# connectivity distribution
		plot.file <- file.path(connectivity.folder,paste0(fname,"_avg_histo"))
		tlog(4,"Plotting average distribution in '",plot.file,"'")
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name=MEAS_LONG_NAMES[MEAS_CONNECTIVITY], file=plot.file)
		{	# export CSV with average connectivity
			tab.file <- file.path(connectivity.folder,paste0(fname,"_avg_values.csv"))
			tlog(4,"Exporting average values as CSV in '",tab.file,"'")
			df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), avg.vals)
			colnames(df) <- c("Id","Name",fname) 
			write.csv(df, file=tab.file, row.names=FALSE)
			
			# add results to the graph (as attributes) and record
			g <- set_vertex_attr(graph=g, name=paste0(fname,"_avg"), value=avg.vals)
			g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
			g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
			stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
			
			# plot graph using color for average connectivity
			plot.file <- file.path(connectivity.folder,paste0(fname,"_avg_graph"))
			tlog(4,"Plotting graph in '",plot.file,"'")
			#g <- update.node.labels(g, avg.vals)
			V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
			g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1); g1 <- rescale.coordinates(g1)
			custom.gplot(g=g1, col.att=paste0(fname,"_avg"), col.att.cap=paste0("Average ",MEAS_LONG_NAMES[MEAS_CONNECTIVITY]), file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
			g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=paste0(fname,"_avg"), col.att.cap=paste0("Average ",MEAS_LONG_NAMES[MEAS_CONNECTIVITY]), file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
		}
		
		# for each node, plot graph using color for connectivity
		if(!fast)
		{	mode.folder <- file.path(connectivity.folder,mode)
			dir.create(path=mode.folder, showWarnings=FALSE, recursive=TRUE)
			plot.folder <- file.path(mode.folder,"lambert")
			dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
			plot.folder <- file.path(mode.folder,"kk")
			dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
			for(n in 1:gorder(g))
			{	id <- vertex_attr(g, COL_LOC_ID, n)
				nname <- get.names(g, n)
				nname <- trimws(gsub("?", "", nname, fixed=TRUE))
				
				# only for significant nodes
				if(degree(g, v=n, mode="all")<3)
					tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as its degree is <3")
				else
				{	g <- set_vertex_attr(graph=g, name=fname, value=vals[n,])
					if(all(vals[n,]==0))
						tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are zero")
					else
					{	g <- update.node.labels(g, vals[n,])
						shrt.nm <- substr(nname,1,30)		# to avoid long file names
						id.cln <- gsub(":", "-", id, fixed=TRUE)
						id.cln <- gsub("/", "_", id.cln, fixed=TRUE)
						V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
						tlog(4,"Plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),") in '",file.path(mode.folder,"xxxx",paste0(id.cln,"_",shrt.nm)),"'")
						g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1); g1 <- rescale.coordinates(g1)
						custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_CONNECTIVITY], v.hl=n, file=file.path(mode.folder,"lambert",paste0(id.cln,"_",shrt.nm)), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
						g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
						custom.gplot(g=g1, col.att=fname, col.att.cap=MEAS_LONG_NAMES[MEAS_CONNECTIVITY], v.hl=n, file=file.path(mode.folder,"kk",paste0(id.cln,"_",shrt.nm)), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
					}
					g <- delete_vertex_attr(graph=g, name=fname)
				}
			}
		}
	}
	
	# export CSV with average connectivity
	tlog(2,"Updating stat file '",stat.file,"'")
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	tlog(2,"Updating graph file '",graph.file,"'")
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
