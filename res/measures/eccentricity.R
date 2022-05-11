#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_ECCENTRICITY <- "eccentricity"
MEAS_DIAMETER <- "diameter"
MEAS_RADIUS <- "radius"
MEAS_LONG_NAMES[MEAS_ECCENTRICITY] <- "Eccentricity"
MEAS_LONG_NAMES[MEAS_DIAMETER] <- "Diameter"
MEAS_LONG_NAMES[MEAS_DIAMETER] <- "RADIUS"



#############################################################
# Computes the diameter, the corresponding paths, and plots them.
# Same thing for radius and eccentricity.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.eccentricity <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	# compute diameter
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	for(mode in modes)
	{	tlog(2,"Computing diameter: mode=",mode)
		diam <- diameter(g, directed=mode==MEAS_MODE_DIR)						# get the network diameter
		tlog(4,"Diameter: ",diam)
		dd <- distances(graph=g, mode=if(mode==MEAS_MODE_DIR) "in" else "all")	# compute all inter-node distances
		idx <- which(dd==diam, arr.ind=TRUE)									# retrieve pairs of nodes matching the diameter
		if(mode==MEAS_MODE_UNDIR)
			idx <- idx[idx[,1]<idx[,2],,drop=FALSE]								# filter (each pair appears twice due to symmetric matrix)
		tlog(4,"Number of diameter paths: ",nrow(idx))
		
		# possibly create folder
		fname <- paste0(MEAS_DIAMETER,"_",mode)
		diameter.folder <- file.path(out.folder, g$name, MEAS_DIAMETER, mode)
		dir.create(path=diameter.folder, showWarnings=FALSE, recursive=TRUE)
		dir.create(path=file.path(diameter.folder,"lambert"), showWarnings=FALSE, recursive=TRUE)
		dir.create(path=file.path(diameter.folder,"kk"), showWarnings=FALSE, recursive=TRUE)
		
		# plot diameter
		diam.paths <- future_lapply(1:nrow(idx), function(r) 
#			if(startsWith(g$name,GR_EST_ESTATE_LEVEL))
#				# too long to find all the paths...
				shortest_paths(graph=g, from=idx[r,1], to=idx[r,2], mode=if(mode==MEAS_MODE_DIR) "in" else "all")$vpath
#			else
#				# looking for all the paths
#				all_shortest_paths(graph=g, from=idx[r,1], to=idx[r,2], mode=if(mode==MEAS_MODE_DIR) "in" else "all")$res
		)
		tlog(4,"Found ",length(diam.paths)," distinct diameters, plotting them")
		for(pp in 1:length(diam.paths))
		{	tlog(6,"Plotting diameter path ",pp,"/",length(diam.paths))
			#V(g)$label <- rep(NA, gorder(g))
			V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
			g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, paths=diam.paths[[pp]], file=file.path(diameter.folder,"lambert",paste0(MEAS_DIAMETER,"_",mode,"_graph_",pp)), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
			g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, paths=diam.paths[[pp]], file=file.path(diameter.folder,"kk",paste0(MEAS_DIAMETER,"_",mode,"_graph_",pp)), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
			
			if(length(diam.paths[[pp]])<=20)
			{	tlog(8,"Plotting the ",length(diam.paths[[pp]])," variants of this diameter")			
				q <- 1
				for(p in 1:length(diam.paths[[pp]]))
				{	tlog(10,"Plotting variant ",p,"/",length(diam.paths[[pp]]))
					if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
					{	#V(g)$label <- rep(NA,gorder(g))
						#vstart <- diam.paths[[pp]][[p]][1]
						#V(g)[vstart]$label <- get.names(g, vstart) 
						#vend <- diam.paths[[pp]][[p]][length(diam.paths[[pp]][[p]])]
						#V(g)[vend]$label <- get.names(g, vend) 
						V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
						g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
						custom.gplot(g=g1, paths=diam.paths[[pp]][[p]], file=file.path(diameter.folder,"lambert",paste0(MEAS_DIAMETER,"_",mode,"_graph_",pp,"_",q)), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
						g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
						custom.gplot(g=g1, paths=diam.paths[[pp]][[p]], file=file.path(diameter.folder,"kk",paste0(MEAS_DIAMETER,"_",mode,"_graph_",pp,"_",q)), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
						
						q <- q + 1
					}
				}
			}
			else
				tlog(8,"Too many variants of the diameter, not plotting them")			
		}
		
		# add value to graph and table
		g <- set_graph_attr(graph=g, name=fname, value=diam)
		stats[fname, ] <- list(Value=diam, Mean=NA, Stdv=NA)
	}
	
	# compute eccentricity and radius
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		
		# compute eccentricity
		tlog(2,"Computing eccentricity: mode=",mode)
		vals <- eccentricity(g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
		
		# possibly create folder
		fname <- paste0(MEAS_ECCENTRICITY,"_",mode)
		eccentricity.folder <- file.path(out.folder, g$name, MEAS_ECCENTRICITY)
		dir.create(path=eccentricity.folder, showWarnings=FALSE, recursive=TRUE)
		
		# plot distribution
		custom.hist(vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_ECCENTRICITY]), file=file.path(eccentricity.folder,paste0(fname,"_histo")))
		
		# export CSV with eccentricity
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(eccentricity.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add eccentricity (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for eccentricity
		#g <- update.node.labels(g, vals)
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, file=file.path(eccentricity.folder,paste0(fname,"_graph_lambert")), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, file=file.path(eccentricity.folder,paste0(fname,"_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
		
		# compute radius
		tlog(2,"Computing radius: mode=",mode)
		rad <- radius(g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
		#rad <- min(vals[vals>0])
		tlog(4,"Radius=",rad)
		
		# add radius to the graph (as attributes) and stats table
		fname <- paste0(MEAS_RADIUS,"_",mode)
		g <- set_graph_attr(graph=g, name=fname, value=rad)
		stats[fname, ] <- list(Value=rad, Mean=NA, Stdv=NA)
	}

	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
