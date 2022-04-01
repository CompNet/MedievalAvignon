#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_DISTANCE <- "distance"
MEAS_LONG_NAMES[MEAS_DISTANCE] <- "Distance"




#############################################################
# Computes average distances and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.distance <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	
	###### deal with graph distance
	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing distances: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_DISTANCE,"_",mode)
		distance.folder <- file.path(out.folder,g$name,MEAS_DISTANCE)
		dir.create(path=distance.folder, showWarnings=FALSE, recursive=TRUE)
		
		# distance distribution
		vals <- distances(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
		flat.vals <- vals[upper.tri(vals)]
		custom.hist(vals=flat.vals, paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_DISTANCE]), file=file.path(distance.folder,paste0(fname,"_histo")))
		
		# average distance distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name=paste("Average",MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_DISTANCE]), file=file.path(distance.folder,paste0(fname,"_avg_histo")))
		{	# export CSV with average distance
			df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), avg.vals)
			colnames(df) <- c("Id","Name",paste0(fname,"_avg")) 
			write.csv(df, file=file.path(distance.folder,paste0(fname,"_avg_values.csv")), row.names=FALSE)
			
			# add results to the graph (as attributes) and stats table
			g <- set_vertex_attr(graph=g, name=paste0(fname,"_avg"), value=avg.vals)
			g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(flat.vals[!is.infinite(flat.vals)]))
			g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(flat.vals[!is.infinite(flat.vals)]))
			stats[fname, ] <- list(Value=NA, Mean=mean(vals[!is.infinite(vals)]), Stdv=sd(vals[!is.infinite(vals)]))
			
			# plot graph using color for average distance
			g <- update.node.labels(g, avg.vals)
			custom.gplot(g=g, col.att=paste0(fname,"_avg"), file=file.path(distance.folder,paste0(fname,"_avg_graph")), size.att=2)
			#custom.gplot(g=g, col.att=paste0(fname,"_avg"))
			g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2
			custom.gplot(g=g1, col.att=paste0(fname,"_avg"), file=file.path(distance.folder,paste0(fname,"_avg_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y))
		}
		
		# for each node, plot graph using color for distance
		mode.folder <- file.path(distance.folder,mode)
		dir.create(path=mode.folder, showWarnings=FALSE, recursive=TRUE)
		for(n in 1:gorder(g))
		{	id <- vertex_attr(g, ND_NAME, n)
			nname <- get.names(g, n)
			nname <- trimws(gsub("?", "", nname, fixed=TRUE))
			
			# only for significant nodes
			if(igraph::degree(g, v=n, mode="all")<3)
				tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as its degree is <3")
			else
			{	g <- set_vertex_attr(graph=g, name=fname, value=vals[n,])
				if(all(is.infinite(vals[n,-n])))
					tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are infinite")
				else
				{	tlog(4,"Plotting graph for node #",id," (",nname, ", ",n,"/",gorder(g),")")
					g <- update.node.labels(g, vals[n,])
					shrt.nm <- substr(nname,1,30)		# to avoid long file names
					id.cln <- gsub(":", "-", nname, fixed=TRUE)
					custom.gplot(g=g, col.att=fname, v.hl=n, file=file.path(mode.folder,paste0("n",id.cln,"_",shrt.nm )), size.att=2)
					#custom.gplot(g=g, col.att=fname, v.hl=n)
				}
				g <- delete_vertex_attr(graph=g, name=fname)
			}
		}
	}
	
	# export CSV with average distance
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	
	###### compare graph and spatial distances
	tlog(2,"Comparing graph and spatial distances")
	sdists <- c("reliable","estimate")	# only positions from the DB vs. all positions including estimates
	fname <- paste0(MEAS_DISTANCE,"_undirected")
	distance.folder <- file.path(out.folder,g$name,MEAS_DISTANCE)
	
	# init correlation table
	cor.tab <- matrix(NA,nrow=2,ncol=3)	
	cor.tab <- data.frame(cor.tab)
	cor.tab <- cbind(c("Reliable","Estimate"), cor.tab)
	colnames(cor.tab) <- c("Coordinates", "Pearson", "Spearman", "Kendall")
	rownames(cor.tab) <- sdists
	
	# compute spatial distances
	xlab <- c(reliable="Spatial distance (as defined)", estimate="Spatial distance (as estimated)")
	for(sdist in sdists)
	{	tlog(6,"Computing spatial distance (",sdist,")")
		if(sdist=="reliable")
			coords <- cbind(vertex_attr(g, name=COL_LOC_HYP_LON), vertex_attr(g, name=COL_LOC_HYP_LAT))
		else
			coords <- cbind(V(g)$x, V(g)$y)
		idx <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
		rem <- which(is.na(coords[,1]) | is.na(coords[,2]))
		svals <- as.matrix(dist(x=coords[idx,], method="euclidean", diag=TRUE, upper=TRUE))
		svals <- svals[upper.tri(svals)]
		
		# compute distribution
		plot.file <- file.path(distance.folder,paste0(fname,"_histo_spatial_",sdist))
		tlog(8,"Plotting in \"",plot.file,"\"")
		custom.hist(vals=svals, xlab[sdist], file=plot.file)
		
		# compute undirected graph distance
		tlog(8,"Computing undirected graph distance")
		if(length(rem)>0) 
			gt <- delete_vertices(g,rem)
		else
			gt <- g
		gvals <- distances(graph=gt, mode="all")
		gvals <- gvals[upper.tri(gvals)]
		idx <- !is.infinite(gvals)
		gvals <- gvals[idx]
		svals <- svals[idx]
		
		# compute correlations
		tlog(8,"Computing correlation between graph and spatial distances")
		cor.tab[sdist,"Pearson"] <- cor(x=gvals, y=svals, method="pearson")
		cor.tab[sdist,"Spearman"] <- cor(x=gvals, y=svals, method="spearman")
		cor.tab[sdist,"Kendall"] <- cor(x=gvals, y=svals, method="kendall")
		
		# plot the spatial distance as a function of the graph-based one
		plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial_",sdist))
		pdf(paste0(plot.file,".pdf"))
			plot(
				x=gvals, y=svals, 
				xlab="Undirected graph distance", ylab=xlab[sdist],
				#log="xy", 
				las=1, col="RED",
				#xlim=c(1,max(deg.vals)*1.1)
			)
			# mean
			avg.dist <- sapply(min(gvals):max(gvals), function(deg) mean(svals[gvals==deg]))
			lines(	
				x=min(gvals):max(gvals), avg.dist,
				col="BLACK"
			)
		dev.off()
	}
	
	# record correlations
	tab.file <- file.path(distance.folder, paste0(fname,"_correlations.csv"))
	write.csv(cor.tab, file=tab.file, row.names=FALSE)

	
	###### finalize
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
