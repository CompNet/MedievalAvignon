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
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	modes <- c(MEAS_MODE_UNDIR)
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
		if(length(flat.vals)>2)
			custom.hist(vals=flat.vals, paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_DISTANCE]), file=file.path(distance.folder,paste0(fname,"_histo")))
		
		# average distance distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		if(length(avg.vals)>2)
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
			#g <- update.node.labels(g, avg.vals)
			V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
			g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=paste0(fname,"_avg"), file=file.path(distance.folder,paste0(fname,"_avg_graph_lambert")), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
			g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=paste0(fname,"_avg"), file=file.path(distance.folder,paste0(fname,"_avg_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
		}
		
		# for each node, plot graph using color for distance
		mode.folder <- file.path(distance.folder,mode)
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
					id.cln <- gsub(":", "-", id, fixed=TRUE)
					id.cln <- gsub("/", "_", id.cln, fixed=TRUE)
					V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
					g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
					custom.gplot(g=g1, col.att=fname, v.hl=n, file=file.path(mode.folder,"lambert",paste0(id.cln,"_",shrt.nm )), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
					g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
					custom.gplot(g=g1, col.att=fname, v.hl=n, file=file.path(mode.folder,"kk",paste0(id.cln,"_",shrt.nm )), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
				}
				g <- delete_vertex_attr(graph=g, name=fname)
			}
		}
	}
	
	# export CSV with average distance
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	
	###### compare graph and spatial distances
	tlog(2,"Comparing graph and spatial distances")
	sdists <- c("database","interpolation")			# only positions from the DB vs. all positions including estimates
	fname <- paste0(MEAS_DISTANCE,"_undirected")
	distance.folder <- file.path(out.folder,g$name,MEAS_DISTANCE)
	
	# init correlation table
	cor.tab <- matrix(NA,nrow=2,ncol=6)	
	cor.tab <- data.frame(cor.tab)
	cor.tab <- cbind(c("Database","Interpolation"), cor.tab)
	colnames(cor.tab) <- c("Coordinates", "PearsonCoef", "PearsonPval", "SpearmanCoef", "SpearmanPval", "KendallCoef", "KendallPval")
	rownames(cor.tab) <- sdists
	
	# compute spatial distances
	xlab <- c(database="Spatial distance (database)", interpolation="Spatial distance (interpolation)")
	for(sdist in sdists)
	{	tlog(6,"Computing spatial distance (",sdist,")")
		if(sdist=="database")
			coords <- cbind(vertex_attr(g, name=COL_LOC_X), vertex_attr(g, name=COL_LOC_Y))
		else
			coords <- cbind(V(g)$x, V(g)$y)
		idx <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
		rem <- which(is.na(coords[,1]) | is.na(coords[,2]))
		svals <- as.matrix(dist(x=coords[idx,], method="euclidean", diag=TRUE, upper=TRUE))
		svals.avg <- apply(X=svals, MARGIN=1, FUN=function(v) mean(v[!is.na(v)]))
		svals <- svals[upper.tri(svals)]
		
		# compute distribution
		plot.file <- file.path(distance.folder,paste0(fname,"_histo_spatial_",sdist))
		tlog(8,"Plotting in \"",plot.file,"\"")
		if(length(svals)>2)
			custom.hist(vals=svals, xlab[sdist], file=plot.file)
		
		# plot graph using color for average distance
		satt <- "SpatialDist_avg"
		avg.vals <- rep(NA,gorder(g))
		avg.vals[idx] <- svals.avg
		g <- set_vertex_attr(graph=g, name=satt, value=avg.vals)
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=satt, file=file.path(distance.folder,paste0("distance_spatial_",sdist,"_avg_graph_lambert")), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=satt, file=file.path(distance.folder,paste0("distance_spatial_",sdist,"_avg_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
		
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
		tlog(8,"Computing correlation between graph and spatial distances (",length(gvals)," values vs. ",length(svals)," values)")
		if(length(gvals)<3)
			tlog(10,"WARNING: not enough values to compute correlation or produce plots")
		else
		{	tmp <- cor.test(x=gvals, y=svals, method="pearson")
			cor.tab[sdist,"PearsonCoef"] <- tmp$estimate
			cor.tab[sdist,"PearsonPval"] <- tmp$p.value
			tmp <- cor.test(x=gvals, y=svals, method="spearman")
			cor.tab[sdist,"SpearmanCoef"] <- tmp$estimate
			cor.tab[sdist,"SpearmanPval"] <- tmp$p.value
			tmp <- cor.test(x=gvals, y=svals, method="kendall")
			cor.tab[sdist,"KendallCoef"] <- tmp$estimate
			cor.tab[sdist,"KendallPval"] <- tmp$p.value
			# NOTE: null hypothesis=zero correlation >> small p means this hypothesis is rejected
			
			# plot the spatial distance as a function of the graph-based one
			plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial_",sdist))
			for(fformat in FORMAT)
			{	if(fformat=="pdf")
					pdf(paste0(plot.file,".pdf"))
				else if(fformat=="png")
					png(paste0(plot.file,".png"))
					plot(
						x=gvals, y=svals, 
						xlab="Undirected graph distance", ylab=xlab[sdist],
						#log="xy", 
						las=1, col=make.color.transparent("RED",75)
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
			
			# same using degree for colors
			meas <- MEAS_DEGREE
			vals <- igraph::degree(graph=gt, mode="all")
			cb <- t(combn(1:gorder(gt),2))
			vals <- (vals[cb[,1]] * vals[cb[,2]])[idx]
			# set colors
			fine <- 500 									# granularity of the color gradient
#			cols <- sapply(viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))], function(col) make.color.transparent(col,75))
			cols <- viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))]
			plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial_",sdist,"_col=",meas))
			for(fformat in FORMAT)
			{	if(fformat=="pdf")
					pdf(paste0(plot.file,".pdf"))
				else if(fformat=="png")
					png(paste0(plot.file,".png"))
					plot(
						x=gvals[order(vals)], y=svals[order(vals)], 
						xlab="Undirected graph distance", ylab=xlab[sdist],
						#log="xy", 
						las=1, col=cols[order(vals)],
						#xlim=c(1,max(deg.vals)*1.1)
					)
					# mean
					avg.dist <- sapply(min(gvals):max(gvals), function(deg) mean(svals[gvals==deg]))
					lines(	
						x=min(gvals):max(gvals), avg.dist,
						col="BLACK"
					)
					# legend
					gradientLegend(range(vals), color=viridis(fine,direction=-1), inside=TRUE)
				dev.off()
			}
		}
	}
	
	# record correlations
	tab.file <- file.path(distance.folder, paste0(fname,"_correlations.csv"))
	write.csv(cor.tab, file=tab.file, row.names=FALSE)	
	
	
	###### compare graph and spatial average distances
	tlog(2,"Comparing graph and spatial average distances")
	sdists <- c("database","interpolation")			# only positions from the DB vs. all positions including estimates
	fname <- paste0(MEAS_DISTANCE,"_undirected_avg")
	distance.folder <- file.path(out.folder,g$name,MEAS_DISTANCE)
	
	# init correlation table
	cor.tab <- matrix(NA,nrow=2,ncol=6)	
	cor.tab <- data.frame(cor.tab)
	cor.tab <- cbind(c("Database","Interpolation"), cor.tab)
	colnames(cor.tab) <- c("Coordinates", "PearsonCoef", "PearsonPval", "SpearmanCoef", "SpearmanPval", "KendallCoef", "KendallPval")
	rownames(cor.tab) <- sdists
	
	# compute spatial average distances
	xlab <- c(database="Average spatial distance (database)", interpolation="Average spatial distance (interpolation)")
	for(sdist in sdists)
	{	tlog(6,"Computing spatial distance (",sdist,")")
		if(sdist=="database")
			coords <- cbind(vertex_attr(g, name=COL_LOC_X), vertex_attr(g, name=COL_LOC_Y))
		else
			coords <- cbind(V(g)$x, V(g)$y)
		idx <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
		rem <- which(is.na(coords[,1]) | is.na(coords[,2]))
		svals <- as.matrix(dist(x=coords[idx,], method="euclidean", diag=TRUE, upper=TRUE))
		
		# compute average undirected graph distance
		tlog(8,"Computing undirected graph distance")
		if(length(rem)>0) 
			gt <- delete_vertices(g,rem)
		else
			gt <- g
		if(gorder(gt)>2)
		{	gvals <- distances(graph=gt, mode="all")
			idx <- which(is.infinite(gvals), arr.ind=TRUE)
			if(nrow(idx)>0)
			{	for(r in 1:nrow(idx)) gvals[idx[r,1],idx[r,2]] <- NA
				for(r in 1:nrow(idx)) svals[idx[r,1],idx[r,2]] <- NA
			}
			gvals <- sapply(1:nrow(gvals), function(r) mean(gvals[r,-r], na.rm=TRUE))
			svals <- sapply(1:nrow(svals), function(r) mean(svals[r,-r], na.rm=TRUE))
			flag.keep <- !is.na(gvals) & !is.na(svals)
			gvals <- gvals[flag.keep]
			svals <- svals[flag.keep]
			
			# compute distribution
			plot.file <- file.path(distance.folder,paste0(fname,"_histo_spatial_",sdist))
			tlog(8,"Plotting in \"",plot.file,"\"")
			if(length(svals)>2)
				custom.hist(vals=svals, xlab[sdist], file=plot.file)
			
			# compute correlations
			tlog(8,"Computing correlation between graph and spatial distances (",length(gvals)," values vs. ",length(svals)," values)")
			if(length(gvals)<3)
				tlog(10,"WARNING: not enough values to compute correlation or produce plots")
			else
			{	tmp <- cor.test(x=gvals, y=svals, method="pearson")
				cor.tab[sdist,"PearsonCoef"] <- tmp$estimate
				cor.tab[sdist,"PearsonPval"] <- tmp$p.value
				tmp <- cor.test(x=gvals, y=svals, method="spearman")
				cor.tab[sdist,"SpearmanCoef"] <- tmp$estimate
				cor.tab[sdist,"SpearmanPval"] <- tmp$p.value
				tmp <- cor.test(x=gvals, y=svals, method="kendall")
				cor.tab[sdist,"KendallCoef"] <- tmp$estimate
				cor.tab[sdist,"KendallPval"] <- tmp$p.value
				# NOTE: null hypothesis=zero correlation >> small p means this hypothesis is rejected
				
				# plot the spatial distance as a function of the graph-based one
				avg.dist <- sapply(sort(unique(gvals)), function(deg) mean(svals[gvals==deg]))
				plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial_",sdist))
				for(fformat in FORMAT)
				{	if(fformat=="pdf")
						pdf(paste0(plot.file,".pdf"))
					else if(fformat=="png")
						png(paste0(plot.file,".png"))
					plot(
						x=gvals, y=svals, 
						xlab="Undirected average graph distance", ylab=xlab[sdist],
						#log="xy", 
						las=1, col=make.color.transparent("RED",75)
						#xlim=c(1,max(deg.vals)*1.1)
					)
					# mean
					lines(	
						x=sort(unique(gvals)), y=avg.dist,
						col="BLACK"
					)
					dev.off()
				}
				
				# same using degree for colors
				meas <- MEAS_DEGREE
				vals <- igraph::degree(graph=gt, mode="all")[flag.keep]
				# set colors
				fine <- 500 									# granularity of the color gradient
				cols <- viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))]
				# produce files
				avg.dist <- sapply(sort(unique(gvals)), function(deg) mean(svals[gvals==deg]))
				plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial_",sdist,"_col=",meas))
				for(fformat in FORMAT)
				{	if(fformat=="pdf")
						pdf(paste0(plot.file,".pdf"))
					else if(fformat=="png")
						png(paste0(plot.file,".png"))
					plot(
						x=gvals[order(vals)], y=svals[order(vals)], 
						xlab="Undirected average graph distance", ylab=xlab[sdist],
						#log="xy", 
						las=1, col=cols[order(vals)],
						#xlim=c(1,max(deg.vals)*1.1)
					)
					# mean
					lines(	
						x=sort(unique(gvals)), y=avg.dist,
						col="BLACK"
					)
					# legend
					gradientLegend(range(vals), color=viridis(fine,direction=-1), inside=TRUE)
					dev.off()
				}
				
				# same using colors for fixed buildings
				edf <- vertex_attr(graph=g, name=COL_LOC_TYPE)[flag.keep] %in% c("Edifice","Porte","Repere")
				types <- vertex_attr(graph=g, name=COL_LOC_TYPE)[flag.keep]
				if(any(edf))
				{	plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial_",sdist,"_col=fixed"))
					cols <- rep(make.color.transparent("BLACK",75), length(vals))
					pal <- get.palette(3)[1:3]
					cols[edf & types=="Edifice"] <- pal[1]
					cols[edf & types=="Porte"] <- pal[2]
					cols[edf & types=="Repere"] <- pal[3]
					
					for(fformat in FORMAT)
					{	if(fformat=="pdf")
							pdf(paste0(plot.file,".pdf"))
						else if(fformat=="png")
							png(paste0(plot.file,".png"))
						plot(
							x=gvals[order(edf)], y=svals[order(edf)], 
							xlab="Undirected average graph distance", ylab=xlab[sdist],
							#log="xy", 
							las=1, col=cols[order(edf)],
							#xlim=c(1,max(deg.vals)*1.1)
						)
						# mean
#						lines(	
#							x=min(gvals):max(gvals), avg.dist,
#							col="BLACK"
#						)
						# legend
						legend(
							x="bottomright",
							fill=c(pal,"BLACK"),
							legend=c("Edifice","Gate","Landmark","Others")
						)
						dev.off()
					}
				}
			}
		}
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
