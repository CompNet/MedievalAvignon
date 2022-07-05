#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
# 
# source("res/measures/distance.R")
#############################################################################################




#############################################################
# measure name
MEAS_DISTANCE <- "distance"
MEAS_DISTANCE_AVG_SPATIAL <- "distance-arith-spatial"
MEAS_DISTANCE_AVG_GEODESIC <- "distance-arith-geodesic"
MEAS_DISTANCE_HARM_SPATIAL <- "distance-harmo-spatial"
MEAS_DISTANCE_HARM_GEODESIC <- "distance-harmo-geodesic"
MEAS_DISTANCE_COR_PEARSON <- "distance-cor-pearson"
MEAS_DISTANCE_COR_SPEARMAN <- "distance-cor-spearman"
MEAS_DISTANCE_COR_KENDALL <- "distance-cor-kendall"
MEAS_LONG_NAMES[MEAS_DISTANCE] <- "Distance"
MEAS_LONG_NAMES[MEAS_DISTANCE_AVG_SPATIAL] <- "Average Spatial Distance"
MEAS_LONG_NAMES[MEAS_DISTANCE_AVG_GEODESIC] <- "Average Geodesic Distance"
MEAS_LONG_NAMES[MEAS_DISTANCE_HARM_SPATIAL] <- "Harmonic Mean of the Spatial Distance"
MEAS_LONG_NAMES[MEAS_DISTANCE_HARM_GEODESIC] <- "Harmonic Mean of the Geodesic Distance"
MEAS_LONG_NAMES[MEAS_DISTANCE_COR_PEARSON] <- "Distance Correlation (Pearson)"
MEAS_LONG_NAMES[MEAS_DISTANCE_COR_SPEARMAN] <- "Distance Correlation (Spearman)"
MEAS_LONG_NAMES[MEAS_DISTANCE_COR_KENDALL] <- "Distance Correlation (Kendall)"




#############################################################
# Computes average distances and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.distance <- function(g, out.folder, fast)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing distances: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_DISTANCE,"_",mode)
		distance.folder <- file.path(out.folder,g$name,MEAS_DISTANCE)
		dir.create(path=distance.folder, showWarnings=FALSE, recursive=TRUE)
		
		
		###### deal with graph distance
		# distance distribution
		vals <- distances(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
		diag(vals) <- NA
		flat.vals <- vals[upper.tri(vals,diag=FALSE)]
		if(length(flat.vals)>2)
		{	plot.file <- file.path(distance.folder,paste0(fname,"_histo"))
			tlog(4,"Plotting distribution in '",plot.file,"'")
			custom.hist(vals=flat.vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_DISTANCE]), file=plot.file)
		}
		
		# average distance distributions
		tlog(4,"Dealing with average distance distribution")
		for(avg.type in c("arith","harmo"))
		{	tlog(6,"Computing mean type ",avg.type)
			
			# compute mean
			if(avg.type=="arith")
			{	avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)], na.rm=TRUE))
				xlab <- paste0("Average ",MEAS_LONG_NAMES[mode]," Geodesic ",MEAS_LONG_NAMES[MEAS_DISTANCE])
			}
			else if(avg.type=="harmo")
			{	avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) 1/mean(1/v[v>0], na.rm=TRUE))
				xlab <- paste0("Harmonic Mean of the ",MEAS_LONG_NAMES[mode]," Geodesic ",MEAS_LONG_NAMES[MEAS_DISTANCE])
			}
			
			# record values and create plots
			if(length(avg.vals)>2)
			{	plot.file <- file.path(distance.folder,paste0(fname,"_avg-",avg.type,"_histo"))
				tlog(8,"Plotting histogram in '",plot.file,"'")
				custom.hist(vals=avg.vals, name=xlab, file=plot.file)
				
				# export CSV with average distance
				df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), avg.vals)
				colnames(df) <- c("Id","Name",paste0(fname,"_avg")) 
				tab.file <- file.path(distance.folder,paste0(fname,"_avg-",avg.type,"_values.csv"))
				tlog(8,"Recording values in '",tab.file,"'")
				write.csv(df, file=tab.file, row.names=FALSE)
				
				# add results to the graph (as attributes) and stats table
				g <- set_vertex_attr(graph=g, name=paste0(fname,"-",avg.type,"_avg"), value=avg.vals)
				g <- set_graph_attr(graph=g, name=paste0(fname,"-",avg.type,"_mean"), value=mean(flat.vals[!is.infinite(flat.vals)], na.rm=TRUE))
				g <- set_graph_attr(graph=g, name=paste0(fname,"-",avg.type,"_stdev"), value=sd(flat.vals[!is.infinite(flat.vals)], na.rm=TRUE))
				stats[paste0(fname,"-",avg.type), ] <- list(Value=NA, Mean=mean(vals[!is.infinite(vals)], na.rm=TRUE), Stdv=sd(vals[!is.infinite(vals)], na.rm=TRUE))
				
				# plot graph using color for average distance
				plot.file <- file.path(distance.folder,paste0(fname,"_avg-",avg.type,"_graph"))
				tlog(8,"Plotting graph in '",plot.file,"'")
				#g <- update.node.labels(g, avg.vals)
				V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
				g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=paste0(fname,"-",avg.type,"_avg"), file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
				g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=paste0(fname,"-",avg.type,"_avg"), file=paste0(plot.file,"_kk"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
			}
		}
		
		# for each node, plot graph using color for distance
		if(!fast)
		{	tlog(4,"Dealing with individual node plots")
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
					tlog(6,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as its degree is <3")
				else
				{	g <- set_vertex_attr(graph=g, name=fname, value=vals[n,])
					if(all(is.infinite(vals[n,-n])))
						tlog(6,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are infinite")
					else
					{	g <- update.node.labels(g, vals[n,])
						shrt.nm <- substr(nname,1,30)		# to avoid long file names
						id.cln <- gsub(":", "-", id, fixed=TRUE)
						id.cln <- gsub("/", "_", id.cln, fixed=TRUE)
						V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
						tlog(6,"Plotting graph for node #",id," (",nname, ", ",n,"/",gorder(g),") in '",file.path(mode.folder,"xxxx",paste0(id.cln,"_",shrt.nm )),"'")
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
		tlog(2,"Recording stats in '",stat.file,"'")
		write.csv(stats, file=stat.file, row.names=TRUE)
		
		
		###### compare graph and spatial distances
		tlog(2,"Comparing graph and spatial distances")
		sdists <- c("database","interpolation")			# only positions from the DB vs. all positions including estimates
		fname <- paste0(MEAS_DISTANCE,"_undirected")
		distance.folder <- file.path(out.folder,g$name,MEAS_DISTANCE)
		
		# init correlation table
		cor.tab <- matrix(NA,nrow=2,ncol=10)	
		cor.tab <- data.frame(cor.tab)
		cor.tab <- cbind(c("Database","Interpolation"), cor.tab)
		colnames(cor.tab) <- c("Coordinates", "PearsonFiniteCoef", "PearsonFinitePval", "SpearmanFiniteCoef", "SpearmanFinitePval", "SpearmanInfiniteCoef", "SpearmanInfinitePval", "KendallFiniteCoef", "KendallFinitePval", "KendallInfiniteCoef", "KendallInfinitePval")
		rownames(cor.tab) <- sdists
		
		# compute spatial distances
		ylabs <- c(database="Spatial distance (database)", interpolation="Spatial distance (interpolation)")
		xlab <- paste0(mode," Geodesic Distance")
		for(sdist in sdists)
		{	tlog(6,"Computing spatial distance (",sdist,")")
			if(sdist=="database")
				coords <- cbind(vertex_attr(g, name=COL_LOC_X), vertex_attr(g, name=COL_LOC_Y))
			else
				coords <- cbind(V(g)$x, V(g)$y)
			idx0 <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
			svals <- as.matrix(dist(x=coords[idx0,], method="euclidean", diag=TRUE, upper=TRUE))
			diag(svals) <- NA
			svals.avg.arith <- apply(X=svals, MARGIN=1, FUN=function(v) mean(v[!is.na(v)]))
			svals.avg.harmo <- apply(X=svals, MARGIN=1, FUN=function(v) 1/mean(1/v[!is.na(v) & v>0]))
			svals <- svals[upper.tri(svals, diag=FALSE)]
			
			# compute distribution
			if(length(svals)>2)
			{	plot.file <- file.path(distance.folder,paste0(fname,"_histo_spatial-",sdist))
				tlog(8,"Plotting histogram in \"",plot.file,"\"")
				custom.hist(vals=svals, name=ylabs[sdist], file=plot.file)
			}
			
			# plot graph using color for average distance
			for(avg.type in c("arith","harmo"))
			{	satt <- paste0("SpatialDist_avg-",avg.type)
				plot.file <- file.path(distance.folder,paste0("distance_spatial-",sdist,"_avg-",avg.type,"_graph"))
				tlog(8,"Plotting graph in \"",plot.file,"\"")
				
				avg.vals <- rep(NA,gorder(g))
				if(avg.type=="arith")
					avg.vals[idx0] <- svals.avg.arith
				else
					avg.vals[idx0] <- svals.avg.harmo
				g <- set_vertex_attr(graph=g, name=satt, value=avg.vals)
				V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
				g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=satt, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
				g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=satt, file=paste0(plot.file,"_kk"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
			}
			
			# compute undirected graph distance
			tlog(8,"Computing undirected geodesic distance")
			gvals <- distances(graph=g, mode="all", v=idx0, to=idx0)
			diag(gvals) <- NA
			gvals <- gvals[upper.tri(gvals, diag=FALSE)]
			idx <- !is.infinite(gvals)
			gvals2 <- gvals; gvals2[which(is.infinite(gvals))] <- rep(max(gvals[idx],na.rm=TRUE)+1, length(which(is.infinite(gvals))))	# values with max+1 instead of Inf (for rank-based correlation measures)
			svals2 <- svals
			gvals <- gvals[idx]
			svals <- svals[idx]
			
			# compute correlations
			tlog(8,"Computing correlation between graph and spatial distances (",length(gvals)," values vs. ",length(svals)," values)")
			if(length(gvals)<5)
				tlog(10,"WARNING: not enough values to compute correlation or produce plots")
			else
			{	tmp <- cor.test(x=gvals, y=svals, method="pearson")
				cor.tab[sdist,"PearsonFiniteCoef"] <- tmp$estimate
				cor.tab[sdist,"PearsonFinitePval"] <- tmp$p.value
				tmp <- rcorr(x=gvals, y=svals, type="spearman")
				cor.tab[sdist,"SpearmanFiniteCoef"] <- tmp$r[1,2]
				cor.tab[sdist,"SpearmanFinitePval"] <- tmp$P[1,2]
				tmp <- rcorr(x=gvals2, y=svals2, type="spearman")
				cor.tab[sdist,"SpearmanInfiniteCoef"] <- tmp$r[1,2]
				cor.tab[sdist,"SpearmanInfinitePval"] <- tmp$P[1,2]
				tmp <- cor.test(x=gvals, y=svals, method="kendall")	# alternative: cor.fk(x=gvals, y=svals) # problem: it does not compute the p-value
				cor.tab[sdist,"KendallFiniteCoef"] <- tmp$estimate
				cor.tab[sdist,"KendallFinitePval"] <- tmp$p.value
				tmp <- cor.test(x=gvals2, y=svals2, method="kendall")	# alternative: cor.fk(x=gvals, y=svals) # problem: it does not compute the p-value
				cor.tab[sdist,"KendallInfiniteCoef"] <- tmp$estimate
				cor.tab[sdist,"KendallInfinitePval"] <- tmp$p.value
				# NOTE: null hypothesis=zero correlation >> small p means this hypothesis is rejected
				
				# compute average & stdev
				xs <- sort(unique(gvals))
				avg.dist <- sapply(xs, function(x) mean(svals[gvals==x],na.rm=TRUE))
				stdev.dist <- sapply(xs, function(x) sd(svals[gvals==x]))
				stdev.dist[is.na(stdev.dist)] <- 0
		
				# plot the spatial distance as a function of the graph-based one
				plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial-",sdist))
				tlog(10,"Plotting data in 'plot.file'")
				for(fformat in FORMAT)
				{	if(fformat=="pdf")
						pdf(paste0(plot.file,".pdf"))
					else if(fformat=="png")
						png(paste0(plot.file,".png"))
						plot(
							x=gvals, y=svals, 
							xlab=xlab, ylab=ylabs[sdist],
							#log="xy", 
							las=1, col=make.color.transparent("RED",75)
							#xlim=c(1,max(deg.vals)*1.1)
						)
						# mean
						lines(	
							x=min(gvals):max(gvals), avg.dist,
							col="BLACK"
						)
					dev.off()
				}
				
				# same using degree for colors
				meas <- MEAS_DEGREE
				vals <- igraph::degree(graph=g, mode="all", v=idx0)
				cb <- t(combn(1:length(idx0),2))
				vals <- (vals[cb[,1]] * vals[cb[,2]])[idx]
				# set colors
				fine <- 500 									# granularity of the color gradient
#				cols <- sapply(viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))], function(col) make.color.transparent(col,75))
				cols <- viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))]
				plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial-",sdist,"_col=",meas))
				tlog(10,"Plotting data in 'plot.file'")
				for(fformat in FORMAT)
				{	if(fformat=="pdf")
						pdf(paste0(plot.file,".pdf"))
					else if(fformat=="png")
						png(paste0(plot.file,".png"))
						plot(
							x=gvals[order(vals)], y=svals[order(vals)], 
							xlab=xlab, ylab=ylabs[sdist],
							#log="xy", 
							las=1, col=cols[order(vals)],
							#xlim=c(1,max(deg.vals)*1.1)
						)
						# mean
						lines(	
							x=min(gvals):max(gvals), avg.dist,
							col="BLACK"
						)
						# legend
						gradientLegend(range(vals), color=viridis(fine,direction=-1), inside=TRUE)
					dev.off()
				}
				
				# same but as a binned scatterplot
				plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial-",sdist,"_binned"))
				tlog(10,"Plot binned version in  '",plot.file,"'")
				for(fformat in FORMAT)
				{	if(fformat=="pdf")
						pdf(paste0(plot.file,".pdf"))
					else if(fformat=="png")
						png(paste0(plot.file,".png"))
					plot(
						NULL,
						xlab="Undirected geodesic distance", ylab="Spatial distance",
						las=1, #log="xy", 
						ylim=range(svals,na.rm=TRUE),
						xlim=range(gvals,na.rm=TRUE)
					)
					polygon(
						x=c(xs,rev(xs)), y=c(avg.dist-stdev.dist,rev(avg.dist+stdev.dist)), 
						col=make.color.transparent("RED",85), border=NA
					)
#					arrows(
#						x0=xs, y0=avg.dist-stdev.dist, 
#						x1=xs, y1=avg.dist+stdev.dist, 
#						code=3, angle=90, length=0.05, 
#						col="PINK", lwd=2
#					)
					lines(
						x=xs, y=avg.dist, 
						col="RED", pch=19
					)
					dev.off()
				}
			}
		}
		
		# record correlations
		tab.file <- file.path(distance.folder, paste0(fname,"_correlations.csv"))
		tlog(4,"Recording correlation values in file '",tab.file,"'")
		write.csv(cor.tab, file=tab.file, row.names=FALSE)	
	
	
		###### compare graph and spatial average distances
		tlog(2,"Comparing graph and spatial average distances")
		sdists <- c("database","interpolation")			# only positions from the DB vs. all positions including estimates
		distance.folder <- file.path(out.folder,g$name,MEAS_DISTANCE)
		
		for(avg.type in c("arith","harmo"))
		{	tlog(4,"Using average type: ",avg.type)
			fname <- paste0(MEAS_DISTANCE,"_undirected_avg-",avg.type)
			
			# set labels
			if(avg.type=="arith")
				m.lab <- "Arithmetic"
			else if(avg.type=="harmo")
				m.lab <- "Harmonic"
			
			# init correlation table
			cor.tab <- matrix(NA,nrow=2,ncol=6)	
			cor.tab <- data.frame(cor.tab)
			cor.tab <- cbind(c("Database","Interpolation"), cor.tab)
			colnames(cor.tab) <- c("Coordinates", "PearsonCoef", "PearsonPval", "SpearmanCoef", "SpearmanPval", "KendallCoef", "KendallPval")
			rownames(cor.tab) <- sdists
			
			# compute spatial average distances
			ylabs <- c(database=paste0(m.lab," Mean of the Spatial Distance (database)"), interpolation=paste0(m.lab," Mean of the Spatial Distance (interpolation)"))
			xlab <- paste0(m.lab," Mean of the ",mode," Geodesic Distance")
			for(sdist in sdists)
			{	tlog(6,"Computing spatial distance (",sdist,")")
				if(sdist=="database")
					coords <- cbind(vertex_attr(g, name=COL_LOC_X), vertex_attr(g, name=COL_LOC_Y))
				else
					coords <- cbind(V(g)$x, V(g)$y)
				idx0 <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
				svals <- as.matrix(dist(x=coords[idx0,], method="euclidean", diag=TRUE, upper=TRUE))
				diag(svals) <- NA
				
				# compute average undirected graph distance
				tlog(8,"Computing undirected geodesic distance")
				if(length(idx0)>4)
				{	gvals <- distances(graph=g, mode="all", v=idx0, to=idx0)
					diag(gvals) <- NA
					
					if(avg.type=="arith")
					{	idx <- which(is.infinite(gvals), arr.ind=TRUE)
						if(nrow(idx)>0)
						{	for(r in 1:nrow(idx)) gvals[idx[r,1],idx[r,2]] <- NA
							for(r in 1:nrow(idx)) svals[idx[r,1],idx[r,2]] <- NA
						}
						gvals <- sapply(1:nrow(gvals), function(r) mean(gvals[r,-r], na.rm=TRUE))
						svals <- sapply(1:nrow(svals), function(r) mean(svals[r,-r], na.rm=TRUE))
					}
					else if(avg.type=="harmo")
					{	idx <- which(svals==0, arr.ind=TRUE)
						if(nrow(idx)>0)
						{	for(r in 1:nrow(idx)) gvals[idx[r,1],idx[r,2]] <- NA
							for(r in 1:nrow(idx)) svals[idx[r,1],idx[r,2]] <- NA
						}
						gvals <- sapply(1:nrow(gvals), function(r) 1/mean(1/gvals[r,-r], na.rm=TRUE))
						svals <- sapply(1:nrow(svals), function(r) 1/mean(1/svals[r,-r], na.rm=TRUE))
					}
					
					# compute distribution
					if(length(svals)>2)
					{	plot.file <- file.path(distance.folder,paste0(fname,"_histo_spatial-",sdist))
						tlog(8,"Plotting distribution in \"",plot.file,"\"")
						custom.hist(vals=svals, name=ylabs[sdist], file=plot.file)
					}
					
					# clean NA values
					flag.keep <- !is.na(gvals) & !is.infinite(gvals) & !is.na(svals)
					gvals <- gvals[flag.keep]
					svals <- svals[flag.keep]
					
					# compute correlations
					tlog(8,"Computing correlation between graph and spatial distances (",length(gvals)," values vs. ",length(svals)," values)")
					if(length(gvals)<5)
						tlog(10,"WARNING: not enough values to compute correlation or produce plots")
					else
					{	tmp <- cor.test(x=gvals, y=svals, method="pearson")
						cor.tab[sdist,"PearsonCoef"] <- tmp$estimate
						cor.tab[sdist,"PearsonPval"] <- tmp$p.value
						tmp <- rcorr(x=gvals, y=svals, type="spearman")
						cor.tab[sdist,"SpearmanCoef"] <- tmp$r[1,2]
						cor.tab[sdist,"SpearmanPval"] <- tmp$P[1,2]
						tmp <- cor.test(x=gvals, y=svals, method="kendall")	# alternative: cor.fk(x=gvals, y=svals) # problem: it does not compute the p-value
						cor.tab[sdist,"KendallCoef"] <- tmp$estimate
						cor.tab[sdist,"KendallPval"] <- tmp$p.value
						# NOTE: null hypothesis=zero correlation >> small p means this hypothesis is rejected
						
						# plot the spatial distance as a function of the graph-based one
						avg.dist <- sapply(sort(unique(gvals)), function(v) mean(svals[gvals==v],na.rm=TRUE))
						plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial-",sdist))
						tlog(10,"Plotting in file \"",plot.file,"\"")
						for(fformat in FORMAT)
						{	if(fformat=="pdf")
								pdf(paste0(plot.file,".pdf"))
							else if(fformat=="png")
								png(paste0(plot.file,".png"))
							plot(
								x=gvals, y=svals, 
								xlab=xlab, ylab=ylabs[sdist],
								#log="xy", 
								las=1, col=make.color.transparent("RED",75)
								#xlim=c(1,max(deg.vals)*1.1)
							)
							# mean
# the average follows the individual points, plot not readable
#							lines(	
#								x=sort(unique(gvals)), y=avg.dist,
#								col="BLACK"
#							)
							dev.off()
						}
						
						# same using degree for colors
						meas <- MEAS_DEGREE
						vals <- igraph::degree(graph=g, mode="all", v=idx0)[flag.keep]
						# set colors
						fine <- 500 									# granularity of the color gradient
						cols <- viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))]
						# produce files
						avg.dist <- sapply(sort(unique(gvals)), function(v) mean(svals[gvals==v],na.rm=TRUE))
						plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial-",sdist,"_col=",meas))
						tlog(10,"Plotting in file \"",plot.file,"\"")
						for(fformat in FORMAT)
						{	if(fformat=="pdf")
								pdf(paste0(plot.file,".pdf"))
							else if(fformat=="png")
								png(paste0(plot.file,".png"))
							plot(
								x=gvals[order(vals)], y=svals[order(vals)], 
								xlab=xlab, ylab=ylabs[sdist],
								#log="xy", 
								las=1, col=cols[order(vals)],
								#xlim=c(1,max(deg.vals)*1.1)
							)
							# mean
# the average follows the individual points, plot not readable
#							lines(	
#								x=sort(unique(gvals)), y=avg.dist,
#								col="BLACK"
#							)
							# legend
							gradientLegend(range(vals), color=viridis(fine,direction=-1), inside=TRUE)
							dev.off()
						}
						
						# same using colors for fixed buildings
						edf <- vertex_attr(graph=g, name=COL_LOC_TYPE, index=idx0)[flag.keep] %in% c("Edifice","Porte","Repere")
						types <- vertex_attr(graph=g, name=COL_LOC_TYPE, index=idx0)[flag.keep]
						if(any(edf))
						{	plot.file <- file.path(distance.folder, paste0(fname,"_vs_spatial-",sdist,"_col=fixed"))
							tlog(10,"Plotting in file \"",plot.file,"\"")
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
									xlab=xlab, ylab=ylabs[sdist],
									#log="xy", 
									las=1, col=cols[order(edf)],
									#xlim=c(1,max(deg.vals)*1.1)
								)
								# mean
# the average follows the individual points, plot not readable
#								lines(	
#									x=sort(unique(gvals)), avg.dist,
#									col="BLACK"
#								)
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
			tlog(4,"Recording correlation values in file \"",tab.file,"\"")
			write.csv(cor.tab, file=tab.file, row.names=FALSE)
		}

	}
	
	
	###### finalize
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	tlog(4,"Updating graph file '",graph.file,"'")
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
