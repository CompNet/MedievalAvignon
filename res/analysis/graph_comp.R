# TODO: Add comment
# 
# Author: Vincent Labatut
###############################################################################




#############################################################################################
# Creates plots comparing all pairs of graphs whose names are specified.
#
# graph.names: names of the graphs to compare.
# folder: root folder to find the graph and record the plots.
#############################################################################################
plot.graph.comparisons <- function(graph.names, folder)
{	for(i in 1:(length(graph.names)-1))
	{	# read the first graph
		file.path <- file.path(folder, graph.names[i], FILE_GRAPH)
		g1 <- load.graphml.file(file=file.path)
		# clean it
		V(g1)$label <- paste(vertex_attr(g1,name=COL_LOC_ID), get.location.names(g1),sep="_")
		g1 <- delete_edge_attr(g1, LK_TYPE)
		g1 <- simplify(g1)
		
		for(j in (i+1):length(graph.names))
		{	# read the second graph
			file.path <- file.path(folder, graph.names[j], FILE_GRAPH)
			g2 <- load.graphml.file(file=file.path)
			# clean it
			V(g2)$label <- paste(vertex_attr(g2,name=COL_LOC_ID), get.location.names(g2),sep="_")
			g2 <- delete_edge_attr(g2, LK_TYPE)
			g2 <- simplify(g2)
			
			# produce plots
			plot.graph.comparison(g1, g2, folder)
		}
	}	
}




#############################################################################################
# Creates a plot comparing both specified graph. Both graphs are plot, with colors showing
# extra/missing vertices.
#
# g1: first graph to compare.
# g2: second graph to compare.
# folder: root folder to record the plot files.
#############################################################################################
plot.graph.comparison <- function(g1, g2, folder)
{	# init file names
	plot.file1 <- file.path(folder, g1$name, paste0("graph_comparison_", g2$name))
	plot.file2 <- file.path(folder, g2$name, paste0("graph_comparison_", g1$name))
	
	# perform comparison
	att1 <- rep("Present",gorder(g1))
	att2 <- rep("Present",gorder(g2))
	names1 <- gsub("_part","",V(g1)$idExterne,fixed=TRUE)
	names2 <- gsub("_part","",V(g2)$idExterne,fixed=TRUE)
	idx1 <- which(is.na(match(names1, names2)))
	idx2 <- which(is.na(match(names2, names1)))
	att1[idx1] <- "Absent"
	att2[idx2] <- "Absent"
	V(g1)$comparison <- att1
	V(g2)$comparison <- att2
	
	# create the geo plots
	custom.gplot(g=g1, col.att="comparison", cat.att=TRUE, file=paste0(plot.file1,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
	custom.gplot(g=g2, col.att="comparison", cat.att=TRUE, file=paste0(plot.file2,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
	
	# create the algo plots
	V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5
	custom.gplot(g=g1, col.att="comparison", cat.att=TRUE, file=paste0(plot.file1,"_kk"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
	V(g2)$x <- V(g2)$x2; V(g2)$y <- V(g2)$y2; E(g2)$weight <- 0.5
	custom.gplot(g=g2, col.att="comparison", cat.att=TRUE, file=paste0(plot.file2,"_kk"), rescale=FALSE, xlim=range(V(g2)$x), ylim=range(V(g2)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
}




#############################################################################################
# Loads graphs corresponding to the removal of streets of decreasing length, and compute and
# plots some graph-related stats.
#############################################################################################
plot.street.removal <- function()
{	tlog(2,"Plotting stats related to street removal")
	
	folders <- c(GR_EST_FLAT_MINUS, paste0(GR_EST_FLAT_MINUS,"_filtered"))
	for(f in 1:length(folders))
	{	folder <- folders[f]
		tlog(2,"Dealing with folder '",folder,"' (",f,"/",length(folders),")")
		
		# get folder path 
		main.folder <- file.path(FOLDER_OUT_ANAL_EST, folder, "_removed_streets")
		graph.folder <- file.path(main.folder, "graphs")
		dist.folder <- file.path(main.folder, "dist_geodesic_vs_spatial")
		dir.create(path=dist.folder, showWarnings=FALSE, recursive=TRUE)
		
		# list graph files
		ll <- list.files(path=graph.folder, pattern="graph_rem=[0-9]+\\.graphml", full.names=TRUE)
		tlog(4,"Found ",length(ll)," graph files in folder '",graph.folder,"'")
		
		# read graphs
		tlog(4,"Reading graph files")
		gs <- list()
		street.names <- rep(NA, length(ll))
		street.lengths <- rep(NA, length(ll))
		for(i in 1:length(ll))
		{	graph.file <- ll[[i]]
			tlog(6,"Reading file '",graph.file,"'")
			g <- load.graphml.file(file=graph.file)
			#r <- as.integer(substr(basename(graph.file), start=nchar("graph_rem=")+1, stop=unlist(gregexpr(pattern=".graphml",basename(graph.file)))-1))
			r <- as.integer(strsplit(g$name,"_")[[1]][3])
			gs[[r]] <- g 
			street.names[r] <- g$LastDeletedStreetId
			street.lengths[r] <- g$LastDeletedStreetLength
		}
		#sapply(gs,function(g) g$name)
		
		# compute stats
		meas.names <- c(
			MEAS_NBR_NODES, MEAS_NBR_LINKS, 
			MEAS_NBR_COMPONENTS, 
			MEAS_COMMUNITY_NBR, MEAS_MODULARITY, MEAS_NMI, MEAS_RI, MEAS_ARI,
			MEAS_DISTANCE_AVG_SPATIAL, MEAS_DISTANCE_AVG_GEODESIC, MEAS_DISTANCE_HARM_SPATIAL, MEAS_DISTANCE_HARM_GEODESIC
		)
		cor.names <- c(
			MEAS_DISTANCE_COR_PEARSON, 
			MEAS_DISTANCE_COR_SPEARMAN, paste0(MEAS_DISTANCE_COR_SPEARMAN,"-finite"), 
			MEAS_DISTANCE_COR_KENDALL, paste0(MEAS_DISTANCE_COR_KENDALL,"-finite")
		)
		tab.stats <- data.frame(1:length(gs), street.names, street.lengths, matrix(NA, nrow=length(gs), ncol=length(meas.names)))
		colnames(tab.stats) <- c("NumberDeletedStreets", "LastDeletedStreetId", "LastDeletedStreetLength", meas.names)
		tlog(4,"Computing stats")
		mbr.prev <- NA
		for(i in 1:nrow(tab.stats))
		{	tlog(6,"Processing graph #",i,"/",nrow(tab.stats))
			
			# counts
			tlog(8,"Compute node and edge counts")
			tab.stats[i,MEAS_NBR_NODES] <- gorder(gs[[i]])
			tab.stats[i,MEAS_NBR_LINKS] <- gsize(gs[[i]])
			
			# components
			tlog(8,"Compute component counts")
			tab.stats[i,MEAS_NBR_COMPONENTS] <- components(graph=gs[[i]], mode="weak")$no
			
			# communities
			tlog(8,"Compute communities")
			coms <- cluster_louvain(graph=simplify(as.undirected(gs[[i]])), weights=NULL)
			mbr.cur <- as.integer(membership(coms))
			names(mbr.cur) <- V(gs[[i]])$idExterne
			tab.stats[i,MEAS_COMMUNITY_NBR] <- length(unique(mbr.cur))
			tab.stats[i,MEAS_MODULARITY] <- modularity(coms)
			if(i>1)
			{	nn <- intersect(names(mbr.cur), names(mbr.prev))	# we only consider vertices common to both graphs
				tab.stats[i,MEAS_NMI] <- compare(comm1=mbr.cur[nn], comm2=mbr.prev[nn], method="nmi")
				tab.stats[i,MEAS_RI] <- compare(comm1=mbr.cur[nn], comm2=mbr.prev[nn], method="rand")
				tab.stats[i,MEAS_ARI] <- compare(comm1=mbr.cur[nn], comm2=mbr.prev[nn], method="adjusted.rand")
			}
			mbr.prev <- mbr.cur
			
			# spatial distance
			tlog(8,"Compute spatial distance")
			coords <- cbind(vertex_attr(gs[[i]], name=COL_LOC_X), vertex_attr(gs[[i]], name=COL_LOC_Y))
			idx0 <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
			svals <- as.matrix(dist(x=coords[idx0,], method="euclidean", diag=TRUE, upper=TRUE))
			diag(svals) <- NA
			svals <- svals[upper.tri(svals)]
			tab.stats[i,MEAS_DISTANCE_AVG_SPATIAL] <- mean(svals, na.rm=TRUE)
			tab.stats[i,MEAS_DISTANCE_HARM_SPATIAL] <- 1/mean(1/svals[svals>0], na.rm=TRUE)
			
			# geodesic distance
			tlog(8,"Compute geodesic distance")
			gvals <- distances(graph=gs[[i]], mode="all")
			diag(gvals) <- NA
			gvals <- gvals[upper.tri(gvals)]
			idx <- !is.infinite(gvals)
			tab.stats[i,MEAS_DISTANCE_AVG_GEODESIC] <- mean(gvals[idx], na.rm=TRUE)
			tab.stats[i,MEAS_DISTANCE_HARM_GEODESIC] <- 1/mean(1/gvals, na.rm=TRUE)
			
			# distance correlations
			tlog(8,"Compute distance correlation")
			gvals <- distances(graph=gs[[i]], mode="all", v=idx0, to=idx0)
			diag(gvals) <- NA
			gvals <- gvals[upper.tri(gvals)]
			idx <- !is.infinite(gvals)
			gvals2 <- gvals; gvals2[which(is.infinite(gvals))] <- rep(max(gvals[idx],na.rm=TRUE)+1, length(which(is.infinite(gvals))))	# values with max+1 instead of Inf (for rank-based correlation measures)
			tab.stats[i,MEAS_DISTANCE_COR_PEARSON] <- cor(x=gvals[idx], y=svals[idx], method="pearson")
			tab.stats[i,MEAS_DISTANCE_COR_SPEARMAN] <- rcorr(x=gvals2, y=svals, type="spearman")$r[1,2]
			tab.stats[i,paste0(MEAS_DISTANCE_COR_SPEARMAN,"-finite")] <- rcorr(x=gvals[idx], y=svals[idx], type="spearman")$r[1,2]
			tab.stats[i,MEAS_DISTANCE_COR_KENDALL] <- cor.fk(x=gvals2,y=svals)
			tab.stats[i,paste0(MEAS_DISTANCE_COR_KENDALL,"-finite")] <- cor.fk(x=gvals[idx], y=svals[idx])
			gvals <- gvals[idx]
			svals <- svals[idx]
			
			# plot geodesic vs. spatial distance
			vals <- igraph::degree(graph=gs[[i]], mode="all", v=idx0)
			cb <- t(combn(1:length(idx0),2))
			vals <- (vals[cb[,1]] * vals[cb[,2]])[idx]
			# set colors
			fine <- 500 									# granularity of the color gradient
			cols <- viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))]
			plot.file <- file.path(dist.folder, paste0("dist_geodesic_vs_spatial_rem=",i))
			tlog(8,"Plot geodesic vs. spatial distance in file '",plot.file,"'")
			for(fformat in c("png"))	# FORMAT
			{	if(fformat=="pdf")
					pdf(paste0(plot.file,".pdf"))
				else if(fformat=="png")
					png(paste0(plot.file,".png"))
				plot(
					x=gvals[order(vals)], y=svals[order(vals)], 
					xlab="Undirected geodesic distance", ylab="Spatial distance",
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
		
		# record stats
		tab.file <- file.path(main.folder, "stats.csv")
		tlog(4,"Recording stats in file '",tab.file,"'")
		write.csv(tab.stats, file=tab.file, row.names=FALSE)
		
		# plot correlations
		pal <- get.palette(length(cor.names))
		plot.file <- file.path(main.folder, "evolution_correlation")
		for(fformat in FORMAT)
		{	if(fformat=="pdf")
				pdf(paste0(plot.file,".pdf"))
			else if(fformat=="png")
				png(paste0(plot.file,".png"))
			plot(NULL, xlim=range(del.rates),ylim=c(0,1),xlab="Number of streets removed (by decreasing length)", ylab="Correlation value")
			for(i in 1:length(cor.names))
			{	cor.name <- cor.names[i]
				lines(x=1:length(gs), y=tab.stats[,cor.name],pal[i])
			}
			legend(x="bottomleft", fill=pal, legend=cor.names)
			dev.off()
		}
		
		# plot stats
		tlog(4,"Plotting stats")
		for(meas.name in meas.names)
		{	if(!all(is.na(tab.stats[,meas.name])))
			{	plot.file <- file.path(main.folder, paste0("evolution_",meas.name))
				tlog(4,"Creating plot '",plot.file,"'")
				for(fformat in FORMAT)
				{	if(fformat=="pdf")
						pdf(paste0(plot.file,".pdf"), width=25, height=25)
					else if(fformat=="png")
						png(paste0(plot.file,".png"), width=1024, height=1024)
					par(mar=c(5.1, 4.1, 4.1, 2.1))		# remove the title space Bottom Left Top Right
					plot(
						x=1:length(gs), y=tab.stats[,meas.name],
						xlab="Number of streets removed (by decreasing length)",
						ylab=MEAS_LONG_NAMES[meas.name],
						type="l", col="RED"
					)
					dev.off()
				}
			}
		}
	}
}
