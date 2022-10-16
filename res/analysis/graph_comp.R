# Scripts used to compare graphs extracted through different methods.
# 
# Author: Vincent Labatut
#
# source("res/analysis/graph_comp.R")
###############################################################################




#############################################################################################
# Creates plots comparing all pairs of graphs whose names are specified.
#
# graph.names: names of the graphs to compare.
# folder: root folder to find the graph and record the plots.
#############################################################################################
plot.graph.comparisons <- function(graph.names, folder)
{	tlog(2,"Comparing graphs")
	
	nbr <- length(graph.names)*(length(graph.names)-1)/2
	k <- 1
	tlog.start.loop(3,nbr,"Looping over pairs of graphs")
	for(i in 1:(length(graph.names)-1))
	{	# read the first graph
		file.path <- file.path(folder, graph.names[i], FILE_GRAPH)
		tlog(4,"Reading graph file '",file.path,"' (",i,"/",(length(graph.names)-1),")")
		g1 <- load.graphml.file(file=file.path)
		# clean it
		V(g1)$label <- paste(vertex_attr(g1,name=COL_LOC_ID), get.location.names(g1),sep="_")
		g1 <- delete_edge_attr(g1, LK_TYPE)
		g1 <- simplify(g1)
		s1 <- grepl("split", graph.names[i], fixed=TRUE)
		
		for(j in (i+1):length(graph.names))
		{	# read the second graph
			file.path <- file.path(folder, graph.names[j], FILE_GRAPH)
			tlog(6,"Reading graph file '",file.path,"' (",j,"/",length(graph.names),")")
			g2 <- load.graphml.file(file=file.path)
			# clean it
			V(g2)$label <- paste(vertex_attr(g2,name=COL_LOC_ID), get.location.names(g2),sep="_")
			g2 <- delete_edge_attr(g2, LK_TYPE)
			g2 <- simplify(g2)
			s2 <- grepl("split", graph.names[j], fixed=TRUE)
			
			# produce plots
			tlog.loop(6,k,"Processing pair ",k,"/",nbr)
			if(s1==s2)
				plot.graph.comparisons.same.types(g1, g2, folder)
			else
			{	if(s1)
				{	g.non <- g2
					g.split <- g1
				}
				else
				{	g.non <- g1
					g.split <- g2
				}
				plot.graph.comparisons.diff.types(g.non, g.split, folder)
			}		
			
			k <- k + 1
		}
	}	
	tlog.end.loop(3,"Loop over")
}




#############################################################################################
# Compares both specified graphs in terms of their constituting vertices. Both graphs are
# assumed to contain the same types of vertices, i.e. either both raw or split vertices.
#
# g1: first graph to compare.
# g2: second graph to compare.
# folder: root folder to record the plot files.
#############################################################################################
plot.graph.comparisons.same.types <- function(g1, g2, folder)
{	tlog(6,"Comparing same type graphs")
	
	# init folders
	folder1 <- file.path(folder, g1$name, "comparison")
	folder2 <- file.path(folder, g2$name, "comparison")
	dir.create(path=folder1, showWarnings=FALSE, recursive=TRUE)
	dir.create(path=folder2, showWarnings=FALSE, recursive=TRUE)
	# init file names
	plot.file1 <- file.path(folder1, paste0("comp_", gsub("/","__",g2$name)))
	plot.file2 <- file.path(folder2, paste0("comp_", gsub("/","__",g1$name)))
	
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
	
	# export vertex list as CSV file
	names <- sort(union(names1,names2))
	flags.g1 <- rep(FALSE,length(names))
	flags.g1[match(names1,names)] <- TRUE
	flags.g2 <- rep(FALSE,length(names))
	flags.g2[match(names2,names)] <- TRUE
	tab <- data.frame(names, flags.g1, flags.g2)
	colnames(tab) <- c("Id", g1$name, g2$name)
	tab.file <- paste0(plot.file1,"_nodes.csv")
	tlog(8,"Record vertex list in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=FALSE)
	tab.file <- paste0(plot.file2,"_nodes.csv")
	tlog(8,"Record vertex list in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=FALSE)
	
	# build result matrix
	cn <- c(g1$name,g2$name)
	rn <- c("Graph-specific vertices","Common vertices","Total")
	tab <- matrix(0,nrow=length(rn),ncol=length(cn))
	rownames(tab) <- rn
	colnames(tab) <- cn
	tab["Graph-specific vertices",] <- c(length(idx1),length(idx2))
	tab["Common vertices",] <- c(gorder(g1)-length(idx1), gorder(g2)-length(idx2))
	tab["Total",] <- colSums(tab)
	print(tab)
	
	# record result matrix in both folders
	tab.file <- paste0(plot.file1,".csv")
	tlog(8,"Record results in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=TRUE)
	tab.file <- paste0(plot.file2,".csv")
	tlog(8,"Record results in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=TRUE)
	
	# create the geo plots
	plot.file <- paste0(plot.file1,"_lambert")
	tlog(8,"Plotting in file '",plot.file,"'")
	g1 <- rescale.coordinates(g1)
	custom.gplot(g=g1, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
	#
	plot.file <- paste0(plot.file2,"_lambert")
	tlog(8,"Plotting in file '",plot.file,"'")
	g2 <- rescale.coordinates(g2)
	custom.gplot(g=g2, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
	
	# create the algo plots
	plot.file <- paste0(plot.file1,"_algo")
	tlog(8,"Plotting in file '",plot.file,"'")
	V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5
	custom.gplot(g=g1, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
	#
	plot.file <- paste0(plot.file2,"_algo")
	tlog(8,"Plotting in file '",plot.file,"'")
	V(g2)$x <- V(g2)$x2; V(g2)$y <- V(g2)$y2; E(g2)$weight <- 0.5
	custom.gplot(g=g2, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, rescale=FALSE, xlim=range(V(g2)$x), ylim=range(V(g2)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
}




#############################################################################################
# Compares both specified graphs in terms of their constituting vertices. The graphs are
# assumed to contain different types of vertices: the first one has raw vertices and the second
# has split vertices.
#
# g.non: first (non-split) graph to compare.
# g2: second (split) graph to compare.
# folder: root folder to record the plot files.
#############################################################################################
plot.graph.comparisons.diff.types <- function(g.non, g.split, folder)
{	tlog(6,"Comparing different type graphs")
	
	# init folders
	folder.non <- file.path(folder, g.non$name, "comparison")
	folder.split <- file.path(folder, g.split$name, "comparison")
	dir.create(path=folder.non, showWarnings=FALSE, recursive=TRUE)
	dir.create(path=folder.split, showWarnings=FALSE, recursive=TRUE)
	# init file names
	plot.file.non <- file.path(folder.non, paste0("comp_", gsub("/","__",g.split$name)))
	plot.file.split <- file.path(folder.split, paste0("comp_", gsub("/","__",g.non$name)))
	
	# retrieve vertex names
	vnames.non <- V(g.non)$idExterne
	vnames.split <- V(g.split)$idExterne
	# find split vertices in split graph
	v.split <- vnames.split[grepl("_",vnames.split,fixed=TRUE)]
	vnames.split <- setdiff(vnames.split, v.split)
	# find split vertices in non-split graph
	v.unsplit <- intersect(vnames.non, unique(sapply(strsplit(v.split, "_"),function(x) x[1])))
	vnames.non <- setdiff(vnames.non, v.unsplit)
	# count other vertices
	v.common <- intersect(vnames.split, vnames.non)
	v.only.non <- setdiff(vnames.non, vnames.split)
	v.only.split <- setdiff(vnames.split, vnames.non)
	
	# display results
	tlog(8,"Results:")
	#
	tlog(10,"Number of original vertices that are split: ",length(v.unsplit))
	tlog(10,"Number of such pieces in the split graph: ",length(v.split))
	#
	tlog(10,"Number of other vertices only in the non-split graph: ",length(v.only.non))
	tlog(10,"Number of other vertices only in the split graph: ",length(v.only.split))
	#
	tlog(10,"Number of other vertices common to both graphs: ",length(v.common))
	#
	tlog(10,"Total number of vertices in non-split graph: ",gorder(g.non))
	tlog(10,"Total number of vertices in split graph: ",gorder(g.split))
	
	# perform comparison
	att.non <- rep("Present",gorder(g.non))
	att.split <- rep("Present",gorder(g.split))
	att.non[V(g.non)$idExterne %in% v.only.non] <- "Absent"
	att.split[V(g.split)$idExterne %in% v.only.split] <- "Absent"
	V(g.non)$comparison <- att.non
	V(g.split)$comparison <- att.split
	
	# export vertex list as CSV file
	vnames <- sort(union(vnames.non,vnames.split))
	flags.g.non <- rep(FALSE,length(vnames ))
	flags.g.non[match(vnames.non,vnames )] <- TRUE
	flags.g.split <- rep(FALSE,length(vnames ))
	flags.g.split[match(vnames.split,vnames )] <- TRUE
	tab <- data.frame(vnames , flags.g.non, flags.g.split)
	colnames(tab) <- c("Id", g.non$name, g.split$name)
	tab.file <- paste0(plot.file.non,"_nodes.csv")
	tlog(8,"Record vertex list in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=FALSE)
	tab.file <- paste0(plot.file.split,"_nodes.csv")
	tlog(8,"Record vertex list in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=FALSE)
	
	# build result matrix
	rn <- c("Split vertices","Graph-specific vertices","Common vertices","Total")
	cn <- c(g.non$name,g.split$name)
	tab <- matrix(0,nrow=length(rn),ncol=length(cn))
	rownames(tab) <- rn
	colnames(tab) <- cn
	tab["Split vertices",] <- c(length(v.unsplit),length(v.split))
	tab["Graph-specific vertices",] <- c(length(v.only.non),length(v.only.split))
	tab["Common vertices",] <- rep(length(v.common), 2)
	tab["Total",] <- colSums(tab)
	print(tab)
	
	# record result matrix in both folders
	tab.file <- paste0(plot.file.non,".csv")
	tlog(8,"Record results in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=TRUE)
	tab.file <- paste0(plot.file.split,".csv")
	tlog(8,"Record results in file '",tab.file,"'")
	write.csv(tab, file=tab.file, row.names=TRUE)
	
	# create the geo plots
	plot.file <- paste0(plot.file.non,"_lambert")
	tlog(8,"Plotting in file '",plot.file,"'")
	g1 <- rescale.coordinates(g.non)
	custom.gplot(g=g1, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
	#
	plot.file <- paste0(plot.file.split,"_lambert")
	tlog(8,"Plotting in file '",plot.file,"'")
	g1 <- rescale.coordinates(g.split)
	custom.gplot(g=g1, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1, rescale=FALSE)
	
	# create the algo plots
	plot.file <- paste0(plot.file.non,"_algo")
	tlog(8,"Plotting in file '",plot.file,"'")
	V(g.non)$x <- V(g.non)$x2; V(g.non)$y <- V(g.non)$y2; E(g.non)$weight <- 0.5
	custom.gplot(g=g.non, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, rescale=FALSE, xlim=range(V(g.non)$x), ylim=range(V(g.non)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
	#
	plot.file <- paste0(plot.file.split,"_algo")
	tlog(8,"Plotting in file '",plot.file,"'")
	V(g.split)$x <- V(g.split)$x2; V(g.split)$y <- V(g.split)$y2; E(g.split)$weight <- 0.5
	custom.gplot(g=g.split, col.att="comparison", col.att.cap="Comparison", cat.att=TRUE, file=plot.file, rescale=FALSE, xlim=range(V(g.split)$x), ylim=range(V(g.split)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=8)
}




#############################################################################################
# Loads graphs corresponding to the removal of streets of decreasing length, and compute and
# plots some graph-related stats.
#############################################################################################
plot.street.removal <- function(mode)
{	tlog(2,"Plotting stats related to street removal: mode=",mode)
			
	folders <- c(GR_EST_FLAT_MINUS, paste0(GR_EST_FLAT_MINUS,"_filtered"))
#	folders <- c(GR_EST_FLAT_MINUS)
#	folders <- c(paste0(GR_EST_FLAT_MINUS,"_filtered"))
	for(f in 1:length(folders))
	{	folder <- folders[f]
		tlog(2,"Dealing with folder '",folder,"' (",f,"/",length(folders),")")
		
		# get folder path 
		main.folder <- file.path(FOLDER_OUT_ANAL_EST, mode, folder, "_removed_streets")
		graph.folder <- file.path(main.folder, "graphs")
		dist.folder <- file.path(main.folder, "dist_geodesic_vs_spatial")
		binned.folder <- file.path(dist.folder, "binned")
		dir.create(path=binned.folder, showWarnings=FALSE, recursive=TRUE)
		hexa.folder <- file.path(dist.folder, "hexa")
		dir.create(path=hexa.folder, showWarnings=FALSE, recursive=TRUE)
		muros.folder <- file.path(dist.folder, "muros")
		dir.create(path=muros.folder, showWarnings=FALSE, recursive=TRUE)
		
		# list graph files
		ll <- list.files(path=graph.folder, pattern="graph_rem=[0-9.]+\\.graphml", full.names=TRUE)
		tlog(4,"Found ",length(ll)," graph files in folder '",graph.folder,"'")
		
		# get street numbers or thresholds
		str <- basename(ll)
		street.vals <- as.numeric(substr(str, start=nchar("graph_rem=")+1, stop=nchar(str)-nchar(".graphml")))
		if(grepl("split", mode, fixed=TRUE))
			idx <- order(-street.vals)
		else
			idx <- order(street.vals)
		
		# read graphs
		tlog(4,"Reading graph files")
		gs <- list()
		street.names <- c()
		street.lengths <- c()
		street.spans <- c()
		street.cur.degs <- c()
		street.orig.degs <- c()
		for(i in idx)
		{	graph.file <- ll[i]
			tlog(6,"Reading file '",graph.file,"' (",length(gs)+1,"/",length(idx),")")
			g <- load.graphml.file(file=graph.file)
			r <- as.character(street.vals[i])
			gs[[r]] <- g 
			street.names <- c(street.names, g$LastDeletedStreetId)
			street.lengths <- c(street.lengths, g$LastDeletedStreetLength)
			street.spans <- c(street.spans, g$LastDeletedStreetSpan)
			street.cur.degs <- c(street.cur.degs, g$LastDeletedStreetCurrentDegree)
			street.orig.degs <- c(street.orig.degs, g$LastDeletedStreetOriginalDegree)
		}
		#sapply(gs,function(g) g$name)
		
		# record correlation for street measures
		cor.data <- cbind(street.lengths, street.spans, street.cur.degs, street.orig.degs)
		colnames(cor.data) <- c("Length", "Span", "CurrentDegree", "OriginalDegree")
		cor.mat <- cor(cor.data, use="pairwise.complete.obs")
		tab.file <- file.path(main.folder, "street_stats.csv")
		tlog(4,"Recording correlations in '",tab.file,"' (note: this does not work for the split graphs)")
		write.csv(cor.mat, file=tab.file, row.names=TRUE)
		print(cor.mat)
		
		# compute stats
		meas.names <- c(
			MEAS_NBR_NODES, "estate_nbr", MEAS_NBR_LINKS, 
			MEAS_NBR_COMPONENTS, 
			MEAS_COMMUNITY_NBR, MEAS_MODULARITY, MEAS_NMI, MEAS_RI, MEAS_ARI,
			MEAS_DISTANCE_AVG_SPATIAL, MEAS_DISTANCE_AVG_GEODESIC, MEAS_DISTANCE_HARM_SPATIAL, MEAS_DISTANCE_HARM_GEODESIC
		)
		cor.names <- c(
			paste0(MEAS_DISTANCE_COR_PEARSON,"-finite"), 
			paste0(MEAS_DISTANCE_COR_SPEARMAN,"-infinite"), paste0(MEAS_DISTANCE_COR_SPEARMAN,"-finite"), 
			paste0(MEAS_DISTANCE_COR_KENDALL,"-infinite"), paste0(MEAS_DISTANCE_COR_KENDALL,"-finite")
		)
		tab.stats <- data.frame(1:length(gs), street.names, street.lengths, street.spans, street.cur.degs, street.orig.degs, matrix(NA, nrow=length(gs), ncol=length(meas.names)))
		colnames(tab.stats) <- c("NumberDeletedStreets", "LastDeletedStreetId", "LastDeletedStreetLength", "LastDeletedStreetSpan", "LastDeletedStreetCurrentDegree", "LastDeletedStreetOriginalDegree", meas.names)
		tlog(4,"Computing stats")
		mbr.prev <- NA
		for(i in 1:nrow(tab.stats))
		{	tlog(6,"Processing graph #",i,"/",nrow(tab.stats))
			
			# counts
			tlog(8,"Compute node and edge counts")
			tab.stats[i,MEAS_NBR_NODES] <- gorder(gs[[i]])
			tab.stats[i,"estate_nbr"] <- length(which(V(gs[[i]])$typeExterne=="Bien"))
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
			idx0 <- which(!is.na(coords[,1]) & !is.na(coords[,2]) & !vertex_attr(gs[[i]], name=COL_LOC_EXCLUDE))
			svals <- as.matrix(dist(x=coords[idx0,], method="euclidean", diag=TRUE, upper=TRUE))
			diag(svals) <- NA
			svals <- svals[upper.tri(svals, diag=FALSE)]
			tab.stats[i,MEAS_DISTANCE_AVG_SPATIAL] <- mean(svals, na.rm=TRUE)
			tab.stats[i,MEAS_DISTANCE_HARM_SPATIAL] <- 1/mean(1/svals[svals>0], na.rm=TRUE)
			
			# geodesic distance
			tlog(8,"Compute geodesic distance")
			gvals <- distances(graph=gs[[i]], mode="all")
			diag(gvals) <- NA
			gvals <- gvals[upper.tri(gvals, diag=FALSE)]
			idx <- !is.infinite(gvals)
			tab.stats[i,MEAS_DISTANCE_AVG_GEODESIC] <- mean(gvals[idx], na.rm=TRUE)
			tab.stats[i,MEAS_DISTANCE_HARM_GEODESIC] <- 1/mean(1/gvals, na.rm=TRUE)
			
			# distance correlations
			tlog(8,"Compute distance correlation")
			gvals <- distances(graph=gs[[i]], mode="all", v=idx0, to=idx0)
			diag(gvals) <- NA
			gvals <- gvals[upper.tri(gvals, diag=FALSE)]
			idx <- !is.infinite(gvals)
			gvals2 <- gvals; gvals2[which(is.infinite(gvals))] <- rep(max(gvals[idx],na.rm=TRUE)+1, length(which(is.infinite(gvals))))	# values with max+1 instead of Inf (for rank-based correlation measures)
			tab.stats[i,paste0(MEAS_DISTANCE_COR_PEARSON,"-finite")] <- cor(x=gvals[idx], y=svals[idx], method="pearson")
			tab.stats[i,paste0(MEAS_DISTANCE_COR_SPEARMAN,"-infinite")] <- rcorr(x=gvals2, y=svals, type="spearman")$r[1,2]
			tab.stats[i,paste0(MEAS_DISTANCE_COR_SPEARMAN,"-finite")] <- rcorr(x=gvals[idx], y=svals[idx], type="spearman")$r[1,2]
			tab.stats[i,paste0(MEAS_DISTANCE_COR_KENDALL,"-infinite")] <- cor.fk(x=gvals2,y=svals)
			tab.stats[i,paste0(MEAS_DISTANCE_COR_KENDALL,"-finite")] <- cor.fk(x=gvals[idx], y=svals[idx])
			gvals <- gvals[idx]
			svals <- svals[idx]
			
			# plot geodesic vs. spatial distance using degree for color
			vals <- igraph::degree(graph=gs[[i]], mode="all", v=idx0)
			cb <- t(combn(1:length(idx0),2))
			vals <- (vals[cb[,1]] * vals[cb[,2]])[idx]
			xs <- sort(unique(gvals))
			avg.dist <- sapply(xs, function(x) mean(svals[gvals==x]))
			stdev.dist <- sapply(xs, function(x) sd(svals[gvals==x]))
			stdev.dist[is.na(stdev.dist)] <- 0
			# set colors
			fine <- 500 									# granularity of the color gradient
			cols <- viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))]
			# produce plot
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
				lines(	
					x=xs, y=avg.dist,
					col="BLACK"
				)
				# legend
				gradientLegend(range(vals), color=viridis(fine,direction=-1), inside=TRUE)
				dev.off()
			}
			
			# plot geodesic vs. spatial distance using intra- vs extra-muros for color
			ie.vals <- V(gs[[i]])[idx0]$loc
			cb <- t(combn(1:length(idx0),2))
			ie.vals <- apply(cb, 1, function(row) paste0((ie.vals[row]),collapse="--"))[idx]
# the plot is a mess, nothing to read there
#			# set colors
#			plain.col <- make.color.transparent("BLACK",75)
#			cols <- rep(plain.col, length(ie.vals))								# at least one NA
#			pal <- get.palette(val.nbr=3)[1:3]
#			cols[ie.vals=="intra--intra"] <- pal[1]								# two intra-muros
#			cols[ie.vals=="intra--extra" | ie.vals=="extra--intra"] <- pal[2]	# one intra- and one extra-muros
#			cols[ie.vals=="extra--extra"] <- pal[3]								# two extra-muros
#			ord <- which(cols!=plain.col)
#			# produce plot
#			plot.file <- file.path(muros.folder, paste0("dist_geodesic_vs_spatial_rem=",i))
#			tlog(8,"Plot geodesic vs. spatial distance in file '",plot.file,"'")
#			for(fformat in c("png"))	# FORMAT
#			{	if(fformat=="pdf")
#					pdf(paste0(plot.file,".pdf"))
#				else if(fformat=="png")
#					png(paste0(plot.file,".png"))
#				plot(
#					x=gvals[order(ord)], y=svals[order(ord)], 
#					xlab="Undirected geodesic distance", ylab="Spatial distance",
#					#log="xy", 
#					las=1, col=cols[order(ord)],
#					#xlim=c(1,max(deg.vals)*1.1)
#				)
#				# mean
#				lines(	
#					x=xs, y=avg.dist,
#					col="BLACK"
#				)
#				# legend
#				legend(
#					x="bottomright",
#					fill=c(pal,"BLACK"),
#					legend=c("intra--intra","intra--extra","extra--extra","Others")
#				)
#				dev.off()
#			}
			
			# plot geodesic vs. spatial distance as a binned scatterplot, distinguishing intra/extra muros pairs
			avg.dists <- list(); xss <- list(); stdev.dists <- list(); sles <- list(); rgs <- list()
			sles[[1]] <- ie.vals=="intra--intra"; sles[[2]] <- (ie.vals=="intra--extra" | ie.vals=="extra--intra"); sles[[3]] <- ie.vals=="extra--extra"
			sles[[4]] <- !sles[[1]] & !sles[[2]] & !sles[[3]]
			for(s in 1:length(sles))
			{	sle <- sles[[s]]
				xss[[s]] <- sort(unique(gvals[sle]))
				avg.dists[[s]] <- sapply(xss[[s]], function(x) mean(svals[sle][gvals[sle]==x]))
				stdev.dists[[s]] <- sapply(xss[[s]], function(x) sd(svals[sle][gvals[sle]==x]))
				stdev.dists[[s]][is.na(stdev.dists[[s]])] <- 0
				rgs[[s]] <- range(c(avg.dists[[s]]-stdev.dists[[s]],avg.dists[[s]]+stdev.dists[[s]]))
			}
			# colors
			plain.col <- make.color.transparent("BLACK",75)
			pal <- c(get.palette(val.nbr=3)[1:3],plain.col)
			# produce plot
			plot.file <- file.path(muros.folder, paste0("dist_geodesic_vs_spatial_rem=",i))
			tlog(8,"Plot muros version in '",plot.file,"'")
			for(fformat in c("png"))	# FORMAT
			{	if(fformat=="pdf")
					pdf(paste0(plot.file,".pdf"))
				else if(fformat=="png")
					png(paste0(plot.file,".png"))
				plot(
					NULL,
					xlab="Undirected geodesic distance", ylab="Spatial distance",
					las=1, #log="xy", 
					ylim=range(unlist(rgs),na.rm=TRUE),
					xlim=range(unlist(xss),na.rm=TRUE)
				)
				for(s in 1:length(xss))
				{	polygon(
						x=c(xss[[s]],rev(xss[[s]])), y=c(avg.dists[[s]]-stdev.dists[[s]],rev(avg.dists[[s]]+stdev.dists[[s]])), 
						col=make.color.transparent(pal[s],95), border=NA
					)
#					arrows(
#						x0=xss[[s]], y0=avg.dists[[s]]-stdev.dist[[s]], 
#						x1=xss[[s]], y1=avg.dists[[s]]+stdev.dist[[s]], 
#						code=3, angle=90, length=0.05, 
#						col=make.color.transparent(pal[s],85), lwd=2
#					)
					lines(
						x=xss[[s]], y=avg.dists[[s]], 
						col=pal[s], pch=19
					)
				}
				# legend
				legend(
					x="topleft",
					fill=c(pal,"BLACK"),
					legend=c("intra--intra","intra--extra","extra--extra","Others")
				)
				dev.off()
			}
			
			# plot geodesic vs. spatial distance as a binned scatterplot, no distinct color
			plot.file <- file.path(binned.folder, paste0("dist_geodesic_vs_spatial_rem=",i))
			tlog(8,"Plot binned version in  '",plot.file,"'")
			for(fformat in c("png"))	# FORMAT
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
#				arrows(
#					x0=xs, y0=avg.dist-stdev.dist, 
#					x1=xs, y1=avg.dist+stdev.dist, 
#					code=3, angle=90, length=0.05, 
#					col="PINK", lwd=2
#				)
				lines(
					x=xs, y=avg.dist, 
					col="RED", pch=19
				)
				dev.off()
			}
			
			# plot geodesic vs. spatial distance, but as an hexa map
			plot.file <- file.path(hexa.folder, paste0("dist_geodesic_vs_spatial_rem=",i))
			tlog(8,"Plot hexbin version in  '",plot.file,"'")
			for(fformat in c("png"))	# FORMAT
			{	if(fformat=="pdf")
					pdf(paste0(plot.file,".pdf"))
				else if(fformat=="png")
					png(paste0(plot.file,".png"))
				data <- data.frame(gvals, svals)
				avg.data <- data.frame(xs,avg.dist)
				p=ggplot(data, aes(x=gvals, y=svals)) +
					geom_hex(aes(colour=..count..)) + 
#					coord_fixed() +
					scale_fill_viridis(begin=0.1, limits=c(0,NA), aesthetics=c("colour", "fill")) + 
					theme_bw() +
					theme_classic() +	# base_size=18
					xlab("Undirected geodesic distance") + ylab("Spatial distance") +
					theme(legend.position="left") +
					geom_point(aes(x=gvals, y=svals), alpha=0) +
					geom_line(data=avg.data, aes(x=xs, y=avg.dist), size=0.75, color="WHITE")
#				ggMarginal(p, type="histogram", fill="GRAY")
				print(p)
				dev.off()
			}
		}
		
		# record stats
		tab.file <- file.path(main.folder, "stats.csv")
		tlog(4,"Recording stats in file '",tab.file,"'")
		write.csv(tab.stats, file=tab.file, row.names=FALSE)
		
		# plot correlations
		pal <- get.palette(val.nbr=length(cor.names))
		plot.file <- file.path(main.folder, "evolution_correlation")
		for(fformat in FORMAT)
		{	if(fformat=="pdf")
				pdf(paste0(plot.file,".pdf"))
			else if(fformat=="png")
				png(paste0(plot.file,".png"))
			plot(NULL, xlim=c(1,length(gs)),ylim=c(0,1),xlab="Number of streets removed (by decreasing length)", ylab="Correlation value")
			for(i in 1:length(cor.names))
			{	cor.name <- cor.names[i]
				lines(x=1:length(gs), y=tab.stats[,cor.name], col=pal[i])
			}
			legend(x="topleft", fill=pal, legend=cor.names)
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




#############################################################################################
# Perform street ablation for partial split graph types.
#
# mode: either "split_raw" or "split_ext".
#############################################################################################
partial.street.ablation <- function(mode)
{	tlog(2, "Performing pseudo street ablation for graph mode='",mode,"'")
	
	# read the list of splittable streets
	info.split <- load.location.table(FILE_IN_ANAL_SPLIT_FIX,"vertex")
	splittable <- paste0("Rue:",sort(unique(info.split[,COL_STREET_ID])))
	
	# read flat minus graph
	graph.file <- file.path(FOLDER_OUT_ANAL_EST, if(mode=="split_ext") "whole_ext" else "whole_raw", "flat_minus", FILE_GRAPH)
	tlog(4, "Reading graph '",graph.file,"'")
	g <- load.graphml.file(graph.file)
	
	# get the lengths of available and splittable streets
	available <- V(g)$idExterne[V(g)$typeExterne=="Rue"]
	streets <- intersect(splittable,available)
	lengths <- V(g)$length[which(V(g)$idExterne %in% streets)]
	lengths <- floor(sort(lengths[!is.na(lengths)], decreasing=TRUE)*10)/10
	# which(table(lengths)>1)		# check unicity
	
	# loop over the extraction function
	for(length in lengths)
		extract.estate.networks(split.surf=length,  compl.streets=mode=="split_ext", street.ablation=TRUE)
	
}
