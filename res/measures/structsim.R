#############################################################################################
# Functions used during network analysis.
# 
# 10/2021 Vincent Labatut
# 
# source("res/measures/structsim.R")
#############################################################################################




#############################################################
# measure name
MEAS_STRUCT_SIM <- "structsim"
MEAS_LONG_NAMES[MEAS_STRUCT_SIM] <- "Structural Similarity"




#############################################################
# Computes the structural similarity and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.structsim <- function(g, out.folder)
{	#modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing structural similarity: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_STRUCT_SIM,"_",mode)
		sim.folder <- file.path(out.folder, g$name, MEAS_STRUCT_SIM)
		dir.create(path=sim.folder, showWarnings=FALSE, recursive=TRUE)
		
		
		###### compute structural similarity
		tlog(4,"Compute similarity values")
		
		# compute node-to-node similarity values
		vals <- similarity(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, method="jaccard")
		flat.vals <- vals[upper.tri(vals)]
		if(length(flat.vals)>2)
			custom.hist(vals=flat.vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_STRUCT_SIM]), file=file.path(sim.folder,paste0(fname,"_histo")))
		
		# record table of node pairs ranked by decreasing similarity
		unique.vals <- vals[upper.tri(vals)]
		idx <- which(upper.tri(vals),arr.ind=T)
		nids <- cbind(V(g)$idExterne[idx[,1]], V(g)$idExterne[idx[,2]])
		nnames <- cbind(get.names(g)[idx[,1]], get.names(g)[idx[,2]])
		df <- data.frame(nids, nnames, unique.vals, check.rows=FALSE, check.names=FALSE, stringsAsFactors=FALSE)
		colnames(df) <- c("Id1", "Id2", "Name1", "Name2", "Similarity")
		df <- df[order(unique.vals,decreasing=TRUE),]
		write.csv(df, file=file.path(sim.folder,paste0(fname,"_sim_ordered_values.csv")), row.names=FALSE)
		
		# for each node, plot graph using color for similarity
		tlog(4,"Plot individual nodes")
		mode.folder <- file.path(sim.folder,mode)
		dir.create(path=mode.folder, showWarnings=FALSE, recursive=TRUE)
		plot.folder <- file.path(mode.folder,"lambert")
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.folder <- file.path(mode.folder,"kk")
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		for(n in 1:gorder(g))
		{	id <- vertex_attr(g, ND_NAME, n)
			nname <- get.names(g, n)
			nname <- trimws(gsub("?", "", nname, fixed=TRUE))
			
			# only for significant nodes
			deg.lim <- 4
			if(igraph::degree(g, v=n, mode="all")<deg.lim)
				tlog(6,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as its degree is <",deg.lim)
			else
			{	g <- set_vertex_attr(graph=g, name=fname, value=vals[n,])
				if(all(is.infinite(vals[n,-n]) | is.nan(vals[n,-n])))
					tlog(6,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are infinite or NaN")
				else
				{	tlog(6,"Plotting graph for node #",id," (",nname, ", ",n,"/",gorder(g),")")
					shrt.nm <- substr(nname,1,30)		# to avoid long file names
					id.cln <- gsub(":", "-", id, fixed=TRUE)
					id.cln <- gsub("/", "_", id.cln, fixed=TRUE)
					V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
					g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
					custom.gplot(g=g1, col.att=fname, v.hl=n, file=file.path(mode.folder,"lambert",paste0(id.cln,"_",shrt.nm)), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
					g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
					custom.gplot(g=g1, col.att=fname, v.hl=n, file=file.path(mode.folder,"kk",paste0(id.cln,"_",shrt.nm)), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
				}
				g <- delete_vertex_attr(graph=g, name=fname)
			}
		}
		
		
		###### compare with spatial distance
		tlog(4,"Comparing structural similarity and spatial distances")
		sdists <- c("database","interpolation")			# only positions from the DB vs. all positions including estimates
		
		# init correlation table
		cor.tab <- matrix(NA,nrow=2,ncol=6)	
		cor.tab <- data.frame(cor.tab)
		cor.tab <- cbind(c("Database","Interpolation"), cor.tab)
		colnames(cor.tab) <- c("Coordinates", "PearsonCoef", "PearsonPval", "SpearmanCoef", "SpearmanPval", "KendallCoef", "KendallPval")
		rownames(cor.tab) <- sdists
		
		# compute spatial distances
		ylab <- c(database="Spatial distance (database)", interpolation="Spatial distance (interpolation)")
		for(sdist in sdists)
		{	tlog(6,"Computing spatial distance (",sdist,")")
			if(sdist=="database")
				coords <- cbind(vertex_attr(g, name=COL_LOC_HYP_LON), vertex_attr(g, name=COL_LOC_HYP_LAT))
			else
				coords <- cbind(V(g)$x, V(g)$y)
			idx <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
			rem <- which(is.na(coords[,1]) | is.na(coords[,2]))
			svals <- as.matrix(dist(x=coords[idx,], method="euclidean", diag=TRUE, upper=TRUE))
			svals <- svals[upper.tri(svals)]
			
			# filter structural similarity
			tlog(8,"Computing undirected geodesic distance")
			if(length(rem)>0) 
				gt <- delete_vertices(g,rem)
			else
				gt <- g
			gvals <- similarity(graph=gt, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, method="jaccard")
			gvals <- gvals[upper.tri(gvals)]
			dd <- as.matrix(distances(graph=gt, mode="all"))
			dd <- dd[upper.tri(dd)]
			idx <- !is.infinite(dd) & gvals>0	# we ignore pairs with infinite distance or zero structural sim 
			gvals <- gvals[idx]
			svals <- svals[idx]
			
			# compute correlations
			tlog(8,"Computing correlation between structural similarity and spatial distances (",length(gvals)," values vs. ",length(svals)," values)")
			if(length(gvals)<5)
				tlog(10,"WARNING: not enough values to compute correlation or produce plots")
			else
			{	tmp <- cor.test(x=gvals, y=svals, method="pearson")
				cor.tab[sdist,"PearsonCoef"] <- tmp$estimate
				cor.tab[sdist,"PearsonPval"] <- tmp$p.value
				tmp <- rcorr(x=gvals, y=svals, type="spearman")
				cor.tab[sdist,"SpearmanCoef"] <- tmp$r[1,2]
				cor.tab[sdist,"SpearmanPval"] <- tmp$P[1,2]
				tmp <- cor.test(x=gvals, y=svals, method="kendall")
				cor.tab[sdist,"KendallCoef"] <- tmp$estimate
				cor.tab[sdist,"KendallPval"] <- tmp$p.value
				
				# plot the spatial distance as a function of the structural similarity
				plot.file <- file.path(sim.folder, paste0(fname,"_vs_spatial_",sdist))
				pdf(paste0(plot.file,".pdf"))
					plot(
						x=gvals, y=svals, 
						xlab=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_STRUCT_SIM]), ylab=ylab[sdist],
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
		}
			
		# record correlations
		tab.file <- file.path(sim.folder, paste0(fname,"_vs_spatial_correlations.csv"))
		write.csv(cor.tab, file=tab.file, row.names=FALSE)
	}
	
	return(g)
}
