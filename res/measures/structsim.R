#############################################################################################
# Functions used during network analysis.
# 
# 10/2021 Vincent Labatut
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
{	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing structural similarity: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_STRUCT_SIM,"_",mode)
		sim.folder <- file.path(out.folder, g$name, MEAS_STRUCT_SIM)
		dir.create(path=sim.folder, showWarnings=FALSE, recursive=TRUE)
		
		
		###### compute structural similarity
		
		# compute node-to-node similarity values
		vals <- similarity(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, method="jaccard")
		flat.vals <- vals[upper.tri(vals)]
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
		mode.folder <- file.path(sim.folder,mode)
		dir.create(path=mode.folder, showWarnings=FALSE, recursive=TRUE)
		for(n in 1:gorder(g))
		{	id <- vertex_attr(g, ND_NAME, n)
			nname <- get.names(g, n)
			nname <- trimws(gsub("?", "", nname, fixed=TRUE))
			
			# only for significant nodes
			deg.lim <- 4
			if(igraph::degree(g, v=n, mode="all")<deg.lim)
				tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as its degree is <",deg.lim)
			else
			{	g <- set_vertex_attr(graph=g, name=fname, value=vals[n,])
				if(all(is.infinite(vals[n,-n])) | is.nan(vals[n,-n]))
					tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are infinite or NaN")
				else
				{	tlog(4,"Plotting graph for node #",id," (",nname, ", ",n,"/",gorder(g),")")
					g <- update.node.labels(g, vals[n,])
					shrt.nm <- substr(nname,1,30)		# to avoid long file names
					id.cln <- gsub(":", "-", id, fixed=TRUE)
					custom.gplot(g=g, col.att=fname, v.hl=n, file=file.path(mode.folder,paste0(id.cln,"_",shrt.nm)), size.att=2)
					#custom.gplot(g=g, col.att=fname, v.hl=n)
				}
				g <- delete_vertex_attr(graph=g, name=fname)
			}
		}
		
		
		###### compare with spatial distance
		
		tlog(4,"Comparing structural similarity and spatial distances")
		sdists <- c("reliable","estimate")	# only positions from the DB vs. all positions including estimates
		
		# init correlation table
		cor.tab <- matrix(NA,nrow=2,ncol=3)	
		cor.tab <- data.frame(cor.tab)
		cor.tab <- cbind(c("Reliable","Estimate"), cor.tab)
		colnames(cor.tab) <- c("Coordinates", "Pearson", "Spearman", "Kendall")
		rownames(cor.tab) <- sdists
		
		# compute spatial distances
		ylab <- c(reliable="Spatial distance (as defined)", estimate="Spatial distance (as estimated)")
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
			
			# filter structural similarity
			tlog(8,"Computing undirected graph distance")
			if(length(rem)>0) 
			{	gt <- delete_vertices(g,rem)
				gvals <- similarity(graph=gt, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, method="jaccard")
				gvals <- gvals[upper.tri(gvals)]
			}
			else
				gvals <- vals
			idx <- !is.infinite(gvals)
			gvals <- gvals[idx]
			svals <- svals[idx]
			
			# compute correlations
			tlog(8,"Computing correlation between structural similarity and spatial distances")
			cor.tab[sdist,"Pearson"] <- cor(x=gvals, y=svals, method="pearson")
			cor.tab[sdist,"Spearman"] <- cor(x=gvals, y=svals, method="spearman")
			cor.tab[sdist,"Kendall"] <- cor(x=gvals, y=svals, method="kendall")
			
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
		
		# record correlations
		tab.file <- file.path(sim.folder, paste0(fname,"_vs_spatial_correlations.csv"))
		write.csv(cor.tab, file=tab.file, row.names=FALSE)
	}
	
	return(g)
}
