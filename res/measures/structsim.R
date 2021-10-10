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
		
		# compute node-to-node similarity values
		vals <- similarity(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, method="jaccard")
		flat.vals <- vals[upper.tri(vals)]
		custom.hist(vals=flat.vals, name=paste(MEAS_LONG_NAMES[mode],MEAS_LONG_NAMES[MEAS_STRUCT_SIM]), file=file.path(sim.folder,paste0(fname,"_histo")))
		
		# record table of node pairs ranked by decreasing similarity
		unique.vals <- vals[upper.tri(vals)]
		idx <- which(upper.tri(vals),arr.ind=T)
		nnames <- cbind(get.names(g)[idx[,1]], get.names(g)[idx[,2]])
		df <- data.frame(nnames, unique.vals, check.rows=FALSE, check.names=FALSE, stringsAsFactors=FALSE)
		colnames(df) <- c("Node1","Node2", "Similarity")
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
			if(igraph::degree(g, v=n, mode="all")<3)
				tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as its degree is <3")
			else
			{	g <- set_vertex_attr(graph=g, name=fname, value=vals[n,])
				if(all(is.infinite(vals[n,-n])) | is.nan(vals[n,-n]))
					tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are infinite or NaN")
				else
				{	tlog(4,"Plotting graph for node #",id," (",nname, ", ",n,"/",gorder(g),")")
					g <- update.node.labels(g, vals[n,])
					shrt.nm <- substr(nname,1,30)		# to avoid long file names
					id.cln <- gsub(":", "-", nname, fixed=TRUE)
					custom.gplot(g=g, col.att=fname, v.hl=n, file=file.path(mode.folder,paste0("n",id.cln,"_",shrt.nm)))
					#custom.gplot(g=g, col.att=fname, v.hl=n)
				}
				g <- delete_vertex_attr(graph=g, name=fname)
			}
		}
	}
	
	return(g)
}
