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
			custom.gplot(g=g, col.att=paste0(fname,"_avg"), file=file.path(distance.folder,paste0(fname,"_avg_graph")))
			#custom.gplot(g=g, col.att=paste0(fname,"_avg"))
		}
		
		# for each node, plot graph using color for distance
		mode.folder <- file.path(distance.folder,mode)
		dir.create(path=mode.folder, showWarnings=FALSE, recursive=TRUE)
		for(n in 1:gorder(g))
		{	id <- vertex_attr(g, ND_NAME, n)
			nname <- get.names(g, n)
			nname <- trimws(gsub("?", "", nname, fixed=TRUE))
			
			# only for significant nodes
			if(degree(g, v=n, mode="all"))
				tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as its degree is <3")
			else
			{	g <- set_vertex_attr(graph=g, name=fname, value=vals[n,])
				if(all(is.infinite(vals[n,-n])))
					tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are infinite")
				else
				{	tlog(4,"Plotting graph for node #",id," (",nname, ", ",n,"/",gorder(g),")")
					g <- update.node.labels(g, vals[n,])
					custom.gplot(g=g, col.att=fname, v.hl=n, file=file.path(mode.folder,paste0("n",id,"_",nname)))
					#custom.gplot(g=g, col.att=fname, v.hl=n)
				}
				g <- delete_vertex_attr(graph=g, name=fname)
			}
		}
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}
