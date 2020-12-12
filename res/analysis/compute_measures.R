#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/compute_measures.R")
#############################################################################################




#############################################################
# Reads or inititalizes the statistic file, then returns the
# corresponding table.
#
# stat.file: file containing the statistics.
#
# returns: loaded or initialized table.
#############################################################
retrieve.stats <- function(stat.file)
{	if(file.exists(stat.file))
		stats <- read.csv(file=stat.file, header=TRUE, row.names=1)
	else
		stats <- data.frame(Value=as.numeric(),Mean=as.numeric(),Stdv=as.numeric())
	return(stats)
}



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
	modes <- c("undirected", "directed")
	for(mode in modes)
	{	tlog(2,"Computing diameter: mode=",mode)
		diam <- diameter(g, directed=mode=="directed")						# get the network diameter
		tlog(4,"Diameter: ",diam)
		dd <- distances(graph=g, mode=if(mode=="directed") "in" else "all")	# compute all inter-node distances
		idx <- which(dd==diam, arr.ind=TRUE)								# retrieve pairs of nodes matching the diameter
		if(mode=="undirected")
			idx <- idx[idx[,1]<idx[,2],,drop=FALSE]							# filter (each pair appears twice due to symmetric matrix)
		tlog(4,"Number of diameter paths: ",nrow(idx))
		
		# possibly create folder
		fname <- paste0("diameter_",mode)
		diameter.folder <- file.path(out.folder, g$name, "diameter", mode)
		dir.create(path=diameter.folder, showWarnings=FALSE, recursive=TRUE)
		
		# plot diameter
		diam.paths <- future_lapply(1:nrow(idx), function(r) 
			if(g$name!=LV_ESTATE)
				# looking for all the paths
				all_shortest_paths(graph=g, from=idx[r,1], to=idx[r,2], mode=if(mode=="directed") "in" else "all")$res
			else
				# too long to find all the paths...
				shortest_paths(graph=g, from=idx[r,1], to=idx[r,2], mode=if(mode=="directed") "in" else "all")$vpath
		)
		tlog(4,"Found ",length(diam.paths)," distinct diameters, plotting them")
		for(pp in 1:length(diam.paths))
		{	tlog(6,"Plotting diameter path ",pp,"/",length(diam.paths))
			V(g)$label <- rep(NA, gorder(g))
			custom.gplot(g=g, paths=diam.paths[[pp]], file=file.path(diameter.folder,paste0("diam_",mode,"_graph_",pp)))
			#custom.gplot(g=g, paths=diam.paths[[pp]])
			
			if(length(diam.paths[[pp]])<=20)
			{	tlog(8,"Plotting the ",length(diam.paths[[pp]])," variants of this diameter")			
				q <- 1
				for(p in 1:length(diam.paths[[pp]]))
				{	tlog(10,"Plotting variant ",p,"/",length(diam.paths[[pp]]))
					if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
					{	V(g)$label <- rep(NA,gorder(g))
						vstart <- diam.paths[[pp]][[p]][1]
						V(g)[vstart]$label <- get.names(g, vstart) 
						vend <- diam.paths[[pp]][[p]][length(diam.paths[[pp]][[p]])]
						V(g)[vend]$label <- get.names(g, vend) 
						custom.gplot(g=g, paths=diam.paths[[pp]][[p]], file=file.path(diameter.folder,paste0("diam_",mode,"_graph_",pp,"_",q)))
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
	modes <- c("undirected","in","out")
	long.names <- c("Undirected","Incoming","Outgoing")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		
		# compute eccentricity
		tlog(2,"Computing eccentricity: mode=",mode)
		vals <- eccentricity(g, mode=if(mode=="undirected") "all" else mode)
		
		# possibly create folder
		fname <- paste0("eccentricity_",mode)
		eccentricity.folder <- file.path(out.folder, g$name, "eccentricity")
		dir.create(path=eccentricity.folder, showWarnings=FALSE, recursive=TRUE)
		
		# plot distribution
		custom.hist(vals, name=paste(long.names[i],"Eccentricity"), file=file.path(eccentricity.folder,paste0(fname,"_histo")))
		
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
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(eccentricity.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname)
		
		# compute radius
		tlog(2,"Computing radius: mode=",mode)
		rad <- radius(g, mode=if(mode=="undirected") "all" else mode)
		#rad <- min(vals[vals>0])
		tlog(4,"Radius=",rad)
		
		# add radius to the graph (as attributes) and stats table
		fname <- paste0("radius_",mode)
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




#############################################################
# Computes the degree and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.degree <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","in","out")
	long.names <- c("Undirected","Incoming","Outgoing")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing degree: mode=",mode)
		
		# possibly create folder
		fname <- paste0("degree_",mode)
		degree.folder <- file.path(out.folder, g$name, "degree")
		dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
		
		# degree distribution
		vals <- igraph::degree(g, mode=if(mode=="undirected") "all" else mode)
		custom.hist(vals, name=paste(long.names[i],"Degree"), file=file.path(degree.folder,paste0(fname,"_histo")))
			
		# export CSV with degree
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(degree.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for degree
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(degree.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname)
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Computes Eigencentrality and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.eigencentrality <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","directed")
	long.names <- c("Undirected","Directed")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing Eigencentrality: mode=",mode)
		
		# possibly create folder
		fname <- paste0("eigencentrality_",mode)
		eigen.folder <- file.path(out.folder, g$name, "eigencentrality")
		dir.create(path=eigen.folder, showWarnings=FALSE, recursive=TRUE)
		
		# Eigencentrality distribution
		if(mode=="directed" && is_dag(g))
			vals <- rep(0, gorder(g))
		else
			vals <- eigen_centrality(graph=g, scale=FALSE, directed=mode=="directed")$vector
		custom.hist(vals, name=paste(long.names[i],"Eigencentrality"), file=file.path(eigen.folder,paste0(fname,"_histo")))
		
		# export CSV with Eigencentrality
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(eigen.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for Eigencentrality
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(eigen.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname)
	}
	
	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Computes betweenness and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.betweenness <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected", "directed")
	long.names <- c("Undirected","Directed")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing betweenness: mode=",mode)
		
		# possibly create folder
		fname <- paste0("betweenness_",mode)
		betweenness.folder <- file.path(out.folder,g$name,"betweenness")
		dir.create(path=betweenness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# betweenness distribution
		vals <- betweenness(graph=g, normalized=FALSE, directed=mode=="directed")
		custom.hist(vals, name=paste(long.names[i],"Betweenness"), file=file.path(betweenness.folder,paste0(fname,"_histo")))
		
		# export CSV with betweenness
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(betweenness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for betweenness
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(betweenness.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname)
	}
	
	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Computes closeness and generates plots and CSV files.
#
# If the graph has several components, the closeness is computed
# only for the largest one.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.closeness <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","in","out")
	long.names <- c("Undirected","Incoming","Outgoing")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing closeness: mode=",mode)
		
		# possibly create folder
		fname <- paste0("closeness_",mode)
		closeness.folder <- file.path(out.folder,g$name,"closeness")
		dir.create(path=closeness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# retrieve giant component, do not compute measure for the rest of the graph
		components <- components(graph=g, mode=if(mode=="undirected") "weak" else "strong")
		giant.comp.id <- which.max(components$csize)
		giant.comp.nodes <- which(components$membership==giant.comp.id)
		g.comp <- induced_subgraph(graph=g, giant.comp.nodes)
		
		# closeness distribution: only giant component
		vals <- rep(NA, vcount(g))
		vals[giant.comp.nodes] <- closeness(graph=g.comp, normalized=TRUE, mode=if(mode=="undirected") "all" else mode)
		vals[is.nan(vals)] <- NA
		custom.hist(vals, name=paste(long.names[i],"Closeness"), file=file.path(closeness.folder,paste0(fname,"_histo")))
		
		# export CSV with closeness
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(closeness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals,na.rm=TRUE))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals,na.rm=TRUE))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals,na.rm=TRUE), Stdv=sd(vals,na.rm=TRUE))
		
		# plot graph using color for closeness
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(closeness.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname)
		if(all(is.na(vals)))
			tlog(4,"WARNING: all values are NA, so no color in the plot")
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Computes harmonic closeness and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.harmonic.closeness <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","in","out")
	long.names <- c("Undirected","Incoming","Outgoing")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing harmonic closeness: mode=",mode)
		
		# possibly create folder
		fname <- paste0("closeness_harmo_",mode)
		closeness.folder <- file.path(out.folder,g$name,"closeness_harmo")
		dir.create(path=closeness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# harmonic closeness distribution: only giant component
		vals <- rep(NA, vcount(g))
		vals <- harmonic_centrality(x=g, mode=if(mode=="undirected") "all" else mode)/(length(which(igraph::degree(g)>0))-1)
		vals[is.nan(vals)] <- NA
		custom.hist(vals, name=paste(long.names[i],"Harmonic Closeness"), file=file.path(closeness.folder,paste0(fname,"_histo")))
		
		# export CSV with harmonic closeness
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
		colnames(df) <- c("Id","Name",fname) 
		write.csv(df, file=file.path(closeness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals,na.rm=TRUE))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals,na.rm=TRUE))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals,na.rm=TRUE), Stdv=sd(vals,na.rm=TRUE))
		
		# plot graph using color for harmonic closeness
		g <- update.node.labels(g, vals)
		custom.gplot(g=g, col.att=fname, file=file.path(closeness.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname)
		if(all(is.na(vals)))
			tlog(4,"WARNING: all values are NA, so no color in the plot")
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Computes transitivity and generates plots and CSV files.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.transitivity <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	tlog(2,"Computing transitivity")
	# possibly create folder
	fname <- "transitivity"
	transitivity.folder <- file.path(out.folder,g$name,"transitivity")
	dir.create(path=transitivity.folder, showWarnings=FALSE, recursive=TRUE)
	
	# transitivity distribution
	vals <- transitivity(graph=g, type="localundirected", isolates="zero")
	custom.hist(vals, name="Local Transitivity", file=file.path(transitivity.folder,paste0(fname,"_histo")))
	global <- transitivity(graph=g, type="globalundirected", isolates="zero")
	
	# export CSV with transitivity
	df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
	colnames(df) <- c("Id","Name",fname) 
	write.csv(df, file=file.path(transitivity.folder,paste0(fname,"_values.csv")), row.names=FALSE)
	
	# add results to the graph (as attributes) and stats table
	g <- set_vertex_attr(graph=g, name=fname, value=vals)
	g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
	g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
	g <- set_graph_attr(graph=g, name=paste0(fname,"_global"), value=global)
	stats[paste0(fname,"_local"), ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	stats[paste0(fname,"global"), ] <- list(Value=global, Mean=NA, Stdv=NA)
	
	# plot graph using color for transitivity
	g <- update.node.labels(g, vals)
	custom.gplot(g=g, col.att=fname, file=file.path(transitivity.folder,paste0(fname,"_graph")))
	#custom.gplot(g=g, col.att=fname)
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Detects the community structure of the network.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.comstruct <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","directed")
	long.names <- c("Undirected","Directed")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Detecting community structure: mode=",mode)
		
		# possibly create folder
		fname <- paste0("community_",mode)
		communities.folder <- file.path(out.folder,g$name,"communities",mode)
		dir.create(path=communities.folder, showWarnings=FALSE, recursive=TRUE)
		
		# detect communities
		#coms <- cluster_optimal(graph=simplify(g))		# much slower, obviously
		#coms <- cluster_spinglass(graph=simplify(g))
		#coms <- cluster_infomap(graph=simplify(g))
		coms <- cluster_edge_betweenness(graph=simplify(g), directed=mode)
		mod <- modularity(coms)
		mbrs <- as.integer(membership(coms))
		com.nbr <- length(unique(mbrs))
		tlog(4,"Number of communities: ",com.nbr)
		tlog(4,"Modularity: ",mod)
		
		# community size distribution
		sizes <- table(mbrs,useNA="ifany")
		custom.barplot(sizes, text=names(sizes), xlab="Community", ylab="Size", file=file.path(communities.folder,paste0(fname,"size_bars")))
		
		# export CSV with community membership
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), mbrs)
		colnames(df) <- c("Id","Name","Community") 
		write.csv(df, file=file.path(communities.folder,paste0(fname,"membership.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=mbrs)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_nbr"), value=com.nbr)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mod"), value=mod)
		stats[paste0(fname,"_nbr"), ] <- list(Value=com.nbr, Mean=NA, Stdv=NA)
		stats[paste0(fname,"_mod"), ] <- list(Value=mod, Mean=NA, Stdv=NA)
		
		# plot graph using color for communities
		V(g)$label <- rep(NA, gorder(g))
		custom.gplot(g=g, col.att=fname,cat.att=TRUE, file=file.path(communities.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname,cat.att=TRUE)
		
		# assess community purity for all attributes
		g <- analyze.net.comstruct.attributes(g, mode, out.folder)
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Compares the previously detected community structure and
# the attributes of the nodes.
#
# g: original graph to process.
# mode: directed or undirected.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.comstruct.attributes <- function(g, mode, out.folder)
{	tlog(2,"Comparing nodal attributes and communities")
	# possibly create folders
	coms.folder <- file.path(out.folder,g$name,"communities",mode)
	dir.create(path=coms.folder, showWarnings=FALSE, recursive=TRUE)
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	# retrieve community
	membership <- vertex_attr(graph=g, name=paste0("community_",mode))
	coms <- sort(unique(membership))
	
	# build community graph
	cg <- contract.vertices(g, mapping=membership)	# TODO we could keep edges of different types separted
	E(cg)$weight <- 1
	cg <- simplify(cg, remove.loops=FALSE)
	for(eattr in edge_attr_names(graph=cg))
		cg <- delete_edge_attr(graph=cg, name=eattr)
	# setup its attributes
	V(cg)$name <- paste("C",coms,sep="")
	V(cg)$label <- paste("C",coms,sep="")
	V(cg)$size <- sapply(coms, function(i) length(which(membership==i)))
	V(cg)$x <- sapply(coms, function(i) mean(V(g)$x[membership==i]))
	V(cg)$y <- sapply(coms, function(i) mean(V(g)$y[membership==i]))
	
	#############################
	# deal with categorical attributes
	
	# gather regular categorical attributes
	attrs <- intersect(COL_CAT, vertex_attr_names(g))
	for(attr in attrs)
	{	# attribute folder
		attr.folder <- file.path(coms.folder, attr)
		dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
		
		# get values
		tmp <- vertex_attr(g, attr)
		
		# export community-wise distributions as csv
		tlog(4,"Exporting community-wise distribution for attribute \"",attr,"\"")
		tmp <- factor(tmp)
		tt <- t(sapply(coms, function(i) table(tmp[membership==i], useNA="always")))
		colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
		tab <- as.data.frame(tt)
		tab <- cbind(coms, tab)
		colnames(tab)[1] <- "Community"
		tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
		
		# plot as graph with pie-charts as nodes
		tlog(4,"Plotting community graph with the distribution of \"",attr,"\"")
		for(c in 1:ncol(tt))
			cg <- set_vertex_attr(graph=cg, name=colnames(tt)[c], value=tt[,c])
		colors <- get.palette(length(levels(tmp))+1)[1:(length(levels(tmp))+1)]
		plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
		V(cg)$label <- rep(NA, gorder(cg))
		#plot(cg, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
		custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file)
		#custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
		
		# compute purity for each community
		pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
		tab <- as.data.frame(pur.tab)
		tab <- cbind(coms, tab)
		colnames(tab) <- c("Community", "Purity")
		tab.file <- file.path(attr.folder, paste0(attr,"_purity.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
		
		# compute global measures
		vals <- c()
		meas <- c()
			# purity
			pur.total <- sum(rowSums(tt)/gorder(g)*pur.tab)
			vals <- c(vals, pur.total)
			meas <- c(meas, "Purity")
			# chi-squared test of independence (dpt if p<0.05)
			chisq <- suppressWarnings(chisq.test(tmp, membership, correct=FALSE))$p.value # warning=communities too small
			vals <- c(vals, chisq)
			meas <- c(meas, "Chi2_pval")
			# Cramér's V
			cram <- CramerV(x=tmp, y=membership)
			vals <- c(vals, cram)
			meas <- c(meas, "C_V")
			# Goodman’s Kruskal Tau
			tau <- GKtau(membership, tmp)
			vals <- c(vals, tau$tauxy, tau$tauyx)
			meas <- c(meas, "GK_tau_Com->Att", "GK_tau_Att->Com")
		# record as a table
		tab <- data.frame(meas, vals)
		colnames(tab) <- c("Measure","Value")
		tab.file <- file.path(attr.folder, paste0(attr,"_association.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
	}
	
	# convert tag-type attributes
	attrs.lst <- list()
	#attrs.lst[[COL_PERS_TITLE_LAT1]] <- c(COL_PERS_TITLE_LAT1, COL_PERS_TITLE_LAT2)
	#attrs.lst[[COL_PERS_TITLE_FRE1]] <- c(COL_PERS_TITLE_FRE1, COL_PERS_TITLE_FRE2)
	attrs.lst[[COL_PERS_TITLE_NORM1]] <- c(COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2)
	#attrs.lst[[COL_PERS_OCC_LAT1]] <- c(COL_PERS_OCC_LAT1, COL_PERS_OCC_LAT2)
	#attrs.lst[[COL_PERS_OCC_FRE1]] <- c(COL_PERS_OCC_FRE1, COL_PERS_OCC_FRE2)
	attrs.lst[[COL_PERS_OCC_NORM1]] <- c(COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2)
	attrs.lst[[COL_EST_COMP_LAB1]] <- intersect(c(COL_EST_COMP_LAB1, COL_EST_COMP_LAB2, COL_EST_COMP_LAB3, COL_EST_COMP_LAB4, COL_EST_COMP_LAB5, COL_EST_COMP_LAB6),
			vertex_attr_names(g))
	attrs <- intersect(names(attrs.lst), vertex_attr_names(g))
	for(attr in attrs)
	{	tlog(4,"Processing attribute \"",attr,"\"")
		attrc <- attrs.lst[[attr]]
		m <- sapply(attrc, function(att) vertex_attr(g, att))
		idx.nas <- which(apply(m,1,function(r) all(is.na(r))))	# detect individuals with only NAs
		uvals <- sort(unique(c(m)))
		
		# processing all values at once
# TODO	
		# export community-wise distributions as csv
		tlog(4,"Exporting community-wise distribution for attribute \"",attr,"\"")
		tmp <- factor(tmp)
		tt <- t(sapply(coms, function(i) table(tmp[membership==i], useNA="always")))
		colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
		tab <- as.data.frame(tt)
		tab <- cbind(coms, tab)
		colnames(tab)[1] <- "Community"
		tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
		
		# plot as graph with pie-charts as nodes
		tlog(4,"Plotting community graph with the distribution of \"",attr,"\"")
		for(c in 1:ncol(tt))
			cg <- set_vertex_attr(graph=cg, name=colnames(tt)[c], value=tt[,c])
		colors <- get.palette(length(levels(tmp))+1)[1:(length(levels(tmp))+1)]
		plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
		V(cg)$label <- rep(NA, gorder(cg))
		#plot(cg, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
		custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file)
		#custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
		
		# compute purity for each community
		pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
		tab <- as.data.frame(pur.tab)
		tab <- cbind(coms, tab)
		colnames(tab) <- c("Community", "Purity")
		tab.file <- file.path(attr.folder, paste0(attr,"_purity.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
		
		# compute global measures
		vals <- c()
		meas <- c()
		# purity
		pur.total <- sum(rowSums(tt)/gorder(g)*pur.tab)
		vals <- c(vals, pur.total)
		meas <- c(meas, "Purity")
		# chi-squared test of independence (dpt if p<0.05)
		chisq <- suppressWarnings(chisq.test(tmp, membership, correct=FALSE))$p.value # warning=communities too small
		vals <- c(vals, chisq)
		meas <- c(meas, "Chi2_pval")
		# Cramér's V
		cram <- CramerV(x=tmp, y=membership)
		vals <- c(vals, cram)
		meas <- c(meas, "C_V")
		# Goodman’s Kruskal Tau
		tau <- GKtau(membership, tmp)
		vals <- c(vals, tau$tauxy, tau$tauyx)
		meas <- c(meas, "GK_tau_Com->Att", "GK_tau_Att->Com")
		# record as a table
		tab <- data.frame(meas, vals)
		colnames(tab) <- c("Measure","Value")
		tab.file <- file.path(attr.folder, paste0(attr,"_association.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
		
		
		
		
		
		
		# processing each value separately
		for(uval in uvals)
		{	# binarize tags
			tmp <- apply(m, 1, function(v) uval %in% v[!is.na(v)])
			idxt <- which(tmp)
			idxf <- which(!tmp)
			tmp[idxt] <- VAL_TRUE
			tmp[idxf] <- VAL_FALSE
			tmp[idx.nas] <- NA
			
			# setup folder
			short_val <- trimws(substr(uval,1,30))
			attr_val <- paste0(attr,"_",short_val)
			attr.folder <- file.path(coms.folder, attr, short_val)
			dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
			
			# export community-wise distributions as csv
			tlog(4,"Exporting community-wise distribution for attribute-value \"",attr_val,"\"")
			tmp <- factor(tmp)
			tt <- t(sapply(coms, function(i) table(tmp[membership==i], useNA="always")))
			colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
			tab <- as.data.frame(tt)
			tab <- cbind(coms, tab)
			colnames(tab)[1] <- "Community"
			tab.file <- file.path(attr.folder, paste0("distribution.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
			
			# plot as graph with pie-charts as nodes
			tlog(4,"Plotting community graph with the distribution of \"",attr_val,"\"")
			for(c in 1:ncol(tt))
				cg <- set_vertex_attr(graph=cg, name=colnames(tt)[c], value=tt[,c])
			colors <- get.palette(length(levels(tmp))+1)[1:(length(levels(tmp))+1)]
			plot.file <- file.path(attr.folder, paste0("comgraph"))
			V(cg)$label <- rep(NA, gorder(cg))
			#plot(cg, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
			custom.gplot(cg, col.att=colnames(tt), col.att.cap=paste0(attr," : ",uval), size.att="size", cat.att=TRUE, file=plot.file)
			#custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr_val, size.att="size", cat.att=TRUE)
			
			# compute purity for each community
			pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
			tab <- as.data.frame(pur.tab)
			tab <- cbind(coms, tab)
			colnames(tab) <- c("Community", "Purity")
			tab.file <- file.path(attr.folder, paste0("purity.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
			
			# compute global measures
			vals <- c()
			meas <- c()
			# purity
			pur.total <- sum(rowSums(tt)/gorder(g)*pur.tab)
			vals <- c(vals, pur.total)
			meas <- c(meas, "Purity")
			# chi-squared test of independence (dpt if p<0.05)
			chisq <- suppressWarnings(chisq.test(tmp, membership, correct=FALSE))$p.value # warning=communities too small
			vals <- c(vals, chisq)
			meas <- c(meas, "Chi2_pval")
			# Cramér's V
			cram <- CramerV(x=tmp, y=membership)
			vals <- c(vals, cram)
			meas <- c(meas, "C_V")
			# Goodman’s Kruskal Tau
			tau <- GKtau(membership, tmp)
			vals <- c(vals, tau$tauxy, tau$tauyx)
			meas <- c(meas, "GK_tau_Com->Att", "GK_tau_Att->Com")
			# record as a table
			tab <- data.frame(meas, vals)
			colnames(tab) <- c("Measure","Value")
			tab.file <- file.path(attr.folder, paste0("association.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
		}
	}
	
	#############################
	# deal with numerical attributes
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM, vertex_attr_names(g))
	for(attr in attrs)
	{	# get values
		att.vals <- vertex_attr(g, attr)
		
		# exports basic stats for each community
		tab <- t(sapply(coms, function(i) quantile(att.vals[membership==i])))
		tab <- cbind(tab, sapply(coms, function(i) mean(att.vals[membership==i])))
		colnames(tab)[ncol(tab)] <- "Mean"
		tab <- cbind(tab, sapply(coms, function(i) sd(att.vals[membership==i])))
		colnames(tab)[ncol(tab)] <- "Stdev"
		tab <- cbind(coms, tab)
		colnames(tab)[1] <- "Community"
		tab.file <- file.path(attr.folder, paste0(attr,"_stats.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
		
		# export community-wise distributions as csv
		tlog(4,"Exporting community-wise distribution for attribute \"",attr,"\"")
		tmp <- cut(att.vals, 5)
		tt <- t(sapply(coms, function(i) table(tmp[membership==i], useNA="always")))
		colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
		tab <- as.data.frame(tt)
		tab <- cbind(coms, tab)
		colnames(tab)[1] <- "Community"
		tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
		
		# plot as graph with pie-charts as nodes
		tlog(4,"Plotting community graph with the distribution of \"",attr,"\"")
		for(c in 1:ncol(tt))
			cg <- set_vertex_attr(graph=cg, name=colnames(tt)[c], value=tt[,c])
		colors <- get.palette(length(levels(tmp))+1)[1:(length(levels(tmp))+1)]
		plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
		V(cg)$label <- rep(NA, gorder(cg))
		#plot(cg, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
		custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file)
		#custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
		
#		# compute purity for each community
#		pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
#		tab <- as.data.frame(pur.tab)
#		tab <- cbind(coms, tab)
#		colnames(tab) <- c("Community", "Purity")
#		tab.file <- file.path(attr.folder, paste0(attr,"_purity.csv"))
#		write.csv(tab, file=tab.file, row.names=FALSE)
		
		# compute global measures
		vals <- c()
		meas <- c()
			# anova
			fit <- aov(att.vals[!is.na(att.vals)]~as.factor(membership[!is.na(att.vals)]))
			pval <- summary(fit)[[1]][["Pr(>F)"]][1]	# dirty workaround to get the p-value, see https://stackoverflow.com/questions/3366506/extract-p-value-from-aov
			vals <- c(vals, pval)
			meas <- c(meas, "ANOVA_pval")
			# eta 
			etas <- eta_sq(fit)$etasq
			vals <- c(vals, etas)
			meas <- c(meas, "Eta")
		# record as a table
		tab <- data.frame(meas, vals)
		colnames(tab) <- c("Measure","Value")
		tab.file <- file.path(attr.folder, paste0(attr,"_association.csv"))
		write.csv(tab, file=tab.file, row.names=FALSE)
	}
	
	#############################
	# community attributes over
	
	# record community graph
	graph.file <- file.path(coms.folder, "comgraph.graphml")
	write.graphml.file(g=cg, file=graph.file)
	
	return(g)
}




#############################################################
# Computes the assortativity of the network.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.assortativity <- function(g, out.folder)
{	tlog(2,"Computing the assortativity")
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	# init result table
	val.tab <- matrix(nrow=0,ncol=2)
	colnames(val.tab) <- c("undirected","directed")
	
	#############################
	# deal with categorical attributes
	tlog(4,"Dealing with categorical attributes")
	cat.data <- NA
	
	# gather regular categorical attributes 
	attrs <- intersect(COL_CAT, vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs.lst <- list()
	#attrs.lst[[COL_PERS_TITLE_LAT1]] <- c(COL_PERS_TITLE_LAT1, COL_PERS_TITLE_LAT2)
	#attrs.lst[[COL_PERS_TITLE_FRE1]] <- c(COL_PERS_TITLE_FRE1, COL_PERS_TITLE_FRE2)
	attrs.lst[[COL_PERS_TITLE_NORM1]] <- c(COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2)
	#attrs.lst[[COL_PERS_OCC_LAT1]] <- c(COL_PERS_OCC_LAT1, COL_PERS_OCC_LAT2)
	#attrs.lst[[COL_PERS_OCC_FRE1]] <- c(COL_PERS_OCC_FRE1, COL_PERS_OCC_FRE2)
	attrs.lst[[COL_PERS_OCC_NORM1]] <- c(COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2)
	attrs.lst[[COL_EST_COMP_LAB1]] <- intersect(c(COL_EST_COMP_LAB1, COL_EST_COMP_LAB2, COL_EST_COMP_LAB3, COL_EST_COMP_LAB4, COL_EST_COMP_LAB5, COL_EST_COMP_LAB6),
										vertex_attr_names(g))
	attrs <- intersect(names(attrs.lst), vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- attrs.lst[[attr]]
		m <- sapply(tmp, function(att) vertex_attr(g, att))
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	cat.data <- cbind(cat.data, as.integer(factor(apply(m, 1, function(v) uval %in% v[!is.na(v)]))))
			colnames(cat.data)[ncol(cat.data)] <- paste(attr,uval,sep="_")
		}
	}
	
	tlog(6,"Computing undirected vs. directed links")
	modes <- c("undirected", "directed")
	for(mode in modes)
	{	tlog(8,"Computing mode=",mode)
		
		# compute the assortativity for all categorical attributes
		for(i in 1:ncol(cat.data))
		{	# compute the assortativity
			attr <- colnames(cat.data)[i]
			tlog(10,"Computing attribute ",attr," (",i,"/",ncol(cat.data),")")
			
			# if there are some NAs
			if(any(is.na(cat.data[,i])))
			{	# explicitly represent NAs as a class
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)))
					ass <- NA
				else
				{	cd[is.na(cd)] <- max(cd,na.rm=TRUE) + 1
					ass <- assortativity_nominal(graph=g, types=cd, directed=mode=="directed")
				}
				tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when representing NAs explicitly: ",ass)
				name <- paste0(attr,"_expNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- ass
				# just ignore NAs
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)))
					ass <- NA
				else
				{	cd <- cd[!is.na(cd)]
					gg <- delete_vertices(g, which(is.na(cat.data[,i])))
					ass <- assortativity_nominal(graph=gg, types=cd, directed=mode=="directed")
				}
				tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",ass)
				name <- paste0(attr,"_noNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- ass
			}
			
			# no NA at all
			else
			{	cd <- as.integer(cat.data[,i])
				ass <- assortativity_nominal(graph=g, types=cd, directed=mode=="directed")
				tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,"): ",ass)
				name <- paste0(attr)
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- ass
			}
		}
	}
	
	#############################
	# deal with numerical attributes
	tlog(4,"Dealing with numerical attributes")
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM, vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# compute the assortativity for all numerical attributes
	if(!is.null(ncol(num.data)))
	{	tlog(6,"Computing undirected vs. directed links")
		modes <- c("undirected", "directed")
		for(mode in modes)
		{	tlog(8,"Computing mode=",mode)
			
			for(i in 1:ncol(num.data))
			{	# compute the assortativity
				attr <- colnames(num.data)[i]
				tlog(10,"Computing attribute ",attr," (",i,"/",ncol(num.data),")")
				
				# if there are some NAs
				if(any(is.na(num.data[,i])))
				{	# explicitly represent them as zeroes
					cd <- num.data[,i]
					if(all(is.na(cd)))
						ass <- NA
					else
					{	cd[is.na(cd)] <- 0
						ass <- assortativity(graph=g, types1=cd, directed=mode=="directed")
					}
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when replacing NAs by 0: ",ass)
					name <- paste0(attr,"_expNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
					# ignore them
					cd <- num.data[,i]
					if(all(is.na(cd)))
						ass <- NA
					else
					{	cd <- cd[!is.na(cd)]
						gg <- delete_vertices(g, which(is.na(num.data[,i])))
						ass <- assortativity(graph=gg, types1=cd, directed=mode=="directed")
					}
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",ass)
					name <- paste0(attr,"_noNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
				}
				# no NA at all
				else
				{	ass <- assortativity(graph=g, types1=num.data[,i], directed=mode=="directed")
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,"): ",ass)
					name <- paste0(attr)
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
				}
			}
		}
	}
	else
		tlog(6,"No numerical attribute detected")
	
	#############################
	# record the results
	res.file <- file.path(out.folder, g$name, "assortativity.csv")
	write.csv(val.tab, file=res.file, row.names=TRUE)
	
	#############################
	# assortativity over
	return(g)
}




#############################################################
# Computes stats related to the node attributes.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.attributes <- function(g, out.folder)
{	tlog(2,"Computing nodal attribute stats")
	# possibly create folders
	graph.folder <- file.path(out.folder, g$name)
	attr.folder <- file.path(graph.folder, "attributes")
	dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
	comp.folder <- file.path(attr.folder, "_comparison")
	dir.create(path=comp.folder, showWarnings=FALSE, recursive=TRUE)
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	#############################
	# deal with categorical attributes
	cat.data <- NA
	
	# gather regular categorical attributes
	attrs <- intersect(COL_CAT, vertex_attr_names(g))
	for(attr in attrs)
	{	# get values
		tmp <- vertex_attr(g, attr)
		
		# plot the attribute distribution as a barplot
		tlog(4,"Bar-plotting attribute \"",attr,"\"")
		tt <- table(tmp, useNA="ifany")
		plot.folder <- file.path(attr.folder, attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
		custom.barplot(tt, 
			text=names(tt), 
			xlab=LONG_NAME[attr], ylab="Frequence",
			file=plot.file)
		# record as a table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		
		# plot the graph using colors for attribute values
		tlog(4,"Graph-plotting attribute \"",attr,"\"")
		plot.file <- file.path(plot.folder, paste0(attr,"_graph"))
		V(g)$label <- rep(NA, gorder(g))
		custom.gplot(g=g, col.att=attr, cat.att=TRUE, color.isolates=TRUE, file=plot.file)
		#custom.gplot(g=g, col.att=attr, cat.att=TRUE, color.isolates=TRUE)
		
		# add to matrix
		tlog(4,"Adding attribute \"",attr,"\" to data matrix")
		if(all(is.na(cat.data)))
			cat.data <- matrix(tmp,ncol=1)
		else
			cat.data <- cbind(cat.data, tmp)
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs.lst <- list()
	#attrs.lst[[COL_PERS_TITLE_LAT1]] <- c(COL_PERS_TITLE_LAT1, COL_PERS_TITLE_LAT2)
	#attrs.lst[[COL_PERS_TITLE_FRE1]] <- c(COL_PERS_TITLE_FRE1, COL_PERS_TITLE_FRE2)
	attrs.lst[[COL_PERS_TITLE_NORM1]] <- c(COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2)
	#attrs.lst[[COL_PERS_OCC_LAT1]] <- c(COL_PERS_OCC_LAT1, COL_PERS_OCC_LAT2)
	#attrs.lst[[COL_PERS_OCC_FRE1]] <- c(COL_PERS_OCC_FRE1, COL_PERS_OCC_FRE2)
	attrs.lst[[COL_PERS_OCC_NORM1]] <- c(COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2)
	attrs.lst[[COL_EST_COMP_LAB1]] <- intersect(c(COL_EST_COMP_LAB1, COL_EST_COMP_LAB2, COL_EST_COMP_LAB3, COL_EST_COMP_LAB4, COL_EST_COMP_LAB5, COL_EST_COMP_LAB6),
										vertex_attr_names(g))
	attrs <- intersect(names(attrs.lst), vertex_attr_names(g))
	for(attr in attrs)
	{	attrc <- attrs.lst[[attr]]
		m <- sapply(attrc, function(att) vertex_attr(g, att))
		
		# count tag distribution
		idx.nas <- which(apply(m,1,function(r) all(is.na(r))))	# detect individuals with only NAs
		nbr.nas <- length(idx.nas) 								# count them
		dt <- c(m)[!is.na(c(m))]								# handles non-NA values
		dt <- c(dt,rep(NA,nbr.nas))								# insert the appropriate number of NAs
		# compute highest frequency for later use (to handle plot y-scale)
		tt <- table(dt, useNA="ifany")
		if(any(is.na(names(tt))))
			na.nbr <- tt[is.na(names(tt))]
		else
			na.nbr <- 0
		tmp <- sapply(tt, function(x) gorder(g)-x-na.nbr)
		ymax <- max(tmp,na.nbr)
		# identify least frequent values
		unfrequent <- names(tt)[which(tt<=2)]
		# plot tag distribution as barplot
		tlog(4,"Bar-plotting attributes containing \"",attr,"\"")
		plot.folder <- file.path(attr.folder, attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
		custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				file=plot.file)
		# record tag distribution as table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		# plot tags on a graph
#		if(attr==ATT_NODE_TRAV_DEST)
		{	gg <- g
			for(a in colnames(m))
			{	vals <- vertex_attr(g,a)
				vals[which(!is.na(match(vals,unfrequent)))] <- paste0(" ",VAL_OTHER) # represent all unfrequent value under an umbrella name
				g <- set_vertex_attr(g, a, value=vals)
			}
		}
		plot.file <- file.path(plot.folder, paste0(attr,"_graph"))
		V(g)$label <- rep(NA, gorder(g))
		custom.gplot(g=g, col.att=attrc, cat.att=TRUE, color.isolates=TRUE, file=plot.file)
		#custom.gplot(g=g, col.att=attrc, cat.att=TRUE, color.isolates=TRUE)
#		if(attr==ATT_NODE_TRAV_DEST)
			g <- gg
			
		# add to matrix
		tlog(4,"Adding attribute \"",attr,"\" to data matrix")
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	# binarize tags
			vals <- apply(m, 1, function(v) uval %in% v[!is.na(v)])
			idxt <- which(vals)
			idxf <- which(!vals)
			vals[idxt] <- VAL_TRUE
			vals[idxf] <- VAL_FALSE
			vals[idx.nas] <- NA
			cat.data <- cbind(cat.data, vals)
			att_name <- paste(attr,uval,sep="_")
			colnames(cat.data)[ncol(cat.data)] <- att_name
			
			# setup folder
			short_val <- trimws(substr(uval,1,30))
			plot.folder2 <- file.path(plot.folder, short_val)
			dir.create(path=plot.folder2, showWarnings=FALSE, recursive=TRUE)
			
			# produce TRUE/FALSE barplots
			tlog(6,"Producing barplots for attribute \"",att_name,"\"")
			tt <- table(vals, useNA="ifany")
			plot.file <- file.path(plot.folder2, "bars")
			custom.barplot(tt, 
				text=names(tt), 
				xlab=paste0(LONG_NAME[attr]," : ",uval), ylab="Frequence", 
				file=plot.file,
				ylim=c(0,ymax))
			# record values as table
			tt <- as.data.frame(tt)
			colnames(tt) <- c("Value","Frequency")
			table.file <- file.path(plot.folder2, "vals.csv")
			write.csv(tt, file=table.file, row.names=FALSE)
			
			# plot the graph using colors for attribute values
			tlog(6,"Graph-plotting attribute \"",att_name,"\"")
			plot.file <- file.path(plot.folder2, "graphs")
			gg <- set_vertex_attr(graph=g, name=att_name, value=vals)
			V(gg)$label <- rep(NA, gorder(gg))
			custom.gplot(g=gg, col.att=att_name, col.att.cap=paste0(LONG_NAME[attr]," : ",uval), cat.att=TRUE, color.isolates=TRUE, file=plot.file)
			#custom.gplot(g=gg, col.att=att_name, col.att.cap=paste0(LONG_NAME[attr]," : ",uval), cat.att=TRUE, color.isolates=TRUE)
		}
	}
	
	# replace NAs by "Unknown" tags
#	cat.data[which(is.na(cat.data))] <- VAL_UNK
	
	for(i in 1:ncol(cat.data))
	{	attr <- colnames(cat.data)[i]
		
		# plot one attribute versus another
		if(i<ncol(cat.data))
		{	for(j in (i+1):ncol(cat.data))
			{	attr2 <- colnames(cat.data)[j]
				vals1 <- cat.data[,i]
				vals2 <- cat.data[,j]
				tt <- table(vals1, vals2, useNA="ifany")
				names(dimnames(tt)) <- c(LONG_NAME[attr],LONG_NAME[attr2])
				shrt.attr1 <- substr(attr,1,30)		# to avoid long file names
				shrt.attr2 <- substr(attr2,1,30)	# same
				# plot file
				plot.file <- file.path(comp.folder, paste0(shrt.attr1,"_vs_",shrt.attr2,"_bars"))
				custom.barplot(vals=tt, 
						text=colnames(tt), 
						xlab=LONG_NAME[attr2], ylab="Frequence",
						file=plot.file)
				# record tag distribution as table
				tt <- as.data.frame(tt)
				table.file <- file.path(comp.folder, paste0(shrt.attr1,"_vs_",shrt.attr2,"_vals.csv"))
				write.csv(tt, file=table.file, row.names=FALSE)
			}
		}
	}
	
	#############################
	# deal with numerical attributes
	num.data <- NULL
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM, vertex_attr_names(g))
	for(attr in attrs)
	{	# get values
		tmp <- vertex_attr(g, attr)
		
		# plot the attribute distribution as a histogram 
		# (actually a barplot, for now, as the only numeric attribute is an integer)
		tlog(4,"Bar-plotting attribute \"",attr,"\"")
		tt <- table(tmp, useNA="ifany")
		plot.folder <- file.path(attr.folder, attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
		custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				file=plot.file)
		# record as a table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		
		# add to matrix
		if(is.null(num.data))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# replace NAs by "Unknown" tags
#	num.data[which(is.na(num.data))] <- VAL_UNK
	
	# plot the graph using colors for attribute values
	if(!is.null(num.data))
	{	for(i in 1:ncol(num.data))
		{	attr <- colnames(num.data)[i]
			tlog(4,"Plotting attribute \"",attr,"\"")
			gg <- set_vertex_attr(graph=g, name=attr, value=num.data[,i])
			V(gg)$label <- rep(NA, gorder(gg))
			plot.folder <- file.path(attr.folder, attr)
			plot.file <- file.path(plot.folder, paste0(attr,"_graph"))
			custom.gplot(g=gg, col.att=attr, cat.att=FALSE, color.isolates=TRUE, file=plot.file)
#			custom.gplot(g=gg, col.att=attr, cat.att=FALSE, color.isolates=TRUE)
		}
	}
	else
		tlog(4,"No numerical attribute detected")
	
	#############################
	# attributes over
	return(g)
}




#############################################################
# Recursively computes articulation points.
#
# An articulation point is a vertex whose removal makes the
# graph disconnected (i.e. it contains several components).
# This function recursively identifies them, i.e. it looks for
# first level articulation points, removes them, then look 
# for articulation points in the resulting components to get
# second level ones, and so on until no more articulation
# point is detected. Edge directions are ignored by igraph here.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.articulation <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	# init 
	tlog(2,"Computing articulation points")
	g1 <- g
	level <- 1
	art <- articulation_points(g1)
	art0 <- length(art)
	
	# repeat until no more articulation point
	while(length(art)>0)
	{	tlog(4,"Level ",level)
		# disconnect the articulation nodes
		g1 <- disconnect.nodes(g1, nodes=art)
		# mark them
		vals <- apply(cbind(rep(level,length(art)),V(g1)$Articulation[art]),1,function(v) min(v,na.rm=TRUE))
		V(g1)[art]$Articulation <- vals
		# proceed with the next level
		art <- articulation_points(g1)
		level <- level + 1
	}
	V(g1)$Articulation[is.na(V(g1)$Articulation)] <- level
	vals <- V(g1)$Articulation
	
	# possibly create folder
	articulation.folder <- file.path(out.folder,g$name,"articulation")
	dir.create(path=articulation.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot distribution
	custom.hist(vals, name="Articulation Point Levels", file=file.path(articulation.folder,"articulation_histo"))
	
	# export CSV with articulation
	df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), vals)
	colnames(df) <- c("Id","Name","articulation") 
	write.csv(df, file=file.path(articulation.folder,"articulation_values.csv"), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	g <- set_vertex_attr(graph=g, name="articulation", value=vals)
	g <- set_graph_attr(graph=g, name="articulation", value=art0)
	stats["articulation", ] <- list(Value=art0, Mean=NA, Stdv=NA)
	
	# plot graph using color for articulation level
	g <- update.node.labels(g, vals, best.low=TRUE)
	custom.gplot(g=g, col.att="articulation", file=file.path(articulation.folder,"articulation_graph"))
	#custom.gplot(g=g, col.att="articulation")
	
	# export CSV with number of articulation points
	write.csv(stats, file=stat.file, row.names=TRUE)

	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




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
	
	modes <- c("undirected","in","out")
	long.names <- c("Undirected","Incoming","Outgoing")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing distances: mode=",mode)
		
		# possibly create folder
		fname <- paste0("distance_",mode)
		distance.folder <- file.path(out.folder,g$name,"distance")
		dir.create(path=distance.folder, showWarnings=FALSE, recursive=TRUE)
		
		# distance distribution
		vals <- distances(graph=g, mode=if(mode=="undirected") "all" else mode)
		flat.vals <- vals[upper.tri(vals)]
		custom.hist(vals=flat.vals, paste(long.names[i],"Distance"), file=file.path(distance.folder,paste0(fname,"_histo")))
		
		# average distance distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name=paste("Average",long.names[i],"Distance"), file=file.path(distance.folder,paste0(fname,"_avg_histo")))
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




#############################################################
# Computes vertex connectivity and generates plots and CSV files.
#
# The connectivity of two vertices is the minimal number of vertices
# that should be removed in order to make the vertices disconnected.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.connectivity <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected", "directed")
	for(mode in modes)
	{	tlog(2,"Computing vertex connectivity: mode=",mode)
		
		# possibly create folder
		fname <- paste0("connectivity_",mode)
		connectivity.folder <- file.path(out.folder,g$name,"connectivity")
		dir.create(path=connectivity.folder, showWarnings=FALSE, recursive=TRUE)
		
		# compute connectivity
		vals <- matrix(NA, nrow=gorder(g), ncol=gorder(g))
		g2 <- if(mode=="directed") g else as.undirected(g)
		for(n in 1:(gorder(g2)-1))
		{	vals[n,n] <- 0
			neigh <- neighbors(graph=g2, v=n)
			for(n2 in (n+1):gorder(g2))
			{	if(n2 %in% neigh)
					tmp <- 1	# direct connexion: should be Inf, I guess? but igraph throws an error
				else
					tmp <- vertex_connectivity(
							graph=g2,
							source=n, target=n2)
				vals[n,n2] <- tmp
				vals[n2,n] <- vals[n,n2]
			}
		}
		vals[gorder(g),gorder(g)] <- 0
		flat.vals <- vals[upper.tri(vals)]
		custom.hist(vals=flat.vals, name="Connectivity", file=file.path(connectivity.folder,paste0(fname,"_histo")))
		# connectivity distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name="Connectivity", file=file.path(connectivity.folder,paste0(fname,"_avg_histo")))
		{	# export CSV with average connectivity
			df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), avg.vals)
			colnames(df) <- c("Id","Name",fname) 
			write.csv(df, file=file.path(connectivity.folder,paste0(fname,"_avg_values.csv")), row.names=FALSE)
			
			# add results to the graph (as attributes) and record
			g <- set_vertex_attr(graph=g, name=paste0(fname,"_avg"), value=avg.vals)
			g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
			g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
			stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
			
			# plot graph using color for average connectivity
			g <- update.node.labels(g, avg.vals)
			custom.gplot(g=g, col.att=paste0(fname,"_avg"), file=file.path(connectivity.folder,paste0(fname,"_avg_graph")))
			#custom.gplot(g=g, col.att=paste0(fname,"_avg"))
		}
		
		# for each node, plot graph using color for connectivity
		mode.folder <- file.path(connectivity.folder,mode)
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
				if(all(vals[n,]==0))
					tlog(4,"NOT plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),"), as all values are zero")
				else
				{	tlog(4,"Plotting graph for node #",id," (",nname,", ",n,"/",gorder(g),")")
					g <- update.node.labels(g, vals[n,])
					custom.gplot(g=g, col.att=fname, v.hl=n, file=file.path(mode.folder,paste0("n",id,"_",nname)))
					#custom.gplot(g=g, col.att=fname, v.hl=n)
				}
				g <- delete_vertex_attr(graph=g, name=fname)
			}
		}
	}
	
	# export CSV with average connectivity
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Identifies the weak and strong components of the graph.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.components <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	# numbers of nodes and edges
	stats[paste0("node_nbr"), ] <- list(Value=gorder(g), Mean=NA, Stdv=NA)
	stats[paste0("link_nbr"), ] <- list(Value=gsize(g), Mean=NA, Stdv=NA)
	
	# computing components
	modes <- c("undirected", "directed")
	for(mode in modes)
	{	tlog(2,"Computing components: mode=",mode)
		
		# possibly create folder
		fname <- paste0("components_",mode)
		components.folder <- file.path(out.folder,g$name,"components")
		dir.create(path=components.folder, showWarnings=FALSE, recursive=TRUE)
		
		# detect components
		cmp <- components(graph=g, mode=if(mode=="undirected") "weak" else "strong")
		mbrs <- cmp$membership
		comp.nbr <- cmp$no
		tlog(4,"Number of components: ",comp.nbr)
		
		# component size distribution
		sizes <- table(mbrs,useNA="ifany")
		custom.barplot(sizes, text=names(sizes), xlab="Component", ylab="Size", file=file.path(components.folder,paste0(fname,"size_bars")))
		
		# export CSV with component membership
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), mbrs)
		colnames(df) <- c("Id","Name","Component") 
		write.csv(df, file=file.path(components.folder,paste0(fname,"membership.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=mbrs)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_nbr"), value=comp.nbr)
		stats[paste0(fname,"_nbr"), ] <- list(Value=comp.nbr, Mean=NA, Stdv=NA)
		
		# continue with only the largest components
#		idx <- which(cmp$csize >= 0.1*gorder(g))	# keep only the components containing at least 10% of the nodes
		idx <- which(cmp$csize >= 10)				# too strict: switched to 10 nodes
		tlog(4,"Number of large components: ",length(idx))
		mbrs[is.na(match(mbrs,idx))] <- NA
		g <- set_vertex_attr(graph=g, name=fname, value=mbrs)
		
		# plot graph using color for components
		V(g)$label <- rep(NA, gorder(g))
		custom.gplot(g=g, col.att=fname, cat.att=TRUE, file=file.path(components.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname, cat.att=TRUE)
		g <- set_vertex_attr(graph=g, name=fname, value=cmp$membership)
	
		# plot components separately
		mode.folder <- file.path(components.folder, mode)
		dir.create(path=mode.folder, showWarnings=FALSE, recursive=TRUE)
		for(i in idx)
		{	# plot subgraph
			g2 <- induced_subgraph(graph=g, vids=which(mbrs==i))
			if(gorder(g2)>20)
				V(g2)$label <- rep(NA, gorder(g2))
			else
				V(g2)$label <- get.names(g2)
			custom.gplot(g=g2, file=file.path(mode.folder,paste0("component_",i)))
			#custom.gplot(g=g2)
			
			# export subgraph
			graph.file <- file.path(mode.folder,paste0("component_",i,".graphml"))
			write.graphml.file(g=g, file=graph.file)
		}
	}
	
	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Compute the correlation between component size and attribute values.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.components.corr <- function(g, out.folder)
{	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	# init result table
	val.tab <- matrix(nrow=0,ncol=2)
	colnames(val.tab) <- c("undirected","directed")
vals <- c()
	
	#############################
	# gathering categorical attributes
	tlog(4,"Gathering categorical attributes")
	cat.data <- NA
	
	# gather regular categorical attributes 
	attrs <- intersect(COL_CAT, vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs.lst <- list()
	#attrs.lst[[COL_PERS_TITLE_LAT1]] <- c(COL_PERS_TITLE_LAT1, COL_PERS_TITLE_LAT2)
	#attrs.lst[[COL_PERS_TITLE_FRE1]] <- c(COL_PERS_TITLE_FRE1, COL_PERS_TITLE_FRE2)
	attrs.lst[[COL_PERS_TITLE_NORM1]] <- c(COL_PERS_TITLE_NORM1, COL_PERS_TITLE_NORM2)
	#attrs.lst[[COL_PERS_OCC_LAT1]] <- c(COL_PERS_OCC_LAT1, COL_PERS_OCC_LAT2)
	#attrs.lst[[COL_PERS_OCC_FRE1]] <- c(COL_PERS_OCC_FRE1, COL_PERS_OCC_FRE2)
	attrs.lst[[COL_PERS_OCC_NORM1]] <- c(COL_PERS_OCC_NORM1, COL_PERS_OCC_NORM2)
	attrs.lst[[COL_EST_COMP_LAB1]] <- intersect(c(COL_EST_COMP_LAB1, COL_EST_COMP_LAB2, COL_EST_COMP_LAB3, COL_EST_COMP_LAB4, COL_EST_COMP_LAB5, COL_EST_COMP_LAB6),
			vertex_attr_names(g))
	attrs <- intersect(names(attrs.lst), vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- attrs.lst[[attr]]
		m <- sapply(tmp, function(att) vertex_attr(g, att))
		# create a NA vs. rest attribute
		cat.data <- cbind(cat.data, apply(m, 1, function(v) if(all(is.na(v))) 1 else 2))
		colnames(cat.data)[ncol(cat.data)] <- paste0(attr,"_NAvsRest")
		# decompose into a set of boolean attributes
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	cat.data <- cbind(cat.data, as.integer(factor(apply(m, 1, function(v) uval %in% v[!is.na(v)]))))
			colnames(cat.data)[ncol(cat.data)] <- paste(attr,uval,sep="_")
		}
	}
	
	#############################
	# gathering numerical attributes
	tlog(4,"Gathering numerical attributes")
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM, vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	#############################
	# computing correlation between component size and attribute values
	modes <- c("undirected", "directed")
	for(mode in modes)
	{	tlog(2,"Computing components: mode=",mode)
		
		# detect components
		cmp <- components(graph=g, mode=if(mode=="undirected") "weak" else "strong")
		mbrs <- cmp$membership
		comp.sizes <- cmp$csize[mbrs]
		
		# deal with categorical attributes
		tlog(4,"Dealing with categorical attributes")
		for(i in 1:ncol(cat.data))
		{	# compute the correlation
			attr <- colnames(cat.data)[i]
			tlog(6,"Computing attribute ",attr," (",i,"/",ncol(cat.data),")")
			
			# if there are some NAs
			if(any(is.na(cat.data[,i])))
			{	# explicitly represent NAs as a class
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)))
					cor <- NA
				else
				{	cd[is.na(cd)] <- max(cd,na.rm=TRUE) + 1
					fit <- aov(comp.sizes~as.factor(cd))
					cor <- eta_sq(fit)$etasq
				}
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,") when representing NAs explicitly: ",cor)
				name <- paste0(attr,"_expNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
				# just ignore NAs
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)))
					cor <- NA
				else
				{	cs <- comp.sizes[!is.na(cd)]
					cd <- cd[!is.na(cd)]
					if(length(unique(cd))==1)
						cor <- NA
					else
					{	fit <- aov(cs~as.factor(cd))
						cor <- eta_sq(fit)$etasq
					}
				}
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",cor)
				name <- paste0(attr,"_noNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
				# do NA vs. the rest
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)))
					cor <- NA
				else
				{	cd[!is.na(cd)] <- 1
					cd[is.na(cd)] <- 2
					fit <- aov(comp.sizes~as.factor(cd))
					cor <- eta_sq(fit)$etasq
				}
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,") when considering NAs vs the rest: ",cor)
				name <- paste0(attr,"_NAvsRest")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
			}
			
			# no NA at all
			else
			{	cd <- as.integer(cat.data[,i])
				fit <- aov(comp.sizes~as.factor(cd))
				cor <- eta_sq(fit)$etasq
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,"): ",cor)
				name <- paste0(attr)
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
			}
		}
		
		# deal with numerical attributes
		tlog(4,"Dealing with numerical attributes")
		if(!is.null(ncol(num.data)))
		{	for(i in 1:ncol(num.data))
			{	# compute the correlation
				attr <- colnames(num.data)[i]
				tlog(10,"Computing attribute ",attr," (",i,"/",ncol(num.data),")")
				
				# if there are some NAs
				if(any(is.na(num.data[,i])))
				{	# explicitly represent them as zeroes
					cd <- num.data[,i]
					if(all(is.na(cd)))
						cor <- NA
					else
					{	cd[is.na(cd)] <- 0
						cor <- cor(cd, comp.sizes)
					}
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,") when replacing NAs by 0: ",cor)
					name <- paste0(attr,"_expNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
					# ignore them
					cd <- num.data[,i]
					if(all(is.na(cd)))
						cor <- NA
					else
					{	cs <- comp.sizes[!is.na(cd)]
						cd <- cd[!is.na(cd)]
						cor <- cor(cd, cs)
					}
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",cor)
					name <- paste0(attr,"_noNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
					# do NA vs. the rest
					cd <- num.data[,i]
					if(all(is.na(cd)))
						cor <- NA
					else
					{	cd[!is.na(cd)] <- 1
						cd[is.na(cd)] <- 2
						fit <- aov(comp.sizes~as.factor(cd))
						cor <- eta_sq(fit)$etasq
					}
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,") when considering NAs vs the rest: ",cor)
					name <- paste0(attr,"_NAvsRest")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
				}
				# no NA at all
				else
				{	cor <- cor(num.data[,i], comp.sizes)
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,"): ",cor)
					name <- paste0(attr)
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
				}
			}
		}
	}
	
	# record the results
	res.file <- file.path(out.folder, g$name, "component-size_attribute_corr.csv")
	write.csv(val.tab, file=res.file, row.names=TRUE)
	
	return(g)
}




#############################################################
# Main method for the graph analysis. Generates a bunch of plots 
# and CSV files to store the results. Also updates the graphml
# file with the results whenever possible, for later external
# use.
#
# gname: name of the graph, used to get the path of its graphml file.
# out.folder: main output folder.
#
# returns: the graph with updated attributes.
#############################################################
analyze.network <- function(gname, out.folder)
{	# load graph
	file.path <- file.path(out.folder, gname, FILE_GRAPH)
	g <- load.graphml.file(file=file.path)
	
	if(gsize(g)>=30)
	{	# compute attribute stats 
		# (must be done first, before other results are added as attributes)
#		g <- analyze.net.attributes(g, out.folder)
#		
#		# compute diameters, eccentricity, radius
#		g <- analyze.net.eccentricity(g, out.folder)
#		
#		# compute degree
#		g <- analyze.net.degree(g, out.folder)
#		
#		# compute eigencentrality
#		g <- analyze.net.eigencentrality(g, out.folder)
#		
#		# compute betweenness
#		g <- analyze.net.betweenness(g, out.folder)
#		
#		# compute closeness
#		g <- analyze.net.closeness(g, out.folder)
#		
#		# compute harmonic closeness
#		g <- analyze.net.harmonic.closeness(g, out.folder)
#		
#		# compute distances
#		g <- analyze.net.distance(g, out.folder)
#		
#		# compute articulation points
#		g <- analyze.net.articulation(g, out.folder)
		
		# detect communities
		g <- analyze.net.comstruct(g, out.folder)
		
#		# compute transitivity
#		g <- analyze.net.transitivity(g, out.folder)
#		
#		# compute vertex connectivity
#		g <- analyze.net.connectivity(g, out.folder)
#		
#		# compute components
#		g <- analyze.net.components(g, out.folder)
#
#		# correlation between component size and attributes
#		g <- analyze.net.components.corr(g, out.folder)
#		
#		# compute assortativity
#		g <- analyze.net.assortativity(g, out.folder)
	}
	
	return(g)
}
