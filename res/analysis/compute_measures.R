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
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.eccentricity <- function(g)
{	# get the stat table
	stat.file <- file.path(FOLDER_OUT_ANAL, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	# compute diameter
	modes <- c("undirected", "directed")
	for(mode in modes)
	{	tlog(2,"Computing diameter: mode=",mode)
		diam <- diameter(g, directed=mode=="directed")						# get the network diameter
		tlog(4,"Diameter=",diam)
		dd <- distances(graph=g, mode=if(mode=="directed") "in" else "all")	# compute all inter-node distances
		idx <- which(dd==diam, arr.ind=TRUE)								# retrieve pairs of nodes matching the diameter
		if(mode=="undirected")
			idx <- idx[idx[,1]<idx[,2],,drop=FALSE]							# filter (each pair appears twice due to symmetric matrix)
		
		# possibly create folder
		fname <- paste0("diameter_",mode)
		diameter.folder <- file.path(FOLDER_OUT_ANAL, g$name, "diameter", mode)
		dir.create(path=diameter.folder, showWarnings=FALSE, recursive=TRUE)
		
		# plot diameter
		diam.paths <- lapply(1:nrow(idx), function(r) 
			all_shortest_paths(graph=g, from=idx[r,1], to=idx[r,2], mode=if(mode=="directed") "in" else "all")$res)
		for(pp in 1:length(diam.paths))
		{	tlog(6,"Plotting diameter path ",pp,"/",length(diam.paths))
			V(g)$label <- rep(NA, gorder(g))
			custom.gplot(g, paths=diam.paths[[pp]], file=file.path(diameter.folder,paste0("diam_",mode,"_graph_",pp)))
			#custom.gplot(g, paths=diam.paths[[pp]])
			
			q <- 1
			for(p in 1:length(diam.paths[[pp]]))
			{	tlog(8,"Plotting variant ",p,"/",length(diam.paths[[pp]]))
				if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
				{	V(g)$label <- rep(NA,gorder(g))
					vstart <- diam.paths[[pp]][[p]][1]
					V(g)[vstart]$label <- vertex_attr(g, ND_NAME_FULL, vstart) 
					vend <- diam.paths[[pp]][[p]][length(diam.paths[[pp]][[p]])]
					V(g)[vend]$label <- vertex_attr(g, ND_NAME_FULL, vend) 
					custom.gplot(g, paths=diam.paths[[pp]][[p]], file=file.path(diameter.folder,paste0("diam_",mode,"_graph_",pp,"_",q)))
					q <- q + 1
				}
			}
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
		eccentricity.folder <- file.path(FOLDER_OUT_ANAL, g$name, "eccentricity")
		dir.create(path=eccentricity.folder, showWarnings=FALSE, recursive=TRUE)
		
		# plot distribution
		custom.hist(vals, name=paste(long.names[i],"Eccentricity"), file=file.path(eccentricity.folder,paste0(fname,"_histo")))
		
		# export CSV with eccentricity
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",fname) 
		write.csv(df, file=file.path(eccentricity.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add eccentricity (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for eccentricity
		g <- update.node.labels(g, vals)
		custom.gplot(g,col.att=fname,file=file.path(eccentricity.folder,paste0(fname,"_graph")))
		#custom.gplot(g,col.att=fname)
		
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
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	return(g)
}




#############################################################
# Computes the degree and generates plots and CSV files.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.degree <- function(g)
{	# get the stat table
	stat.file <- file.path(FOLDER_OUT_ANAL, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","in","out")
	long.names <- c("Undirected","Incoming","Outgoing")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing degree: mode=",mode)
		
		# possibly create folder
		fname <- paste0("degree_",mode)
		degree.folder <- file.path(FOLDER_OUT_ANAL, g$name, "degree")
		dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
		
		# degree distribution
		vals <- igraph::degree(g, mode=if(mode=="undirected") "all" else mode)
		custom.hist(vals, name=paste(long.names[i],"Degree"), file=file.path(degree.folder,paste0(fname,"_histo")))
			
		# export CSV with degree
		df <- data.frame(V(g)$name, V(g)$label, vals)
		colnames(df) <- c("Name","Label",fname) 
		write.csv(df, file=file.path(degree.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for degree
		g <- update.node.labels(g, vals)
		custom.gplot(g,col.att=fname,file=file.path(degree.folder,paste0(fname,"_graph")))
		#custom.gplot(g,col.att=fname)
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	return(g)
}




#############################################################
# Computes Eigencentrality and generates plots and CSV files.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.eigencentrality <- function(g)
{	# get the stat table
	stat.file <- file.path(FOLDER_OUT_ANAL, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","directed")
	long.names <- c("Undirected","Directed")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing Eigencentrality: mode=",mode)
		
		# possibly create folder
		fname <- paste0("eigencentrality_",mode)
		eigen.folder <- file.path(FOLDER_OUT_ANAL, g$name, "eigencentrality")
		dir.create(path=eigen.folder, showWarnings=FALSE, recursive=TRUE)
		
		# Eigencentrality distribution
		vals <- eigen_centrality(graph=g, scale=FALSE, directed=mode=="directed")$vector
		custom.hist(vals, name=paste(long.names[i],"Eigencentrality"), file=file.path(eigen.folder,paste0(fname,"_histo")))
		
		# export CSV with Eigencentrality
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",fname) 
		write.csv(df, file=file.path(eigen.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for Eigencentrality
		custom.gplot(g,col.att=fname,file=file.path(eigen.folder,paste0(fname,"_graph")))
		#custom.gplot(g,col.att=fname)
	}
	
	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	return(g)
}




#############################################################
# Computes betweenness and generates plots and CSV files.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.betweenness <- function(g)
{	# get the stat table
	stat.file <- file.path(FOLDER_OUT_ANAL, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected", "directed")
	long.names <- c("Undirected","Directed")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing betweenness: mode=",mode)
		
		# possibly create folder
		fname <- paste0("betweenness_",mode)
		betweenness.folder <- file.path(FOLDER_OUT_ANAL,g$name,"betweenness")
		dir.create(path=betweenness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# betweenness distribution
		vals <- betweenness(graph=g, normalized=FALSE, directed=mode=="directed")
		custom.hist(vals, name=paste(long.names[i],"Betweenness"), file=file.path(betweenness.folder,paste0(fname,"_histo")))
		
		# export CSV with betweenness
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",fname) 
		write.csv(df, file=file.path(betweenness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		
		# plot graph using color for betweenness
		custom.gplot(g,col.att=fname,file=file.path(betweenness.folder,paste0(fname,"_graph")))
		#custom.gplot(g,col.att=fname)
	}
	
	# export CSV with results
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	return(g)
}




#############################################################
# Computes closeness and generates plots and CSV files.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.closeness <- function(g)
{	# get the stat table
	stat.file <- file.path(FOLDER_OUT_ANAL, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c("undirected","in","out")
	long.names <- c("Undirected","Incoming","Outgoing")
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Computing closeness")
		
		# possibly create folder
		fname <- paste0("closeness_",mode)
		closeness.folder <- file.path(FOLDER_OUT_ANAL,g$name,"closeness")
		dir.create(path=closeness.folder, showWarnings=FALSE, recursive=TRUE)
		
		# retrieve giant component, do not compute measure for the rest of the graph
		components <- clusters(graph=g)
		giant.comp.id <- which.max(components$csize)
		giant.comp.nodes <- which(components$membership==giant.comp.id)
		g.comp <- induced_subgraph(graph=g, giant.comp.nodes)
		
		# closeness distribution: only giant component
		vals <- rep(NA, vcount(g))
		vals[giant.comp.nodes] <- closeness(graph=g.comp, normalized=TRUE, mode=if(mode=="undirected") "all" else mode)
		custom.hist(vals, name=paste(long.names[i],"Closeness"), file=file.path(closeness.folder,paste0(fname,"_histo")))
		
		# export CSV with closeness
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",fname) 
		write.csv(df, file=file.path(closeness.folder,paste0(fname,"_values.csv")), row.names=FALSE)
		
		# add degree (as node attributes) to the graph and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=vals)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mean"), value=mean(vals,na.rm=TRUE))
		g <- set_graph_attr(graph=g, name=paste0(fname,"_stdev"), value=sd(vals,na.rm=TRUE))
		stats[fname, ] <- list(Value=NA, Mean=mean(vals,na.rm=TRUE), Stdv=sd(vals,na.rm=TRUE))
		
		# plot graph using color for closeness
		g <- update.node.labels(g, vals)
		custom.gplot(g,col.att=fname,file=file.path(closeness.folder,paste0(fname,"_graph")))
		#custom.gplot(g,col.att=fname)
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	return(g)
}




#############################################################
# Computes transitivity and generates plots and CSV files.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.transitivity <- function(g)
{	tlog(2,"Computing transitivity")
	# possibly create folder
	transitivity.folder <- file.path(FOLDER_OUT_ANAL,g$name,"transitivity")
	dir.create(path=transitivity.folder, showWarnings=FALSE, recursive=TRUE)
	
	# transitivity distribution
	vals <- transitivity(graph=g, type="localundirected", isolates="zero")
	custom.hist(vals, name=LONG_NAME[MEAS_TRANSITIVITY], file=file.path(transitivity.folder,"transitivity_histo"))
	
	# export CSV with transitivity
	df <- data.frame(V(g)$name,V(g)$label,vals)
	colnames(df) <- c("Name","Label",MEAS_TRANSITIVITY) 
	write.csv(df, file=file.path(transitivity.folder,paste0("transitivity_values.csv")), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	V(g)$Transitivity <- vals
	g$Transitivity <- transitivity(graph=g, type="globalundirected", isolates="zero")
	g$TransitivityAvg <- mean(vals)
	g$TransitivityStdv <- sd(vals)
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# plot graph using color for transitivity
	custom.gplot(g,col.att=MEAS_TRANSITIVITY,file=file.path(transitivity.folder,"transitivity_graph"))
#	custom.gplot(g,col.att=MEAS_TRANSITIVITY)
	
	# export CSV with average transitivity
	stat.file <- file.path(FOLDER_OUT_ANAL,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df[MEAS_TRANSITIVITY, ] <- list(Value=g$Transitivity, Mean=mean(vals), Stdv=sd(vals))
	}
	else
	{	df <- data.frame(Value=c(g$Transitivity),Mean=c(mean(vals)),Stdv=c(sd(vals)))
		row.names(df) <- c(MEAS_TRANSITIVITY)
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	return(g)
}




#############################################################
# Detects the community structure of the network.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.comstruct <- function(g)
{	tlog(2,"Detecting community structure")
	# possibly create folder
	communities.folder <- file.path(FOLDER_OUT_ANAL,g$name,"communities")
	dir.create(path=communities.folder, showWarnings=FALSE, recursive=TRUE)
	
	tlog(4,"Processing graph ",i,"/",length(lst))
	op <- delete_edges(graph=g, edges=which(is.na(E(g)$Polarite) | E(g)$Polarite==ATT_VAL_NEGATIVE))	# TODO à adapter
	nn <- delete_edges(graph=g, edges=which(E(g)$Polarite==ATT_VAL_NEGATIVE))
	idx.op <- which(igraph::degree(op)>0)
	idx.nn <- which(igraph::degree(nn)>0)
	
	# community size distribution
#	coms.op <- cluster_optimal(graph=simplify(op))	# much slower, obviously
#	coms.op <- cluster_spinglass(graph=simplify(op))
	coms.op <- cluster_infomap(graph=simplify(op))
	mbrs.op <- as.integer(membership(coms.op))
	mbrs.op[-idx.op] <- NA
	sizes.op <- table(mbrs.op,useNA="ifany")
	custom.barplot(sizes.op, text=names(sizes.op), xlab=LONG_NAME[MEAS_COMMUNITY_ONLYPOS], ylab="Taille", file=file.path(communities.folder,"na-as-positive_community_size_bars"))
	#
	coms.nn <- cluster_infomap(graph=simplify(nn))
	mbrs.nn <- as.integer(membership(coms.nn))
	mbrs.nn[-idx.nn] <- NA
	sizes.nn <- table(mbrs.nn,useNA="ifany")
	custom.barplot(sizes.nn, text=names(sizes.nn), xlab=LONG_NAME[MEAS_COMMUNITY_NONEG], ylab="Taille", file=file.path(communities.folder,"na-ignored_community_size_bars"))
	
	# export CSV with community membership
	df <- data.frame(V(op)$name,V(op)$label,mbrs.op)
	colnames(df) <- c("Name","Label",MEAS_COMMUNITY_ONLYPOS) 
	write.csv(df, file=file.path(communities.folder,paste0("na-as-positive_community_membership.csv")), row.names=FALSE)
	#
	df <- data.frame(V(nn)$name,V(nn)$label,mbrs.nn)
	colnames(df) <- c("Name","Label",MEAS_COMMUNITY_NONEG) 
	write.csv(df, file=file.path(communities.folder,paste0("na-ignored_community_membership.csv")), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	mod.op <- modularity(coms.op)
	op <- set_vertex_attr(graph=op,name=MEAS_COMMUNITY_ONLYPOS,value=mbrs.op)
	op <- set_graph_attr(graph=op,name=MEAS_MODULARITY_ONLYPOS,value=mod.op)
	tlog(4,"Modularity when including NAs as positive links: ",mod.op)
	#
	mod.nn <- modularity(coms.nn)
	nn <- set_vertex_attr(graph=nn,name=MEAS_COMMUNITY_NONEG,value=mbrs.nn)
	nn <- set_graph_attr(graph=nn,name=MEAS_MODULARITY_NONEG,value=mod.nn)
	tlog(4,"Modularity when ignoring NAs: ",mod.nn)
	#
	g <- set_vertex_attr(graph=g,name=MEAS_COMMUNITY_ONLYPOS,value=mbrs.op)
	g <- set_graph_attr(graph=g,name=MEAS_MODULARITY_ONLYPOS,value=mod.op)
	g <- set_vertex_attr(graph=g,name=MEAS_COMMUNITY_NONEG,value=mbrs.nn)
	g <- set_graph_attr(graph=g,name=MEAS_MODULARITY_NONEG,value=mod.nn)
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# plot graph using color for communities
	custom.gplot(op,col.att=MEAS_COMMUNITY_ONLYPOS,cat.att=TRUE,file=file.path(communities.folder,"na-as-positive_communities_graph"))
#	custom.gplot(op,col.att=MEAS_COMMUNITY_ONLYPOS,cat.att=TRUE)
	custom.gplot(nn,col.att=MEAS_COMMUNITY_NONEG,cat.att=TRUE,file=file.path(communities.folder,"na-ignored_communities_graph"))
#	custom.gplot(nn,col.att=MEAS_COMMUNITY_NONEG,cat.att=TRUE)
	
	# export CSV with modularity
	stat.file <- file.path(FOLDER_OUT_ANAL,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df[MEAS_MODULARITY_ONLYPOS, ] <- list(Value=mod.op, Mean=NA, Stdv=NA)
	}
	else
	{	df <- data.frame(Value=c(mod.op),Mean=c(NA),Stdv=c(NA))
		row.names(df) <- c(MEAS_MODULARITY_ONLYPOS)
	}
	df[MEAS_MODULARITY_NONEG, ] <- list(Value=mod.nn, Mean=NA, Stdv=NA)
	write.csv(df, file=stat.file, row.names=TRUE)
	
	return(g)
}




#############################################################
# Computes the assortativity of the network.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.assortativity <- function(g)
{	tlog(2,"Computing the assortativity")
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	#############################
	# deal with categorical attributes
	cat.data <- NA
	
	# TODO à addapter
	# gather regular categorical attributes 
	attrs <- c(ATT_NODE_REL_TRAJ, ATT_NODE_REL_HADR,			# relationships
			ATT_NODE_SEN_POLDER, ATT_NODE_EQU_POLDER,			# last political positions
			ATT_NODE_LACTICLAVIUS, ATT_NODE_SPANISH)			# misc
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- c(ATT_NODE_SEN_POL, ATT_NODE_SEN_MILIT,			# senatorial positions
			ATT_NODE_EQU_POL, ATT_NODE_EQU_MILIT,				# equestrian positions
			ATT_NODE_TRAV_DEST, ATT_NODE_TRAV_REAS,				# travels
			ATT_NODE_CIRCLES)									# circles
	for(attr in attrs)
	{	tmp <- att.list[grepl(att.list,pattern=attr)]
		m <- sapply(tmp, function(att) vertex_attr(g, att))
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	cat.data <- cbind(cat.data, as.integer(factor(apply(m, 1, function(v) uval %in% v[!is.na(v)]))))
			colnames(cat.data)[ncol(cat.data)] <- paste(attr,uval,sep="_")
		}
	}
	
	# compute the assortativity for all categorical attributes
	vals <- c()
	for(i in 1:ncol(cat.data))
	{	# compute the assortativity
		attr <- colnames(cat.data)[i]
		
		# if there are some NAs
		if(any(is.na(cat.data[,i])))
		{	# explicitly represent them as a class
			cd <- cat.data[,i]
			cd[is.na(cd)] <- max(cd,na.rm=TRUE) + 1
			ass <- assortativity_nominal(graph=g, types=cd)
			tlog(4,"Assortativity for attribute \"",attr,"\" when representing NAs by 0: ",ass)
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_explicitNA",sep="")
			# ignore them
			cd <- cat.data[,i]
			cd <- cd[!is.na(cd)]
			if(length(cd)>1)
			{	gg <- delete_vertices(g, which(is.na(cat.data[,i])))
				ass <- assortativity_nominal(graph=gg, types=cd)
			}
			else
				ass <- NA
			tlog(4,"Assortativity for attribute \"",attr,"\" when ignoring NAs: ",ass)
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_noNA",sep="")
		}
		
		# no NA at all
		else
		{	ass <- assortativity_nominal(graph=g, types=cat.data[,i])
			tlog(4,"Assortativity for attribute \"",attr,"\": ",ass)
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- attr
		}
	}
	
	#############################
	# deal with numerical attributes
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- c(ATT_NODE_TRAV_NBR)					# number of travels
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# compute the assortativity for all numerical attributes
	for(i in 1:ncol(num.data))
	{	# compute the assortativity
		attr <- colnames(num.data)[i]
		
		# if there are some NAs
		if(any(is.na(num.data[,i])))
		{	# explicitly represent them as zeroes
			cd <- num.data[,i]
			cd[is.na(cd)] <- 0
			ass <- assortativity(graph=g, types1=cd)
			tlog(4,"Assortativity for attribute \"",attr,"\" when replacing NAs by 0: ",ass)
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_explicitNA",sep="")
			# ignore them
			cd <- num.data[,i]
			cd <- cd[!is.na(cd)]
			if(length(cd)>1)
			{	gg <- delete_vertices(g, which(is.na(num.data[,i])))
				ass <- assortativity(graph=gg, types1=cd)
			}
			else
				ass <- NA
			tlog(4,"Assortativity for attribute \"",attr,"\" when ignoring NAs: ",ass)
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_noNA",sep="")
		}
		# no NA at all
		else
		{	ass <- assortativity(graph=g, types1=num.data[,i])
			tlog(4,"Assortativity for attribute \"",attr,"\": ",ass)
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- attr
		}
	}
	
	#############################
	# record the results
	
	# add results to the graph (as attributes) and record
	for(i in 1:length(vals))
	{	attr <- names(vals)[i]
		g <- set_vertex_attr(graph=g, name=attr, value=vals[i])
	}
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# add assortativity to main CSV
	stat.file <- file.path(FOLDER_OUT_ANAL,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		for(i in 1:length(vals))
		{	attr <- names(vals)[i]
			df[attr, ] <- list(Value=vals[i], Mean=NA, Stdv=NA)
		}
	}
	else
	{	df <- data.frame(Value=c(vals[i]),Mean=c(NA),Stdv=c(NA))
		row.names(df) <- c(names(vals)[1])
		for(i in 2:length(vals))
		{	attr <- names(vals)[i]
			df[attr, ] <- list(Value=vals[i], Mean=NA, Stdv=NA)
		}
		
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	#############################
	# assortativity over
	return(g)
}




#############################################################
# Computes stats related to the node attributes.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.attributes <- function(g)
{	tlog(2,"Computing nodal attribute stats")
	# possibly create folders
	graph.folder <- file.path(FOLDER_OUT_ANAL, g$name)
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
	attrs <- c(ND_ECCL, ND_HEALTH, ND_HOMETOWN, ND_HOMEDIOC, ND_GENDER, 
		ND_NAME_FIRST, ND_NAME_LAST, ND_NAME_NICK, ND_RESIDENCE, ND_STATUS)
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
		
		# add to matrix
		tlog(4,"Adding attribute \"",attr,"\" to data matrix")
		if(all(is.na(cat.data)))
			cat.data <- matrix(tmp,ncol=1)
		else
			cat.data <- cbind(cat.data, tmp)
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- c(	ND_JOB, ND_TITLE)
	for(attr in attrs)
	{	tmp <- att.list[grepl(att.list,pattern=attr)]
		m <- sapply(tmp, function(att) vertex_attr(g, att))
		
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
		plot.file <- file.path(plot.folder,paste0(attr,"_bars"))
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
				vals[which(!is.na(match(vals,unfrequent)))] <- paste0(" ",VAL_OTHER)
				g <- set_vertex_attr(g, a, value=vals)
			}
		}
		custom.gplot(g=g, col.att=attr, cat.att=TRUE, color.isolates=TRUE, file=file.path(plot.folder,paste0(attr,"_1graph")))
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
			
			# produce TRUE/FALSE barplots
			tt <- table(vals, useNA="ifany")
			plot.file <- file.path(plot.folder, paste0(att_name,"_bars"))
			custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[att_name], ylab="Frequence", 
				file=plot.file,
				ylim=c(0,ymax))
			# record values as table
			tt <- as.data.frame(tt)
			colnames(tt) <- c("Value","Frequency")
			table.file <- file.path(plot.folder,paste0(att_name,"_vals.csv"))
			write.csv(tt, file=table.file, row.names=FALSE)
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
		
		# plot the graph using colors for attribute values
		tlog(4,"Graph-plotting attribute \"",attr,"\"")
		gg <- set_vertex_attr(graph=g, name=attr, value=cat.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE,file=file.path(attr.folder,paste0(attr,"_2graph")))
#		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE)
	}
	
#	#############################
#	# deal with numerical attributes
#	num.data <- NA
#	
#	# gather regular numerical attributes
#	attrs <- c(ATT_NODE_TRAV_NBR)
#	for(attr in attrs)
#	{	# get values
#		tmp <- vertex_attr(g, attr)
#		
#		# plot the attribute distribution as a histogram 
#		# (actually a barplot, for now, as the only numeric attribute is an integer)
#		tlog(4,"Bar-plotting attribute \"",attr,"\"")
#		tt <- table(tmp, useNA="ifany")
#		plot.folder <- file.path(attr.folder, attr)
#		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
#		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
#		custom.barplot(tt, 
#				text=names(tt), 
#				xlab=LONG_NAME[attr], ylab="Frequence", 
#				file=plot.file)
#		# record as a table
#		tt <- as.data.frame(tt)
#		colnames(tt) <- c("Value","Frequency")
#		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
#		write.csv(tt, file=table.file, row.names=FALSE)
#		
#		# add to matrix
#		if(all(is.na(num.data)))
#			num.data <- matrix(tmp,ncol=1)
#		else
#			num.data <- cbind(num.data, tmp)
#		colnames(num.data)[ncol(num.data)] <- attr
#	}
#	
#	# replace NAs by "Unknown" tags
##	num.data[which(is.na(num.data))] <- VAL_UNK
#	
#	# plot the graph using colors for attribute values
#	for(i in 1:ncol(num.data))
#	{	attr <- colnames(num.data)[i]
#		tlog(4,"Plotting attribute \"",attr,"\"")
#		gg <- set_vertex_attr(graph=g, name=attr, value=num.data[,i])
#		custom.gplot(gg,col.att=attr,cat.att=FALSE,color.isolates=TRUE,file=file.path(attr.folder,paste0(attr,"_graph")))
##		custom.gplot(gg,col.att=attr,cat.att=FALSE,color.isolates=TRUE)
#	}
	
	#############################
	# attributes over
	return(g)
}




#############################################################
# Recursively computes articulation points.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.articulation <- function(g)
{	# init 
	tlog(2,"Computing articulation points")
	g1 <- g
	level <- 1
	art <- articulation_points(g1)
	
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
	articulation.folder <- file.path(FOLDER_OUT_ANAL,g$name,"articulation")
	dir.create(path=articulation.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot distribution
	custom.hist(vals, name=LONG_NAME[MEAS_ARTICULATION], file=file.path(articulation.folder,"articulation_histo"))
	
	# export CSV with articulation
	df <- data.frame(V(g)$name,V(g)$label,vals)
	colnames(df) <- c("Name","Label",MEAS_ARTICULATION) 
	write.csv(df, file=file.path(articulation.folder,"articulation_values.csv"), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	V(g)$Articulation <- vals
	g$ArticulationAvg <- mean(vals)
	g$ArticulationAvg <- mean(vals)
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# plot graph using color for articulation
	custom.gplot(g,col.att=MEAS_ARTICULATION,file=file.path(articulation.folder,"articulation_graph"))
#	custom.gplot(g,col.att=MEAS_ARTICULATION)
	
	# export CSV with average articulation
	stat.file <- file.path(FOLDER_OUT_ANAL,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df[MEAS_ARTICULATION, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	}
	else
	{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
		row.names(df) <- c(MEAS_ARTICULATION)
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	return(g)
}




#############################################################
# Computes average distances and generates plots and CSV files.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.distance <- function(g)
{	tlog(2,"Computing average distances")
	# possibly create folder
	distance.folder <- file.path(FOLDER_OUT_ANAL,g$name,"distance")
	dir.create(path=distance.folder, showWarnings=FALSE, recursive=TRUE)
	
	# distance distribution
	vals <- distances(graph=g)
	flat.vals <- vals[upper.tri(vals)]
	custom.hist(vals=flat.vals, name=LONG_NAME[MEAS_DISTANCE], file=file.path(distance.folder,"distance_histo"))
	# average distance distribution
	avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
	custom.hist(vals=avg.vals, name=LONG_NAME[MEAS_DISTANCE_AVG], file=file.path(distance.folder,"distance_avg_histo"))
	
	# export CSV with average distance
	df <- data.frame(V(g)$name,V(g)$label,avg.vals)
	colnames(df) <- c("Name","Label",MEAS_DISTANCE_AVG) 
	write.csv(df, file=file.path(distance.folder,"distance_avg_values.csv"), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	V(g)$AverageDistance <- avg.vals
	g$DistanceAvg <- mean(flat.vals)
	g$DistanceStdv <- sd(flat.vals)
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# for each node, plot graph using color for distance
	for(n in 1:gorder(g))
	{	nname <- V(g)$name[n]
		V(g)$Distance <- vals[n,]
		if(all(is.infinite(vals[n,-n])))
			tlog(4,"NOT plotting graph for node #",nname,", as all values are infinite")
		else
		{	tlog(4,"Plotting graph for node #",nname)
			custom.gplot(g,col.att=MEAS_DISTANCE,v.hl=n,file=file.path(distance.folder,paste0("distance_graph_",nname)))
		}
		g <- delete_vertex_attr(graph=g, name=MEAS_DISTANCE)
	}
	
	# export CSV with average distance
	stat.file <- file.path(FOLDER_OUT_ANAL,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df[MEAS_DISTANCE, ] <- list(Value=NA, Mean=mean(flat.vals), Stdv=sd(flat.vals))
	}
	else
	{	df <- data.frame(Value=c(NA),Mean=c(mean(flat.vals)),Stdv=c(sd(flat.vals)))
		row.names(df) <- c(MEAS_DISTANCE)
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	return(g)
}




#############################################################
# Computes vertex connectivity and generates plots and CSV files.
#
# g: original graph to process.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.connectivity <- function(g)
{	tlog(2,"Computing vertex connectivity")
	# possibly create folder
	connectivity.folder <- file.path(FOLDER_OUT_ANAL,g$name,"connectivity")
	dir.create(path=connectivity.folder, showWarnings=FALSE, recursive=TRUE)
	
	# connectivity distribution
	vals <- matrix(NA, nrow=gorder(g), ncol=gorder(g))
	for(n in 1:(gorder(g)-1))
	{	vals[n,n] <- 0
		neigh <- neighbors(graph=g, v=n)
		for(n2 in (n+1):gorder(g))
		{	if(n2 %in% neigh)
				tmp <- 1
			else
				tmp <- vertex_connectivity(graph=g, source=n, target=n2)
			vals[n,n2] <- tmp
			vals[n2,n] <- vals[n,n2]
		}
	}
	vals[gorder(g),gorder(g)] <- 0
	flat.vals <- vals[upper.tri(vals)]
	custom.hist(vals=flat.vals, name=LONG_NAME[MEAS_CONNECTIVITY], file=file.path(connectivity.folder,"connectivity_histo"))
	# connectivity distribution
	avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
	custom.hist(vals=avg.vals, name=LONG_NAME[MEAS_CONNECTIVITY_AVG], file=file.path(connectivity.folder,"connectivity_avg_histo"))
	
	# export CSV with average connectivity
	df <- data.frame(V(g)$name,V(g)$label,avg.vals)
	colnames(df) <- c("Name","Label",MEAS_CONNECTIVITY_AVG) 
	write.csv(df, file=file.path(connectivity.folder,"connectivity_avg_values.csv"), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	V(g)$AverageConnectivity <- avg.vals
	g$ConnectivityAvg <- mean(flat.vals)
	g$ConnectivityStdv <- sd(flat.vals)
	graph.file <- file.path(FOLDER_OUT_ANAL, g$name, FILE_GRAPH)
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# for each node, plot graph using color for connectivity
	for(n in 1:gorder(g))
	{	nname <- V(g)$name[n]
		V(g)$Connectivity <- vals[n,]
		if(all(vals[n,]==0))
			tlog(4,"NOT plotting graph for node #",nname,", as all values are zero")
		else
		{	tlog(4,"Plotting graph for node #",nname)
			custom.gplot(g,col.att=MEAS_CONNECTIVITY,v.hl=n,file=file.path(connectivity.folder,paste0("connectivity_graph_",nname)))
		}
		g <- delete_vertex_attr(graph=g, name="Connectivity")
	}
	
	# export CSV with average connectivity
	stat.file <- file.path(FOLDER_OUT_ANAL,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df[MEAS_CONNECTIVITY, ] <- list(Value=NA, Mean=mean(flat.vals), Stdv=sd(flat.vals))
	}
	else
	{	df <- data.frame(Value=c(NA),Mean=c(mean(flat.vals)),Stdv=c(sd(flat.vals)))
		row.names(df) <- c(MEAS_CONNECTIVITY)
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	return(g)
}




#############################################################
# Main method for the graph analysis. Generates a bunch of plots 
# and CSV files to store the results. Also updates the graphml
# file with the results whenever possible, for later external
# use.
#
# gname: name of the graph, used to get the path of its graphml file.
#
# returns: the graph with updated attributes.
#############################################################
analyze.network <- function(gname)
{	# load graph
	file.path <- file.path(FOLDER_OUT_ANAL, gnames[i], FILE_GRAPH)
	g <- load.graphml.file(file=file.path)
	
	# compute attribute stats 
	# (must be done first, before other results are added as attributes)
#	g <- analyze.net.attributes(g)
		
	# compute diameters, eccentricity, radius
#	g <- analyze.net.eccentricity(g)
		
	# compute degree
#	g <- analyze.net.degree(g)
		
	# compute eigencentrality
#	g <- analyze.net.eigencentrality(g)
	
	# compute betweenness
#	g <- analyze.net.betweenness(g)
	
	# compute closeness
	g <- analyze.net.closeness(g)
	
#	# compute distances
#	g <- analyze.net.distance(g)
#	
#	# compute articulation points
#	g <- analyze.net.articulation(g)
#	
#	# detect communities
#	g <- analyze.net.comstruct(g)
#	
#	# compute transitivity
#	g <- analyze.net.transitivity(g)
#	
#	# compute vertex connectivity
#	g <- analyze.net.connectivity(g)
#	
#	# compute assortativity
#	g <- analyze.net.assortativity(g)
	
	return(g)
}
