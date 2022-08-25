###############################################################################
# Processes various measures related to community roles.
#
# References:
#	Guimerà & Amaral
#	  Functional cartography of complex metabolic networks 
#	  Nature, 2005, 433:895-900
#	  DOI: 10.1038/nature03288
# 	Dugué, Labatut, & Perez 
#	  A community role approach to assess social capitalists visibility in the Twitter network
#	  Social Network Analysis and Mining, 2015, 5:26
#	  DOI: 10.1007/s13278-015-0266-0
# 
# Version: 3
# Author: Vincent Labatut 2013-17
###############################################################################




###############################################################################
# Processes the community role measures. 
#
# g: the graph to process.
# membership: integer vector representing the community id of each node in g.
#
# returns: a table whose cols and rows represent measures and nodes (resp.).
###############################################################################
community.role.measures <- function(g, membership)
{	# process neighborhoods: each variable contains a list of integer vectors
	# each integer vector represents the neighborhood of a node (using the ids of its neighbors)
	neigh.all <- neighborhood(graph=g, order=1, nodes=V(g), mode="all")			# ignoring link directions
	if(is.directed(g))
	{	neigh.in <- neighborhood(graph=g, order=1, nodes=V(g), mode="in")		# incoming links only
		neigh.out <- neighborhood(graph=g, order=1, nodes=V(g), mode="out")		# outgoing links only
	}
	
	# build new lists comparable to the previos ones, 
	# except each node id is replaced by the id of the community containing the node 
	com.all <- list()
	com.in <- list()
	com.out <- list()
	for(u in 1:vcount(g))								# for each one in the graph
	{	nall <- neigh.all[[u]][neigh.all[[u]]!=u]		# we remove the node of interest from its own neighborhood
		com.all[[u]] <- membership[nall]				# we retrieve the community ids of the remaining nodes
		if(is.directed(g))
		{	nin <- neigh.in[[u]][neigh.in[[u]]!=u]		# same for the incoming neighborhood
			com.in[[u]] <- membership[nin]
			nout <- neigh.out[[u]][neigh.out[[u]]!=u]	# same for the outgoing one
			com.out[[u]] <- membership[nout]
		}
	}
	
#	print(com.all);print(com.in);print(com.out) # debug	
	
	# process the undirected role measures
	result <- process.original.guimera.amaral(membership, com.all)
	result <- cbind(result, process.undirected.dulape(membership, com.all))
	# process the directed ones (provided the graph is directed)
	if(is.directed(g))
	{	result <- cbind(result, process.directed.guimera.amaral(membership, com.in, com.out))
		result <- cbind(result, process.directed.dulape(membership, com.in, com.out))
	}
	
	return(result)
}




###############################################################################
# Computes the community z-score, i.e. the z-score of a given value for each
# node, but processed relatively to the node community (by opposition to the
# whole network. This method is used to process most of the community role
# measures.
#
# values: a numerical vector containing (for each node) the values to consider.
# membership: communities of the nodes.
#
# returns: the z-score for the specified values and community.
###############################################################################
process.community.zscore <- function(values, membership)
{	# init result vector
	result <- rep(NA,length(values))
	# get existing com ids
	coms <- sort(unique(membership))
	
	# for each community
	for(i in 1:length(coms))
	{	com <- coms[i]
		
		# identify the nodes belonging to this community
		idx <- which(membership==com)
		
		# process the z-score of the specified values, for this community
		result[idx] <- scale(values[idx])	
	}
	
	# specific handling of zero stdev case (thus generalizing the z-score)
	result[is.nan(result)] <- 0
	return(result)
}




###############################################################################
# Processes a generic version of Guimera & Amaral's Participation coefficient,
# for the specified nodes. Each node is described by an integer vector corresponding
# to the community ids of its neighbors. The parameter is therefore a list of such
# integer vectors.
#
# neigh.coms: community ids of the node neighbors, as a list of integer vectors (one
#			  for each node).
#
# returns: a numerical vector containing the participation coefficient obtained for
#		   each node.
###############################################################################
process.generic.participation.coeff <- function(neigh.coms)
{	# for each node
	result <- sapply(neigh.coms, function(coms)
			{	res <- 0
				
				# if no neighbor: the value is zero
				if(length(coms)>0)
				{	# compute community frequence among neighbors, and take the square of each resulting count
					numerator <- (table(coms))^2							
					
					# take the neighborhood size (squared again) and dupplicate to get a vector of same length
					denominator <- rep((length(coms))^2,length(numerator))
					
					# divide both vectors (term by term), sum the terms of the resulting vector, and take the complement to one
					res <- 1 - sum(numerator/denominator) 
				}
				
				return(res)
			})
	
	return(result)
}




###############################################################################
# Processes a generic version of the in/external degree, for the specified nodes. 
# Each node is described by an integer vector corresponding to the community ids 
# of its neighbors, and by its own community id.
#
# membership: community id of each node, as an integer vector.
# neigh.coms: community ids of the node neighbors, as a list of integer vectors (one
#			  for each node).
# internal: TRUE to process the internal degree, FALSE for the external one.
#
# returns: a numerical vector containing the in/external degree obtained for each node.
###############################################################################
process.generic.community.degree <- function(membership, neigh.coms, internal)
{	# process the internal degree of for each node u
	result <- sapply(1:length(membership), function(u)
			{	# get the community ids of the neighbors
				coms <- neigh.coms[[u]]
				
				# get the community id of the node of interest
				own.com <- membership[u]
				
				# internal degree: count the number of neighbors in the same community
				if(internal)
					res <- length(which(coms==own.com))
				# otherwise, external: count the neighbors in other communities
				else
					res <- length(which(coms!=own.com))
				
				return(res)
			})
	
	return(result)
}




###############################################################################
# Processes a generic version of the standard deviation of the node connectivity, 
# for the specified nodes. # Each node is described by an integer vector corresponding 
# to the community ids of its neighbors, and by its own community id. This function
# determines the distribution of communities among the neighbors of a given node, and
# computes its standard deviation to characterize the said node.
#
# membership: community id of each node, as an integer vector.
# neigh.coms: community ids of the node neighbors, as a list of integer vectors (one
#			  for each node).
#
# returns: a numerical vector containing the stdev connectivity obtained for each node.
###############################################################################
process.generic.stdev.connectivity <- function(membership, neigh.coms)
{	# process the standard deviation of the external connectivity for each node u
	result <- sapply(1:length(membership), function(u)
			{	res <- 0
				
				# get the community ids of the neighbors
				coms <- neigh.coms[[u]]
				
				# if no neighbor: the value is zero
				if(length(coms)>0)
				{	# get the community id of the node of interest
					own.com <- membership[u]
					
					# index of the nodes *not* belonging to the same community
					idx <- which(coms!=own.com)
					
					# get the community frequence among them
					t <- table(coms[idx])
					
					# compute the standard deviation for these values
					res <- sd(t)
					
					# if the stdev is not defined, we use zero
					if(is.na(res))
						res <- 0
				}
				
				return(res)
			})
	
	return(result)
}




###############################################################################
# Processes a generic version of the number of neighboring communities, for the 
# specified nodes. Each node is described by an integer vector corresponding to 
# the community ids of its neighbors, and by its own community id. This function
# determines the number of communities among the neighbors of each node.
#
# membership: community id of each node, as an integer vector.
# neigh.coms: community ids of the node neighbors, as a list of integer vectors (one
#			  for each node).
#
# returns: a numerical vector containing the number of distinct communities among
# 		   the neighbors of each node.
###############################################################################
process.generic.nbr.neigh.com <- function(membership, neigh.coms)
{	# process the number of neighboring communities for each node u
	result <- sapply(1:length(membership), function(u)
			{	res <- 0
				
				# get the community ids of the neighbors
				coms <- neigh.coms[[u]]
				
				# if no neighbor: the value is zero
				if(length(coms)>0)
				{	# get the community id of the node of interest
					own.com <- membership[u]
					
					# index of the nodes *not* belonging to the same community
					idx <- which(coms!=own.com)
					
					# count the number of distinct communities among them
					res <- length(unique(coms[idx]))
				}
				
				return(res)
			})
}




###############################################################################
# Processes the original community role measures of Guimera & Amaral, designed 
# for undirected unweighted graphs.
# 
# membership: integer vector containing the community id of each node.
# com.all: list of integer vectors, each one containing the community ids of the
#		   neighbors of a given node.
#
# returns: a matrix containing as many rows as nodes, and 2 columns (one for each
#          G&A measure P & z).
###############################################################################
process.original.guimera.amaral <- function(membership, com.all)
{	names <- c("P", "z")															# names of the table columns
	result <- matrix(nrow=length(membership),ncol=length(names))					# init result table
	colnames(result) <- names														# setup column names
	
	# undirected participation coefficient P
	result[,"P"] <- process.generic.participation.coeff(com.all)					# the result go to the column "P" in the table
	
	# undirected z within-module degree
	k.int <- process.generic.community.degree(membership, com.all, internal=TRUE)	# process the (undirected) internal degree
	result[,"z"] <- process.community.zscore(values=k.int, membership)				# z is simply the z-score of the internal degree
	
	return(result)
}




###############################################################################
# Processes the generalized version of the community role measures of Guimera & Amaral, 
# designed for directed unweighted graphs.
# 
# membership: integer vector containing the community id of each node.
# com.in: list of integer vectors, each one containing the community ids of the
#		  incoming neighbors of a given node.
# com.out: list of integer vectors, each one containing the community ids of the
#		   outgoing neighbors of a given node.
#
# returns: a matrix containing as many rows as nodes, and 4 columns (one for each
#          G&A measure P & z, in their outgoing and incoming versions).
###############################################################################
process.directed.guimera.amaral <- function(membership, com.in, com.out)
{	names <- c("P_in", "P_out", "z_in", "z_out")
	result <- matrix(nrow=length(membership),ncol=length(names))
	colnames(result) <- names
	
	# incoming participation coefficient P_in
	result[,"P_in"] <- process.generic.participation.coeff(com.in)
	# outgoing participation coefficient P_out
	result[,"P_out"] <- process.generic.participation.coeff(com.out)
	
	# incoming within-module degree z_in
	k.int <- process.generic.community.degree(membership, com.in, internal=TRUE)
	result[,"z_in"] <- process.community.zscore(values=k.int, membership)
	# outgoing within-module degree z_out
	k.int <- process.generic.community.degree(membership, com.out, internal=TRUE)
	result[,"z_out"] <- process.community.zscore(values=k.int, membership)
	
	return(result)
}




###############################################################################
# Processes the generalization of the role measures proposed by Dugué, Labatut &
# Perez, for undirected unweighted networks.  
#
# membership: integer vector containing the community id of each node.
# com.all: list of integer vectors, each one containing the community ids of the
#		   neighbors of a given node.
#
# returns: a matrix containing as many rows as nodes, and 4 columns (one for each
#          measure: internal and external intensities, heterogeneity and diversity).
###############################################################################
process.undirected.dulape <- function(membership, com.all) 
{	names <- c("I_int", "I_ext", "H", "D")
	result <- matrix(nrow=length(membership),ncol=length(names))
	colnames(result) <- names
	
	# internal intensity I_int
	k.int <- process.generic.community.degree(membership, com.all, internal=TRUE)
	result[,"I_int"] <- process.community.zscore(values=k.int,membership)
	
	# external intensity I_ext
	k.ext <- process.generic.community.degree(membership, com.all, internal=FALSE)
	result[,"I_ext"] <- process.community.zscore(values=k.ext,membership)
	
	# heterogeneity H
	sd.ext <- process.generic.stdev.connectivity(membership, com.all)
	result[,"H"] <- process.community.zscore(values=sd.ext,membership)
	
	# diversity D
	n.ext <- process.generic.nbr.neigh.com(membership, com.all)
	result[,"D"] <- process.community.zscore(values=n.ext,membership)
	
	return(result)
}




###############################################################################
# Processes the generalization of the role measures proposed by Dugué, Labatut &
# Perez, for directed unweighted networks.  
#
# membership: integer vector containing the community id of each node.
# com.in: list of integer vectors, each one containing the community ids of the
#		  incoming neighbors of a given node.
# com.out: list of integer vectors, each one containing the community ids of the
#		   outgoing neighbors of a given node.
#
# returns: a matrix containing as many rows as nodes, and 8 columns (one for each
#          measure in its outgoing and incoming versions).
###############################################################################
process.directed.dulape <- function(membership, com.in, com.out) 
{	names <- c("I_int^in", "I_int^out", "I_ext^in", "I_ext^out", "H_in", "H_out", "D_in", "D_out")
	result <- matrix(nrow=length(membership),ncol=length(names))
	colnames(result) <- names
	
	# incoming internal intensity I_int^in
	k.int <- process.generic.community.degree(membership, com.in, internal=TRUE)
	result[,"I_int^in"] <- process.community.zscore(values=k.int,membership)
	
	# outgoing internal intensity I_int-out
	k.int <- process.generic.community.degree(membership, com.out, internal=TRUE)
	result[,"I_int^out"] <- process.community.zscore(values=k.int,membership)
	
	# incoming external intensity I_ext^in
	k.ext <- process.generic.community.degree(membership, com.in, internal=FALSE)
	result[,"I_ext^in"] <- process.community.zscore(values=k.ext,membership)
	
	# outgoing external intensity I_ext^out
	k.ext <- process.generic.community.degree(membership, com.out, internal=FALSE)
	result[,"I_ext^out"] <- process.community.zscore(values=k.ext,membership)
	
	# incoming heterogeneity H_in
	sd.ext <- process.generic.stdev.connectivity(membership, com.in)
	result[,"H_in"] <- process.community.zscore(values=sd.ext,membership)
	
	# outgoing heterogeneity H_out
	sd.ext <- process.generic.stdev.connectivity(membership, com.out)
	result[,"H_out"] <- process.community.zscore(values=sd.ext,membership)
	
	# incoming diversity D_in
	n.ext <- process.generic.nbr.neigh.com(membership, com.in)
	result[,"D_in"] <- process.community.zscore(values=n.ext,membership)
	
	# outgoing diversity D_out
	n.ext <- process.generic.nbr.neigh.com(membership, com.out)
	result[,"D_out"] <- process.community.zscore(values=n.ext,membership)
	
	return(result)
}




###############################################################################
# Produces a plot similar to the ones provided in the original paper of Guimera
# & Amaral.
#
# ga.p: participation coefficient.
# ga.z: within-module degree.
# plot.file: path of the file to produce.
#
# returns: the roles identified when building the plot.
###############################################################################
plot.original.guimera.amaral <- function(ga.p, ga.z, plot.file)
{	R_T     <-  2.500
	Z_NH_T1 <-  0.050
	Z_NH_T2 <-  0.625
	Z_NH_T3 <-  0.800
	Z_H_T1  <-  0.300
	Z_H_T2  <-  0.750
	LEFT    <- -0.1
	RIGHT   <-  1.1
	BOT     <-  min(ga.z) - 0.1*(max(ga.z)-min(ga.z))
	TOP     <-  max(ga.z) + 0.1*(max(ga.z)-min(ga.z))
	
	# identify the catefory of each node according to GA typology
	ga.r <- rep(NA, length(ga.p))
	# non-hubs
	ga.r[ga.z<R_T  & ga.p<Z_NH_T1] <- "Ultra-peripheral Non-hubs"
	ga.r[ga.z<R_T  & ga.p>=Z_NH_T1 & ga.p<Z_NH_T2] <- "Peripheral Non-hubs"
	ga.r[ga.z<R_T  & ga.p>=Z_NH_T2 & ga.p<Z_NH_T3] <- "Connnector Non-hubs"
	ga.r[ga.z<R_T  & ga.p>=Z_NH_T3] <- "Kinless Non-hubs"
	# hubs
	ga.r[ga.z>=R_T & ga.p<Z_H_T1] <- "Provincial Hubs"
	ga.r[ga.z>=R_T & ga.p>=Z_H_T1 & ga.p<Z_H_T2] <- "Connector Hubs"
	ga.r[ga.z>=R_T & ga.p>=Z_H_T2] <- "Kinless Hub"
	
	# record the obtained values
	df <- data.frame(ga.p, ga.z, ga.r)
	colnames(df) <- c("ParticipationCoefficient","WithinModuleDegree","NodeRole")
	tab.file <- paste0(plot.file,"_values.csv")
	write.csv(df, file=tab.file, row.names=FALSE)
	
	# generate plot
	for(fformat in FORMAT)
	{	if(fformat=="pdf")
			pdf(paste0(plot.file,".pdf"))#, width=25, height=25)
		else if(fformat=="png")
			png(paste0(plot.file,".png"), width=1024, height=1024)
		# main plot
		plot(
			NULL,
			cex=2, cex.lab=1.5, cex.axis=1.5,
			xlab="Participation Coefficient", ylab="Within Community Degree",
			xlim=0:1, ylim=range(ga.z)
		)
		# draw role boundaries
		rect(xleft=LEFT,    ybottom=BOT,  xright=Z_NH_T1, ytop=R_T,  border=NA, col="#EBEBEB")	# light grey
		rect(xleft=Z_NH_T1, ybottom=BOT,  xright=Z_NH_T2, ytop=R_T,  border=NA, col="#F1967C")	# red
		rect(xleft=Z_NH_T2, ybottom=BOT,  xright=Z_NH_T3, ytop=R_T,  border=NA, col="#B8DA9A")	# green
		rect(xleft=Z_NH_T3, ybottom=BOT,  xright=RIGHT,   ytop=R_T,  border=NA, col="#9197CB")	# blue
		#
		rect(xleft=LEFT,    ybottom=R_T,  xright=Z_H_T1,  ytop=TOP,  border=NA, col="#F6F5A3")	# yellow
		rect(xleft=Z_H_T1,  ybottom=R_T,  xright=Z_H_T2,  ytop=TOP,  border=NA, col="#D6C1C1")	# pink
		rect(xleft=Z_H_T2,  ybottom=R_T,  xright=RIGHT,   ytop=TOP,  border=NA, col="#726E6D")	# dark grey
		#rect(xleft=-0.0275, ybottom=-1.18, xright=0.6925, ytop=3.74, border="black")
		# draw nodes
#		cols <- c("grey", "red", "green", "blue", "yellow", "magenta", "darkgrey")
#		names(cols) <- c("Ultra-peripheral Non-hubs", "Peripheral Non-hubs", "Connnector Non-hubs", "Kinless Non-hubs", "Provincial Hubs", "Connector Hubs", "Kinless Hub")
		cols <- COLS_ATT[[MEAS_ROLE_CAT]]
		for(r in 1:length(cols))
		{	idx <- which(ga.r==names(cols)[r])
			points(
				x=ga.p[idx],
				y=ga.z[idx],
				pch=21,
				bg=cols[r],
				cex=2
			)
		}
		# add text
		text(x=-0.0300, y=min(ga.z)-0.02*(max(ga.z)-min(ga.z)), adj=0.0, cex=1.25, labels="Ultra-peripheral Non-hubs", srt=90)
		text(x= 0.3250, y=min(ga.z)-0.02*(max(ga.z)-min(ga.z)), adj=0.5, cex=1.25, labels="Peripheral Non-hubs")
		text(x= 0.6400, y=min(ga.z)-0.02*(max(ga.z)-min(ga.z)), adj=0.0, cex=1.25, labels="Connnector Non-hubs", srt=90)
		text(x= 0.9000, y=min(ga.z)-0.02*(max(ga.z)-min(ga.z)), adj=0.5, cex=1.25, labels="Kinless Non-hubs")
		#
		text(x= 0.1500, y=max(ga.z)+0.02*(max(ga.z)-min(ga.z)), adj=0.5, cex=1.25, labels="Provincial Hubs")
		text(x= 0.5250, y=max(ga.z)+0.02*(max(ga.z)-min(ga.z)), adj=0.5, cex=1.25, labels="Connector Hubs")
		text(x= 0.8750, y=max(ga.z)+0.02*(max(ga.z)-min(ga.z)), adj=0.5, cex=1.25, labels="Kinless Hub")
		dev.off()
	}
	
	return(ga.r)
}
