############################################################################
# Functions related to the processing of link intersections.
#
# Vincent Labatut 12/2015
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/model/intersect.R")
############################################################################



###############################################################################
# Splits the specified edge at the specified point, creating a new vertex and
# replacing the edge by two new edges. The name of the new edges stay the same.
#
# g: city graph.
# e: edge to split.
# x,y: position of the new vertex (must be on the edge).
# name: name of the new vertex.
# 
# returns: updated graph.
###############################################################################
split.edge <- function(g, v1, v2, x, y, v.name)
{	# verification
#	a <- (V(g)[v1]$y - V(g)[v2]$y) / (V(g)[v1]$x - V(g)[v2]$x)
#	b <- V(g)[v1]$y - a*V(g)[v1]$x
#	if(y != a*x+b)
#		stop("ERROR: intersection point (",x,";",y,") not located on edge ",v1,"--",v2)
	
	# delete old edge
	e.name <- E(g)[v1 %--% v2]$name
	e.type <- E(g)[v1 %--% v2]$type
	g <- delete_edges(graph=g, edges=E(g)[v1 %--% v2])
	
	# add new vertex
	g <- add_vertices(graph=g, nv=1, attr=list(name=v.name, type="tertiary", x=x, y=y))
	v3 <- gorder(g)
	
	# create new edges
	g <- add_edges(graph=g, edges=c(v1,v3,v3,v2), attr=list(name=rep(e.name,2), type=rep(e.type,2)))
	
	return(g)
}




###############################################################################
# Create a path between two vertices, passing through existing vertices.
#
# g: graph.
# start: index of the starting vertex.
# end: index of the end vertex.
# e.type: type of the created edges.
# e.name: name of the created edges.
# tolerance: maximal angle when selecting next vertex.
#
# return: a list containing the completed graph and the created path.
###############################################################################
build.path <- function(g, start, end, e.type, e.name, tolerance=30)
{	u <- start
	remaining <- (1:gorder(g))[-start]
	path <- u
	while(u!=end)
	{	# compute angle with all remaining vertices
		angles <- (atan2(V(g)[end]$y - V(g)[u]$y, V(g)[end]$x - V(g)[u]$x) 
					- atan2(V(g)[remaining]$y - V(g)[u]$y, V(g)[remaining]$x - V(g)[u]$x)) / pi * 180
		# normalize angles to [0;180]
		angles[angles>180] <- angles[angles>180] - 360
		angles[angles<(-180)] <- angles[angles<(-180)] + 360
		angles <- abs(angles)
		#print(cbind(remaining,angles))
		# select smaller angle
		idx <- which(angles<=tolerance)
		print(cbind(remaining[idx],angles[idx]))
		dd <- sapply(remaining[idx], function(v) (V(g)[start]$x-V(g)[v]$x)^2 + (V(g)[start]$y-V(g)[v]$y)^2)
		v <- remaining[idx[which.min(dd)]]
		remaining <- remaining[-idx[which.min(dd)]]
		if(!are.connected(graph=g, v1=u, v2=v))
			g <- add_edges(graph=g, edges=c(u,v), attr=list(name=e.name, type=e.type))
		u <- v
		path <- c(path, u)
	}
	
	res <- list(g=g, path=path)
	return(res)
}




###############################################################################
# Computes the intersection point between the line going through points 1 & 2,
# and the perpendicular going through point 3.
# Taken from: https://stackoverflow.com/a/12499474/1254730
#
# x1,y1: position of the first point of the line.
# x2,y2: position of the second point of the line.
# x3,y3: position of the point of the perpendicular.
#
# returns: coordinates of the intersection point.
###############################################################################
get.inter.point <- function(x1, y1, x2, y2, x3, y3)
{	px <- x2 - x1
	py <- y2 - y1
	d12 <- px*px + py*py
	u <- ((x3 - x1) * px + (y3 - y1) * py) / d12;
	x <- x1 + u * px
	y <- y1 + u * py
	
	res <- c(x,y)
	return(res)
}




############################################################################
# This function determines if the two specified segments intersect. Each
# segment is represented by the coordinates of its endpoints. It also returns
# TRUE if the segments overlap (i.e. more than one intersection point).
#
# x1,y1: coordinates of the first endpoint of the first segment.
# x2,y2: coordinates of the second endpoint of the first segment.
# x3,y3: coordinates of the first endpoint of the second segment.
# x4,y4: coordinates of the second endpoint of the second segment.
#
# returns: TRUE iff the segments intersect.
############################################################################
check.segment.crossing <- function(x1, y1, x2, y2, x3, y3, x4, y4)
{	# put all of this in a vector to ease handling
	x <- c(x1,x2,x3,x4)
	y <- c(y1,y2,y3,y4)
	
	# order x coordinates
	if(x1>x2)
	{	x1 <- x[2]
		x2 <- x[1]
	}
	if(x3>x4)
	{	x3 <- x[4]
		x4 <- x[3]
	}
	
	# order y coordinates
	if(y1>y2)
	{	y1 <- y[2]
		y2 <- y[1]
	}
	if(y3>y4)
	{	y3 <- y[4]
		y4 <- y[3]
	}
#cat("x1=",x1," x2=",x2," x3=",x3," x4=",x4,"\n",sep="")	
#cat("y1=",y1," y2=",y2," y3=",y3," y4=",y4,"\n",sep="")	

	# regular case (segments not vertical)
	if(x[1]!=x[2] && x[3]!=x[4])
	{	# process segments line equations
		# y = ax + c
		a <- (y[1]-y[2])/(x[1]-x[2])
		c <- y[1] - a*x[1]
		# y = bx + d
		b <- (y[3]-y[4])/(x[3]-x[4])
		d <- y[3] - b*x[3]
#cat("a=",a," c=",c," b=",b," d=",d,"\n",sep="")	
		# case of parallel segments
		if(a==b)
		{	# case of overlapping segments
			if(c==d)
			{	if(x1<x3)
					result <- x2 >= x3
				else if(x1>x3)
					result <- x4 >= x1
				else
					result <- FALSE
			}
			# case of disjoint segments
			else
				result <- FALSE
		}
		# case of crossing segments
		else
		{	# process intersection point
			xi <- (d-c)/(a-b)
#print(xi)			
			yi <- (a*d-b*c)/(a-b)
			# check if belongs to both segments
			result <- xi>=x1 && xi<=x2 && xi>=x3 && xi<=x4
		}
	}
	
	# one or both segments are vertical
	else
	{	# first segment is vertical
		if(x[1]==x[2])
		{	# both segments are vertical
			if(x[3]==x[4])
			{	# if the segments are aligned
				if(x[1]==x[3])
				{	# check if the segments overlap
					if(y1<y3)
						result <- y2 >= y3
					else if(y1>y3)
						result <- y4 >= y1
					else
						result <- FALSE
				}
				# not aligned
				else
					result <- FALSE
			}
			# only the first segment is vertical
			else
			{	# check if x coordinate belongs to second segment
				result <- x3<=x1 && x1<=x4
			}
		}
		# only the second segment is vertical
		else
		{	# check if x coordinate belongs to first segment
			result <- x1<=x3 && x3<=x2
		}
	}
	
	return(result)
}
############ Tests
## crossing, no vertical segment
#print(check.segment.crossing(0,1, 1,0, 0,0, 1,1))
#print(check.segment.crossing(0,0, 1,1, 0,1, 1,0))
## overlap, no vertical segment
#print(check.segment.crossing(0,0, 2,2, 1,1, 3,3))
#print(check.segment.crossing(1,1, 3,3, 0,0, 2,2))
## no crossing, no overlap, no vertical segment
#print(check.segment.crossing(0,0, 1,1, 2,2, 1,3))
#print(check.segment.crossing(2,2, 1,3, 0,0, 1,1))
## no crossing, no overlap, parallel segments
#print(check.segment.crossing(0,0, 1,1, 2,3, 3,4))
#print(check.segment.crossing(2,3, 3,4, 0,0, 1,1))
## no crossing, no overlap, aligned segments
#print(check.segment.crossing(0,0, 1,1, 2,2, 3,3))
#print(check.segment.crossing(2,2, 3,3, 0,0, 1,1))
## crossing, one segment is vertical
#print(check.segment.crossing(1,0, 1,2, 0,0, 2,2))
#print(check.segment.crossing(0,0, 2,2, 1,0, 1,2))
## overlap, both segments are vertical
#print(check.segment.crossing(0,0, 0,2, 0,1, 0,3))
#print(check.segment.crossing(0,1, 0,3, 0,0, 0,2))
## no overlap, both segments are vertical, segments aligned
#print(check.segment.crossing(0,0, 0,1, 0,2, 0,3))
#print(check.segment.crossing(0,2, 0,3, 0,0, 0,1))
## no overlap, both segments are vertical, segments not aligned
#print(check.segment.crossing(0,0, 0,1, 1,0, 1,1))
#print(check.segment.crossing(1,0, 1,1, 0,0, 0,1))




############################################################################
# Checks if the specified segment intersects with another link (i.e. if the
# graph would still be planar after adding the edge corresponding to this 
# segment).
#
# g: graph to consider.
# v1: first end of the segment (a vertex).
# x2,y2: coordinates of the other end.
#
# returns: TRUE iff the segment crosses an existing link.
############################################################################
check.potential.link.crossing <- function(g, v1, x2, y2)
{	result <- FALSE
	
	# get edge info
	es <- 1:gsize(g)
	el <- get.edgelist(g, names=FALSE)
	x1 <- V(g)[v1]$x
	y1 <- V(g)[v1]$y
	
	# possibly loop over all edges
	i <- 1
	while(!result && i<=length(es))
	{	# get current edge
		e <- es[i]
		v3 <- el[e,1]
		v4 <- el[e,2]
		
		# check that not attached to vertex v1
		if(v3==v1 || v4==v1)
			result <- TRUE
		else
		{	# get coordinates of edge ends
			x3 <- V(g)[v3]$x
			y3 <- V(g)[v3]$y
			x4 <- V(g)[v4]$x
			y4 <- V(g)[v4]$y
			
			# check segment intersection
			result <- check.segment.crossing(x1, y1, x2, y2, x3, y3, x4, y4)
		}
		i <- i + 1		
	}
	
	return(result)
}




############################################################################
# Checks if the specified link intersects with another link (i.e. if the
# graph is still planar).
#
# g: graph to consider.
# e: id of the link of interest in the graph g. If NA, we check all of them.
#
# returns: TRUE iff link e is crossing another link in graph g (or iff there
#		   is any link crossing in g, in the case of e=NA).
############################################################################
check.link.crossing <- function(g, e=NA)
{	result <- FALSE
#cat("START\n")	
	# possibly init e
	if(is.na(e))
		es <- 1:ecount(g)
	else
		es <- e

#print(es)	
	j <- 1
	while(!result && j<=length(es))
	{	e <- es[j]
		
		# get link list, remove considered link
		el <- get.edgelist(g)
		n1 <- el[e,1]
		x1 <- V(g)[n1]$x
		y1 <- V(g)[n1]$y
		n2 <- el[e,2]
		x2 <- V(g)[n2]$x
		y2 <- V(g)[n2]$y
		el <- el[-e,,drop=FALSE]
		
		i <- 1
		# loop on all links, stop when intersection found
		while(!result & i<=nrow(el))
		{	# get the node indices
			n3 <- el[i,1]
			n4 <- el[i,2]
			
			# check if the link if it has no common nodes with e
			if(length(intersect(c(n1,n2),c(n3,n4)))==0)
			{	# get the endpoints coordinates
				x3 <- V(g)[n3]$x
				y3 <- V(g)[n3]$y
				x4 <- V(g)[n4]$x
				y4 <- V(g)[n4]$y
				# check segment intersection
				result <- check.segment.crossing(x1, y1, x2, y2, x3, y3, x4, y4)
#cat("(",n1,",",n2,") vs. (",n3,",",n4,") => ",result,"\n",sep="")
if(result)
	tlog(4,"Crossing detected for (",n1,",",n2,") vs. (",n3,",",n4,")")
			}
			
			i <- i + 1		
		}
		
		j <- j + 1
	}
	
	return(result)
}




############################################################################
# Adds a node at each intersection point between two links. Also checks if
# two links overlap, in which case they are broken down to several links, in
# order to avoid any overlap.
#
# g: the graph to process.
#
# returns: the modified graph, which is plane and without any overlapping links.
############################################################################
add.intersection.nodes <- function(g)
{	modified <- TRUE
	
	# repeat as long as the graph is modified
	while(ecount(g)>1 && modified)
	{	modified <- FALSE
		i <- 1
		
		# loop on all links
		while(i<ecount(g) && !modified)
		{	n <- rep(NA,4)
			x <- rep(NA,4)
			y <- rep(NA,4)
			
			# get the link matrix
			el <- as_edgelist(graph=g, names=FALSE)
			n[1] <- el[i,1]
			n[2] <- el[i,2]
			# get the endpoints coordinates
			x[1] <- V(g)[n[1]]$x
			y[1] <- V(g)[n[1]]$y
			x[2] <- V(g)[n[2]]$x
			y[2] <- V(g)[n[2]]$y
			
			# loop on all other links
			j <- i + 1
			while(j<=ecount(g) && !modified)
			{	# get the node indices
				n[3] <- el[j,1]
				n[4] <- el[j,2]
				
				# check the second link only if it has no common nodes with the first one
				if(length(intersect(c(n[1],n[2]),c(n[3],n[4])))==0)
				{	# get the endpoints coordinates
					x[3] <- V(g)[n[3]]$x
					y[3] <- V(g)[n[3]]$y
					x[4] <- V(g)[n[4]]$x
					y[4] <- V(g)[n[4]]$y
					
					# check segment intersection
					modified <- check.segment.crossing(x[1], y[1], x[2], y[2], x[3], y[3], x[4], y[4])
#print(modified)
#cat("(",n[1],",",n[2],") vs. (",n[3],",",n[4],") => ",modified,"\n",sep="")
					
					# if there's an intersection
					if(modified)
					{	tlog(4,"Crossing detected for (",n[1],",",n[2],") vs. (",n[3],",",n[4],")")
						# get old edge attributes
						e.type1 <- E(g)[n[1] %--% n[2]]$type
						e.name1 <- E(g)[n[1] %--% n[2]]$name
						e.type2 <- E(g)[n[3] %--% n[4]]$type
						e.name2 <- E(g)[n[3] %--% n[4]]$name
						
						# delete both existing links from the graph
						g <- delete.edges(graph=g,c(E(g)[n[1] %--% n[2]],E(g)[n[3] %--% n[4]]))
						if(is.null(g$removed))
							g$removed <- matrix(n,nrow=2,ncol=2,byrow=TRUE)
						else
							g$removed <- rbind(g$removed,matrix(n,nrow=2,ncol=2,byrow=TRUE))
						
						# regular case (segments not vertical)
						if(x[1]!=x[2] && x[3]!=x[4])
						{	# process segments line equations
							# y = ax + c
							a <- (y[1]-y[2])/(x[1]-x[2])
							c <- y[1] - a*x[1]
							# y = bx + d
							b <- (y[3]-y[4])/(x[3]-x[4])
							d <- y[3] - b*x[3]
							
							# case of overlapping segments
							if(a==b && c==d)
							{	# identify overlap nodes
								idx <- order(x)
								ni <- n[idx]
								# add 3 new links to the graph, no overlap
#print(ni)
								es <- c(ni[1],ni[2],ni[2],ni[3],ni[3],ni[4])
								e.types <- c(e.type1, e.type1, e.type2)
								e.names <- c(e.name1, e.name1, e.name2)
								g <- add.edges(graph=g,edges=es,attr=list(added=TRUE, type=e.types, name=e.names))
							}
							# case of crossing segments
							else
							{	# process the intersection point
								xi <- (d-c)/(a-b)
								yi <- (a*d-b*c)/(a-b)
								# add 4 new links to the graph, no crossing
								g <- add.vertices(graph=g,nv=1,attr=list(added=TRUE, x=xi, y=yi, type="tertiary", name="tertiary"))
								nc <- vcount(g)
								es <- c(n[1],nc,n[2],nc,n[3],nc,n[4],nc)
								e.types <- c(e.type1, e.type1, e.type2, e.type2)
								e.names <- c(e.name1, e.name1, e.name2, e.name2)
								g <- add.edges(graph=g, edges=es, attr=list(added=TRUE, type=e.types, name=e.names))
							}
						}
						
						# one or both segments are vertical
						else
						{	# first segment is vertical
							if(x[1]==x[2])
							{	# both segments are vertical >> overlap
								if(x[3]==x[4])
								{	# identify overlap nodes
									idx <- order(y)
									ni <- n[idx]
									# add 3 new links to the graph, no overlap
									es <- c(ni[1],ni[2],ni[2],ni[3],ni[3],ni[4])
									e.types <- c(e.type1, e.type1, e.type2)
									e.names <- c(e.name1, e.name1, e.name2)
									g <- add.edges(graph=g,edges=es,attr=list(added=TRUE, type=e.types, name=e.names))
								}
								# only the first segment is vertical
								else
								{	# y = bx + d
									b <- (y[3]-y[4])/(x[3]-x[4])
									d <- y[3] - b*x[3]
									# process the intersection point
									xi <- x[1]
									yi <- b*xi + d
									# add 4 new links to the graph, no crossing
									g <- add.vertices(graph=g,nv=1,attr=list(added=TRUE, x=xi, y=yi, type="tertiary", name="tertiary"))
									nc <- vcount(g)
									es <- c(n[1],nc,n[2],nc,n[3],nc,n[4],nc)
									e.types <- c(e.type1, e.type1, e.type2, e.type2)
									e.names <- c(e.name1, e.name1, e.name2, e.name2)
									g <- add.edges(graph=g,edges=es,attr=list(added=TRUE, type=e.types, name=e.names))
								}
							}
							# only the second segment is vertical
							else
							{	# y = ax + c
								a <- (y[1]-y[2])/(x[1]-x[2])
								c <- y[1] - a*x[1]
								# process the intersection point
								xi <- x[3]
								yi <- a*xi + c
								# add 4 new links to the graph, no crossing
								g <- add.vertices(graph=g,nv=1,attr=list(added=TRUE, x=xi, y=yi, type="tertiary", name="tertiary"))
								nc <- vcount(g)
								es <- c(n[1],nc,n[2],nc,n[3],nc,n[4],nc)
								e.types <- c(e.type1, e.type1, e.type2, e.type2)
								e.names <- c(e.name1, e.name1, e.name2, e.name2)
								g <- add.edges(graph=g,edges=es,attr=list(added=TRUE, type=e.types, name=e.names))
							}
						}
					}
				}
				
				j <- j + 1
			}
			
			i <- i + 1
		}
	}
	
	return(g)
}
############ Tests
#g <- graph.empty(n=4,directed=FALSE)
#g <- add.edges(g,c(1,2,3,4))
## crossing, no vertical segment
#	V(g)$x <- c(0,1,0,1);V(g)$y <- c(1,0,0,1);plot(g)
#	V(g)$x <- c(0,1,0,1);V(g)$y <- c(0,1,1,0);plot(g)
## overlap, no vertical segment
#	V(g)$x <- c(0,2,1,3);V(g)$y <- c(0,2,1,3);plot(g)
#	V(g)$x <- c(1,3,0,2);V(g)$y <- c(1,3,0,2);plot(g)
## no crossing, no overlap, no vertical segment
#	V(g)$x <- c(0,1,2,1);V(g)$y <- c(0,1,2,3);plot(g)
#	V(g)$x <- c(2,1,0,1);V(g)$y <- c(2,3,0,1);plot(g)
## no crossing, no overlap, parallel segments
#	V(g)$x <- c(0,1,2,3);V(g)$y <- c(0,1,3,4);plot(g)
#	V(g)$x <- c(2,3,0,1);V(g)$y <- c(3,4,0,1);plot(g)
## no crossing, no overlap, aligned segments, no vertical segment
#	V(g)$x <- c(0,1,2,3);V(g)$y <- c(0,1,2,3);plot(g)
#	V(g)$x <- c(2,3,0,1);V(g)$y <- c(2,3,0,1);plot(g)
## crossing, one segment is vertical
#	V(g)$x <- c(1,1,0,2);V(g)$y <- c(0,2,0,2);plot(g)
#	V(g)$x <- c(0,2,1,1);V(g)$y <- c(0,2,0,2);plot(g)
## overlap, both segments are vertical
#	V(g)$x <- c(0,0,0,0);V(g)$y <- c(0,2,1,3);plot(g)
#	V(g)$x <- c(0,0,0,0);V(g)$y <- c(1,3,0,2);plot(g)
## no overlap, both segments are vertical, segments aligned
#	V(g)$x <- c(0,0,0,0);V(g)$y <- c(0,1,2,3);plot(g)
#	V(g)$x <- c(0,0,0,0);V(g)$y <- c(2,3,0,1);plot(g)
## no overlap, both segments are vertical, segments not aligned
#	V(g)$x <- c(0,0,1,1);V(g)$y <- c(0,1,0,1);plot(g)
#	V(g)$x <- c(1,1,0,0);V(g)$y <- c(0,1,0,1);plot(g)
	
#g2 <- add.intersection.nodes(g)
#plot(g,layout=matrix(runif(vcount(g)*2),ncol=2))
#plot(g2,layout=matrix(runif(vcount(g2)*2),ncol=2))
