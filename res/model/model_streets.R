# Random model to create a city network based on relatively realistic streets.
# 
# Vincent Labatut
# 06/2022
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/model_streets.R")
###############################################################################
# load other scripts
source("res/common/_include.R")




###############################################################################
# parameters
n.sec <- 100
n.av <- 5
out.folder <- "out/analysis/estate/model/streets"




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
	g <- delete_edges(graph=g, edges=E(g)[v1 %--% v2])
	
	# add new vertex
	g <- add_vertices(graph=g, nv=1, attr=list(name=v.name, x=x, y=y))
	
	# create new edges
	g <- add_edges(graph=g, edges=c(v1,v.name,v.name,v2), attr=list(name=rep(e.name,2)))
	
	return(g)
}




###############################################################################
# Create a path between two vertices, passing through existing vertices.
#
# g: graph.
# start: index of the starting vertex.
# end: index of the end vertex.
# e.name: name of the created edges.
# tolerance: maximal angle when selecting next vertex.
#
# return: a list containing the completed graph and the created path.
###############################################################################
build.path <- function(g, start, end, e.name, tolerance=45)
{	u <- start
	remaining <- (1:gorder(g))[-start]
	path <- u
	while(u!=end)
	{	# compute angle with all remaining vertices
		angles <- (atan2(V(g)[end]$y - V(g)[u]$y, V(g)[end]$x - V(g)[u]$x) 
			- atan2(V(g)[remaining]$y - V(g)[u]$y, V(g)[remaining]$x - V(g)[u]$x)) / pi * 360
		idx <- which(abs(angles)<=tolerance)
		dd <- sapply(remaining[idx], function(v) (V(g)[start]$x-V(g)[v]$x)^2 + (V(g)[start]$y-V(g)[v]$y)^2)
		v <- remaining[idx[which.min(dd)]]
		remaining <- remaining[-idx[which.min(dd)]]
		if(are.connected(graph=g, v1=u, v2=v))
			v <- end
		else
			g <- add_edges(graph=g, edges=c(u,v), attr=list(name=e.name))
		u <- v
		path <- c(path, u)
	}
	
	res <- list(g=g, path=path)
	return(res)
}




###############################################################################
g <- make_empty_graph(n=0, directed=FALSE)

# create city center
center.x <- 0
center.y <- 0
points.ys <- c(points.ys, center.y)
g <- add_vertices(graph=g, nv=1, attr=list(name="center", type="center", x=center.x, y=center.y))

## external wall
#ext.xs <- runif(n=n.av, min=-1, max=1)
#ext.ys <- runif(n=n.av, min=-1, max=1)
#points.ys <- c(points.ys, ext.ys)
#g <- add_vertices(graph=g, nv=n.av, attr=list(name=paste0("ext_wall_",1:n.av), x=ext.xs, y=ext.ys))
#g <- add_edges(graph=g, edges=c(rbind(rep("center",n.av),paste0("ext_wall_",1:n.av))), attr=list(name=paste0("avenue_",1:n.av)))
#types <- c(types, rep("ext_wall",n.av))
#
## internal wall
#tmp <- runif(n=n.av, min=0.5, max=0.8)
#int.xs <- tmp*ext.xs
#int.ys <- tmp*ext.ys
#points.ys <- c(points.ys, int.ys)
#for(a in 1:n.av)
#{	g <- split.edge(
#		g=g, 
#		v1="center", v2=paste0("ext_wall_",a),
#		x=int.xs[a], y=int.ys[a], 
#		v.name=paste0("int_wall_",a)
#	)
#}
#types <- c(types, rep("int_wall",n.av))
#
## additional points
#sec.xs <- runif(n=n.sec, min=min(ext.xs), max=max(ext.xs))
#sec.ys <- runif(n=n.sec, min=min(ext.ys), max=max(ext.ys))
#g <- add_vertices(graph=g, nv=n.sec, attr=list(name=paste0("secondary_",1:n.sec), x=sec.xs, y=sec.ys))
#types <- c(types, rep("secondary",n.sec))

# create additional points
r <- sqrt(runif(n=n.sec, min=0, max=1))
alpha <- runif(n=n.sec, min=0, max=2*pi)
sec.xs <- r*cos(alpha)
sec.ys <- r*sin(alpha)
g <- add_vertices(graph=g, nv=n.sec, attr=list(name=paste0("secondary_",1:n.sec), type="secondary", x=sec.xs, y=sec.ys))

# create external wall
hull <- convhulln(p=cbind(sec.xs,sec.ys), options="n")$hull
r <- 1
wall <- hull[r,1] + 1
while(nrow(hull)>0)
{	# create next edge
	v1 <- hull[r,1]
	v2 <- hull[r,2]
	V(g)[v1+1]$type <- "ext_wall"
	V(g)[v2+1]$type <- "ext_wall"
	wall <- c(wall, v2+1)
	g <- add_edges(graph=g, edges=c(v1+1,v2+1), attr=list(name="ext_wall"))
	# update
	hull <- hull[-r,,drop=FALSE]
	r <- which(hull[,1]==v2)
	if(length(r)==0)
	{	r <- which(hull[,2]==v2)
		tmp <- hull[r,1]
		hull[r,1] <- hull[r,2]
		hull[r,2] <- tmp
	}
}

# create avenues
step <- floor(length(wall) / n.av)
sq <- seq(1,length(wall),step)
mids <- c()
a <- 1
for(i in sq)
{	tlog("Adding avenue starting at vertex #",i)
	tmp <- build.path(g, start=wall[i], end=which(V(g)$name=="center"), e.name=paste0("avenue_",a))
	g <- tmp$g
	path <- tmp$path
print(path)	
	mids <- c(mids, path[ceiling(length(path)/2)])
	a <- a + 1
}
mids <- c(mids, mids[1])

# add internal wall
for(i in 1:n.av)
	g <- build.path(g, start=mids[i], end=mids[i+1], e.name="int_wall")$g

# add minor streets
idx <- which(V(g)$type=="secondary")
V(g)[idx]$deg <- sample(x=1, size=length(idx), replace=TRUE)
z <- 1
for(i in 1:length(idx))
{	v <- idx[i]
	tlog("Processing node #",v," (",i,"/",length(idx),")")
	while(V(g)[v]$deg>0)
	{	cx <- sample(c("v","e"), size=1)
		# connect to existing vertex
#		if(cx=="v")
		{	neis <- neighbors(graph=g, v=v, mode="all")
			others <- (1:gorder(g))[-c(v,neis)]
			dd <- sapply(others, function(u) (V(g)[v]$x-V(g)[u]$x)^2 + (V(g)[v]$y-V(g)[u]$y)^2)
			u <- others[which.min(dd)]
			g <- add_edges(graph=g, edges=c(v,u), attr=list(name=paste0("street_",z), type="street"))
			z <- z + 1
			V(g)[u]$deg <- V(g)[u]$deg - 1
		}
		# connect to street, requires creating vertex
#		else if(cx=="e")
#		{	
#			
#		}
		V(g)[v]$deg <- V(g)[v]$deg - 1
	}
	plot(g, vertex.label=1:gorder(g), vertex.color=match(V(g)$type,unique(V(g)$type)), vertex.size=4)
	readline(prompt="Press [enter] to continue")
}

# faut récupérer le graphe dual ?
# simplifier les noeuds de degré 2 => même rue



###############################################################################
plot(g, 
	vertex.label=1:gorder(g), 	# 1:gorder(g)
	vertex.color=match(V(g)$type,unique(V(g)$type)), 
	vertex.size=4
)
