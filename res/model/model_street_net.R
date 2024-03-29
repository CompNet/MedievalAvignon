# Random model to create a relatively realistic street network.
# It is designed to mimic Avignon:
# - central point at (0,0);
# - avenues converging towards this center;
# - convex envelope representing the external walls;
# - internal wall placed mid-avenue;
# - rest of the points used to define minor streets.
# 
# Vincent Labatut
# 06/2022
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/model_street_net.R")
###############################################################################
# load other scripts
source("res/common/_include.R")
source("res/model/model_utilities.R")




###############################################################################
# parameters
n.sec <- 200	# number of free points
n.av <- 5		# number of avenues
out.folder <- file.path("out/analysis/estate/model/streets",n.sec)
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)




###############################################################################
tlog(0,"Generating a graph similar to Avignon")
g <- make_empty_graph(n=0, directed=FALSE)

# create city center
center.x <- 0
center.y <- 0
g <- add_vertices(graph=g, nv=1, attr=list(name="center", type="center", x=center.x, y=center.y))
tlog(2,"Initializing city center: (",center.x,";",center.y,")")

# create additional points
tlog(2,"Adding ",n.sec," random points")
r <- sqrt(runif(n=n.sec, min=0, max=1))
alpha <- runif(n=n.sec, min=0, max=2*pi)
sec.xs <- r*cos(alpha)
sec.ys <- r*sin(alpha)
g <- add_vertices(graph=g, nv=n.sec, attr=list(name=paste0("secondary_",1:n.sec), type="secondary", x=sec.xs, y=sec.ys))

# remove those too close
min.dist <- 0.1
tlog(2,"Removing close points: tolerance=",min.dist)
dd <- as.matrix(dist(cbind(V(g)$x,V(g)$y), method="euclidean", diag=FALSE, upper=FALSE))
diag(dd) <- NA
idx <- which(dd<min.dist,arr.ind=TRUE)
i <- 0
while(nrow(idx)>0)
{	tlog(4,"Processing close pair (",nrow(idx)," pairs remaining)")
	if(idx[1,1]==1)
		idx[1,1] <- idx[1,2]
	else if(idx[1,2]==1)
		idx[1,2] <- idx[1,1]
	g <- delete_vertices(graph=g, v=sample(idx[1,],1))
	dd <- as.matrix(dist(cbind(V(g)$x,V(g)$y),method="euclidean",diag=FALSE, upper=FALSE))
	diag(dd) <- NA
	idx <- which(dd<min.dist,arr.ind=TRUE)
	i <- i + 1
}
tlog(4,"Result of the process:")
tlog(6,"Removed: ",i," points")
tlog(6,"Remaining: ",gorder(g)," points")
#n.sec <- gorder(g) - 1
sec.xs <- V(g)$x[-1]
sec.ys <- V(g)$y[-1]
#head(sort(c(dd[upper.tri(dd, diag=FALSE)])))

# create external wall
tlog(2,"Adding external wall = convex hull")
hull <- convhulln(p=cbind(sec.xs,sec.ys), options="n")$hull
r <- 1
wall <- hull[r,1] + 1
while(nrow(hull)>0)
{	# create next edge
	v1 <- hull[r,1]
	v2 <- hull[r,2]
	tlog(4,"Creating edge ",v1+1,"--",v2+1)
	V(g)[v1+1]$type <- "ext_wall"
	V(g)[v2+1]$type <- "ext_wall"
	wall <- c(wall, v2+1)
	g <- add_edges(graph=g, edges=c(v1+1,v2+1), attr=list(name="ext_wall", type="ext_wall"))
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
tlog(4,"Path of the wall: ",paste0(wall,collapsed=", "))
plot(g, vertex.label=NA, vertex.color=match(V(g)$type,unique(V(g)$type)), edge.color=CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))], vertex.size=3)
g1 <- g

# create avenues
tlog(2,"Adding radial avenues")
wall <- wall[-length(wall)]
step <- floor(length(wall) / n.av)
sq <- seq(1,length(wall),step)
tlog(2,"Adding ",length(sq)," radial avenues")
mids <- c()
a <- 1
for(i in sq)
{	tlog(4,"Adding avenue starting at vertex #",wall[i])
	tmp <- build.path(g, start=wall[i], end=which(V(g)$name=="center"), e.type="avenue", e.name=paste0("avenue_",a))
	g <- tmp$g
	path <- tmp$path
	tlog(6,"Avenue path: ",paste0(path,collapse=", "))	
	mids <- c(mids, path[ceiling(length(path)/2)])
	a <- a + 1
}
mids <- c(mids, mids[1])
plot(g, vertex.label=NA, vertex.color=match(V(g)$type,unique(V(g)$type)), edge.color=CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))], vertex.size=3)
g2 <- g

# add internal wall
tlog(6,"Adding internal wall = going through avenues midpoint, path: ",paste0(mids,collapse=", "))	
for(i in 1:(length(mids)-1))
	g <- build.path(g, start=mids[i], end=mids[i+1], e.type="int_wall", e.name="int_wall")$g
plot(g, vertex.label=NA, vertex.color=match(V(g)$type,unique(V(g)$type)), edge.color=CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))], vertex.size=3)
g3 <- g

# add minor streets
min.length <- 0.1	# min length when splitting edges
tlog(2,"Adding minor streets")	
idx <- which(V(g)$type=="secondary")
V(g)$deg <- sample(x=2:4, size=gorder(g), replace=TRUE)  - degree(graph=g,mode="all")
V(g)$deg <- sapply(V(g)$deg, function(d) max(0, d))
changed <- TRUE
while(changed)
{	idx <- sample(idx)
	changed <- FALSE
	tlog(4,"----- Iteration -----")
	
	for(i in 1:length(idx))
	{	v <- idx[i]
		tlog(4,"Processing node #",v,": deg=",V(g)[v]$deg," (",i,"/",length(idx),")")
		if(V(g)[v]$deg>0)
		{	vrtx <- TRUE
			# connect to street, requires creating vertex
				el <- as_edgelist(graph=g, names=FALSE)
				lengths <- sapply(1:nrow(el), function(r) sqrt((V(g)[el[r,1]]$x-V(g)[el[r,2]]$x)^2 + (V(g)[el[r,1]]$y-V(g)[el[r,2]]$y)^2))
				neis <- c(as.integer(neighbors(graph=g, v=v, mode="all")), v)
				# filter if attached to vertex or direct neighbors
				ee <- unique(unlist(sapply(neis, function(u) as.integer(incident(graph=g, v=u, mode="all")))))
				# or if already the result of previous split
				ee <- union(ee, which(V(g)[el[,1]]$type=="tertiary" & V(g)[el[,2]]$type=="tertiary"))
				# or if too short to be split
				ee <- union(ee, which(lengths<=min.length))
				others <- setdiff(1:gsize(g), ee)
			if(length(others)>0)
			{	# compute end points with target edge, and keep only intersection-free cases
				end.pts <- t(sapply(others, function(e) c(mean(c(V(g)[el[e,1]]$x,V(g)[el[e,2]]$x)), mean(c(V(g)[el[e,1]]$y, V(g)[el[e,2]]$y)))))
				intersections <- sapply(1:length(others), function(i) check.potential.edge.crossing(delete_edges(g,c(others[i],incident(graph=g,v=v,mode="all"))), v1=v, x2=end.pts[i,1], y2=end.pts[i,2]))
				#intersections <- sapply(1:length(others), function(i) check.segment.crossing(x1=V(g)[el[others[i],1]]$x, y1=V(g)[el[others[i],1]]$y, x2=V(g)[el[others[i],2]]$x, y2=V(g)[el[others[i],2]]$y, x3=V(g)[v]$x, y3=V(g)[v]$y, x4=end.pts[i,1], y4=end.pts[i,2]))
				#print(cbind(el[others,],intersections))
				others <- others[!intersections]
				end.pts <- end.pts[!intersections,,drop=FALSE]
				if(length(others)>0)
				{	# compute distances and keep closest one
					dd <- apply(end.pts, 1, function(r) (V(g)[v]$x-r[1])^2 + (V(g)[v]$y-r[2])^2)
					s <- which.min(dd)
					tlog(6,"Connecting to edge ",el[others[s],1],"--",el[others[s],2]," (new vertex ",gorder(g)+1,")")
					# apply modification
					g <- split.edge(g, v1=el[others[s],1], v2=el[others[s],2], x=end.pts[s,1], y=end.pts[s,2], v.name="tertiary")
					u <- as.integer(V(g)[gorder(g)])
					g <- add_edges(graph=g, edges=c(v,u), attr=list(name=NA, type="street"))
					V(g)[u]$deg <- sample(x=0:1, size=1)
					vrtx <- FALSE
					changed <- TRUE
				}
			}
			
			# connect to existing vertex
			if(vrtx)
			{	# select possible vertices
				neis <- as.integer(neighbors(graph=g, v=v, mode="all"))		# ignore neighbors
				full <- which(V(g)$deg<1)									# ignore vertices whose target degree is already met
				tmp <- setdiff(1:gorder(g),c(neis,v,full))
				aligned <- tmp[sapply(tmp, function(u)						# ignore vertices aligned with an incident edge
							any(sapply(neis, function(nei) check.alignment(g, v, nei, u))))]
				others <- setdiff(1:gorder(g), c(v,neis,full,aligned))
				dd <- sapply(others, function(u) (V(g)[v]$x-V(g)[u]$x)^2 + (V(g)[v]$y-V(g)[u]$y)^2)
				u <- others[which.min(dd)]
				tlog(6,"Connecting to vertex ",u)
				# create new edge
				g <- add_edges(graph=g, edges=c(v,u), attr=list(name=NA, type="street"))
				V(g)[u]$deg <- V(g)[u]$deg - 1
				changed <- TRUE
			}
			
#			v.cols <- match(V(g)$type,unique(V(g)$type))
#			e.cols <- CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))]
#			v.sizes <- rep(3, gorder(g)); v.sizes[c(u,v)] <- rep(9,2)
#			e.widths <- rep(1,gsize(g)); e.widths[gsize(g)] <- 3
#			v.labels <- 1:gorder(g)
#			v.labels <- NA
#			plot(g, vertex.label=v.labels, vertex.color=v.cols, vertex.size=v.sizes, edge.color=e.cols, edge.width=e.widths)
#			readline(prompt="Press [enter] to continue")
			
#			# verify if some vertices are aligned (debugging)
#			neis <- as.integer(neighbors(graph=g, v=v, mode="all"))
#			if(length(neis)>1)
#			for(i in 1:(length(neis)-1))
#			{	for(j in (i+1):length(neis))
#				{	if(are.connected(g,neis[i],neis[j]) && check.alignment(g, v, neis[i], neis[j]))
#						error("ERROR: Aligned and connected triad")
#				}
#			}
			
			V(g)[v]$deg <- V(g)[v]$deg - 1
		}
	}
}
plot(g, vertex.label=NA, vertex.color=match(V(g)$type,unique(V(g)$type)), edge.color=CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))], vertex.size=3)
g4 <- g

# possibly add vertices for intersection points
tlog(2,"Adding vertices for intersection points")
g <- add.intersection.vertices(g)
plot(g, vertex.label=NA, vertex.color=match(V(g)$type,unique(V(g)$type)), edge.color=CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))], vertex.size=3)
g5 <- g

# name unnamed streets
tlog(2,"Adding street names")
z <- 1
unnamed.edges <- which(is.na(E(g)$name))
while(length(unnamed.edges)>0)
{	# name next edge
	str.name <- paste0("street_",z)
	tlog(4,"Naming street ",str.name)
	path <- c()
#	el <- as_edgelist(graph=g, names=FALSE)
	e <- unnamed.edges[1]
	unnamed.edges <- unnamed.edges[-1]
	E(g)[e]$name <- str.name
	path <- c(ends(graph=g, es=e, names=FALSE))
	tlog(6,"Starting with edge #",e," (",path[1],"--",path[2],")")
	
	# possibly propagate name to adjacent edges
	inc.edges <- unique(unlist(lapply(incident_edges(graph=g, v=ends(graph=g, es=e, names=FALSE), mode="all"), as.integer)))
	inc.edges <- setdiff(inc.edges, e)					# remove current edge
	inc.edges <- intersect(inc.edges, unnamed.edges)	# keep only unnamed edges
	#print(ends(g,inc.edges,F))
	if(length(inc.edges)>0)
	{	# keep edge with flatter angle
		common.ends <- sapply(1:length(inc.edges), function(i) intersect(c(ends(graph=g, es=e, names=FALSE)), c(ends(graph=g, es=inc.edges[i], names=FALSE))))
		old.ends <- sapply(1:length(inc.edges), function(i) setdiff(c(ends(graph=g, es=e, names=FALSE)), common.ends[i]))
		new.ends <- sapply(1:length(inc.edges), function(i) setdiff(c(ends(graph=g, es=inc.edges[i], names=FALSE)), common.ends[i]))
		angles <- (atan2(V(g)[common.ends]$y - V(g)[old.ends]$y, V(g)[common.ends]$x - V(g)[old.ends]$x) - 
					atan2(V(g)[new.ends]$y - V(g)[common.ends]$y, V(g)[new.ends]$x - V(g)[common.ends]$x)) / pi * 180
		angles[angles>180] <- angles[angles>180] - 360
		angles[angles<(-180)] <- angles[angles<(-180)] + 360
		angles[angles<0] <- -angles[angles<0]
		# filtering if angle too acute
		is <- which(angles<90 | degree(graph=g,v=common.ends,mode="all")<3)
		inc.edges <- inc.edges[is]
		common.ends <- common.ends[is]
		old.ends <- old.ends[is]
		new.ends <- new.ends[is]
		angles <- angles[is]
		# using the edge of smallest angle
		if(length(angles)>0)
		{	i <- which.min(angles)
			tlog(6,"Adding edge ",common.ends[i],"--",new.ends[i])
			e <- inc.edges[i]
			unnamed.edges <- unnamed.edges[-which(unnamed.edges==e)]
			E(g)[e]$name <- str.name
			path <- c(old.ends[i], common.ends[i], new.ends[i])
			tlog(8,"Current path: ",paste0(path,colapse=", "))
			
			# try to continue, but vertex-based this time
			goOn <- TRUE
			while(goOn)
			{	goOn <- FALSE
				v <- new.ends[i]
				inc.edges <- as.integer(incident(graph=g, v=v, mode="all"))
				inc.edges <- setdiff(inc.edges, e)					# remove current edge
				inc.edges <- intersect(inc.edges, unnamed.edges)	# keep only unnamed edges
				#print(ends(g,inc.edges,F))
				if(length(inc.edges)>0)
				{	# keep edge with flatter angle
					common.ends <- sapply(1:length(inc.edges), function(i) intersect(c(ends(graph=g, es=e, names=FALSE)), c(ends(graph=g, es=inc.edges[i], names=FALSE))))
					old.ends <- sapply(1:length(inc.edges), function(i) setdiff(c(ends(graph=g, es=e, names=FALSE)), common.ends[i]))
					new.ends <- sapply(1:length(inc.edges), function(i) setdiff(c(ends(graph=g, es=inc.edges[i], names=FALSE)), common.ends[i]))
					# ignore edges leading to vertices already in the street 
					is <- which(new.ends %in% path)
					if(length(is)>0)
					{	inc.edges <- inc.edges[-is]
						common.ends <- common.ends[-is]
						old.ends <- old.ends[-is]
						new.ends <- new.ends[-is]
					}
					if(length(inc.edges)>0)
					{	angles <- (atan2(V(g)[common.ends]$y - V(g)[old.ends]$y, V(g)[common.ends]$x - V(g)[old.ends]$x) - 
									atan2(V(g)[new.ends]$y - V(g)[common.ends]$y, V(g)[new.ends]$x - V(g)[common.ends]$x)) / pi * 180
						angles[angles>180] <- angles[angles>180] - 360
						angles[angles<(-180)] <- angles[angles<(-180)] + 360
						angles[angles<0] <- -angles[angles<0]
						# filtering if angle too acute
						is <- which(angles<90 | degree(graph=g,v=common.ends,mode="all")<3)
						inc.edges <- inc.edges[is]
						common.ends <- common.ends[is]
						old.ends <- old.ends[is]
						new.ends <- new.ends[is]
						angles <- angles[is]
						# using the edge of smallest angle
						if(length(angles)>0)
						{	i <- which.min(angles)
							tlog(6,"Adding edge ",common.ends[i],"--",new.ends[i])
							e <- inc.edges[i]
							unnamed.edges <- unnamed.edges[-which(unnamed.edges==e)]
							E(g)[e]$name <- str.name
							path <- c(path, new.ends[i])
							tlog(8,"Current path: ",paste0(path,colapse=", "))
							goOn <- TRUE
						}
					}
				}
			}
		}
	}
	
	tlog(6,"Street path: ",paste0(path,collapse=", "))
	z <- z + 1
	#unnamed.edges <- which(is.na(E(g)$name))
	
#	plot(g, 
##		vertex.label=1:gorder(g),
#		vertex.label=NA,
#		vertex.color=match(V(g)$type,unique(V(g)$type)), 
#		edge.color=CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))],
#		edge.width=sapply(E(g)$name, function(nm) if(!is.na(nm) && nm==str.name) 3 else 1),
#		vertex.size=3
#	)
#	readline(prompt="Press [enter] to continue")
}

# draw each street with a different color
plot(g, 
#	vertex.label=1:gorder(g),
	vertex.label=NA,
	vertex.color=match(V(g)$type,unique(V(g)$type)), 
	edge.color=CAT_COLORS_18[(match(E(g)$name,unique(E(g)$name))-1) %% length(CAT_COLORS_18) + 1],
	edge.width=sapply(E(g)$name, function(nm) if(!is.na(nm)) 3 else 1),
	vertex.size=3
)




###############################################################################
# possible improvements for street names: 
# - explore graph in both directions (from the starting edge)
# - break street when crossing avenue

# possible improvement for realism:
# - create plazas at the intersection of important streets?




###############################################################################
#plot(g, 
#	vertex.label=1:gorder(g),
##	vertex.label=NA,
#	vertex.color=match(V(g)$type,unique(V(g)$type)), 
#	edge.color=CAT_COLORS_8[match(E(g)$type,unique(E(g)$type))],
##	edge.width=sapply(E(g)$name, function(nm) if(is.na(nm)) 1 else 3),
#	vertex.size=3
#)

# record next file
ll <- list.files(path=out.folder, patter="+.graphml", full.names=FALSE)
{	if(length(ll)==0)
		i <- 1
	else
		i <- max(as.integer(unlist(strsplit(ll,split=".graphml",fixed=TRUE)))) + 1
	
	# record graphml file
	net.file <- file.path(out.folder,i)
	tlog(2,"Recording procuded graph in file '",net.file,"'")
	write.graph(graph=g, file=paste0(net.file,".graphml"), format="graphml")
	
	# record graph plots
	for(fformat in FORMAT)
	{	if(fformat=="pdf")
			pdf(paste0(net.file,".pdf"))
		else if(fformat=="png")
			png(paste0(net.file,".png"))
		plot(g, 
#			vertex.label=1:gorder(g),
			vertex.label=NA,
			vertex.color=match(V(g)$type,unique(V(g)$type)), 
			edge.color=CAT_COLORS_18[(match(E(g)$name,unique(E(g)$name))-1) %% length(CAT_COLORS_18) + 1],
			edge.width=sapply(E(g)$name, function(nm) if(!is.na(nm)) 3 else 1),
			vertex.size=3
		)
		dev.off()
	}
}
