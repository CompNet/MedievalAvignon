# Random model to convert a street network into a confront network.
# 
# Vincent Labatut
# 07/2022
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/model_confronts.R")
###############################################################################
# load other scripts
source("res/common/_include.R")
source("res/model/model_utilities.R")




###############################################################################
# parameters
n.sec <- 200	# size of the street network
n.est <- 1000	# number of buildings
kmax <- 5		# max outgoing degree of building vertices (in terms of confronts)

street.folder <- file.path("out/analysis/estate/model/streets",n.sec)
out.folder <- file.path("out/analysis/estate/model/confronts",n.est)
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)
it <- 1			# iteration number




###############################################################################
# load street graph
graph.file <- file.path(street.folder, paste0(it,".graphml"))
tlog(0,"Loading road network '",graph.file,"'")
g <- load.graphml.file(file=graph.file)




###############################################################################
# create random buildings
tlog(0,"Creating ",n.est," random buildings")
r <- sqrt(runif(n=n.est, min=0, max=1))
alpha <- runif(n=n.est, min=0, max=2*pi)
est.xs <- r*cos(alpha)
est.ys <- r*sin(alpha)
g2 <- add_vertices(graph=g, nv=n.est, attr=list(name=paste0("building_",1:n.est), type="building", x=est.xs, y=est.ys))

# TODO: rather add the building along the streets?
#       (instead of completely randomly)

# plot resulting graph
plot(g2, 
#	vertex.label=1:gorder(g2), vertex.label.cex=0.5,
	vertex.label=NA,
	vertex.color=match(V(g2)$type,unique(V(g2)$type)), 
	vertex.size=sapply(V(g2)$type, function(t) if(t=="building") 1 else 0.1),
	xlim = c(-1, 1), ylim = c(-1, 1), rescale=FALSE
)




###############################################################################
# create confront graph
tlog(0,"Creating empty confront graph")
gconf <- make_empty_graph(n=0, directed=FALSE)

# add buildings
tlog(0,"Adding ",n.est," building vertices to confront graph")
gconf <- add_vertices(graph=gconf, nv=n.est, attr=list(name=paste0("building_",1:n.est), type="building", x=est.xs, y=est.ys))

# add streets
street.names <- sort(unique(E(g)$name))
tlog(0,"Adding ",length(street.names)," street vertices to confront graph")
street.pos <- t(sapply(street.names, function(street.name) # street pos = mean of their constituting vertices
				{	vs <- unique(c(ends(graph=g, es=E(g)[E(g)$name==street.name], names=FALSE)))
					x <- mean(V(g)[vs]$x)
					y <- mean(V(g)[vs]$y)
					return(c(x,y))
				}))
gconf <- add_vertices(graph=gconf, nv=length(street.names), attr=list(name=street.names, type="street", x=street.pos[,1], y=street.pos[,2]))

# draw degrees
tlog(0,"Randomly drawing building vertex degrees")
degs <- sample(1:kmax, size=n.est, replace=TRUE)

# add confronts for buildings
tlog(0,"Looping over all building vertices to add confront edges")
for(v in 1:n.est)
{	tlog(2,"Processing node ",v,"/",n.est)
	# strategy: connect to the k closest buildings/streets
	
	# distance between the vertex and each other vertex
	vs <- (1:n.est)[-v]
	dd.v <- sapply(vs, function(u) sqrt((V(gconf)[v]$x-V(gconf)[u]$x)^2 + (V(gconf)[v]$y-V(gconf)[u]$y)^2))
	names.v <- paste0("building_",vs)
	# test
	#u <- names.v[which.min(dd.v)]
	#points(V(gconf)[v]$x, V(gconf)[v]$y, col="red")
	#points(V(gconf)[u]$x, V(gconf)[u]$y, col="blue")
	
	# distance between the vertex and each street segment
	dd.e <- t(sapply(1:gsize(g), function(e) 
			{	ee <- ends(graph=g, es=e, names=FALSE)
				res <- get.dist.point.segment(x1=V(g)[ee[1]]$x, y1=V(g)[ee[1]]$y, x2=V(g)[ee[2]]$x, y2=V(g)[ee[2]]$y, x3=V(gconf)[v]$x, y3=V(gconf)[v]$y, return.pos=TRUE)
				return(res)
			}))
	names.e <- E(g)$name
	# test
	#e <- which.min(dd.e[,"dist"])
	#ee <- ends(g,E(g)[e],names=FALSE)
	#points(V(g)[ee[1]]$x, V(g)[ee[1]]$y, col="orange")
	#points(V(g)[ee[2]]$x, V(g)[ee[2]]$y, col="purple")
	
	# gather and sort distances
	df <- rbind(data.frame(dist=dd.v, name=names.v, type=rep("building",length(dd.v)), x=V(gconf)$x[vs], y=V(gconf)$y[vs]), 
			data.frame(dist=dd.e[,"dist"], name=names.e, type=rep("street",nrow(dd.e)), x=dd.e[,"x"], y=dd.e[,"y"]))
	df <- df[order(df[,"dist"]),]
	
	# create links in the confront graph
	i <- 1		# loop over the closest buildings/streets
	cnt <- 0	# count the number of edges created
	# stop when target degree reached (or no alternative)
	while(cnt < degs[v] && i<=nrow(df))	
	{	# check if the next vertex was not already confronted
		if(i==1 || !(df[i,"name"] %in% df[1:(i-1),"name"]))
		{	# set confront based on relative position of intersection point
			dx <- V(gconf)[v]$x - df[i,"x"]
			dy <- V(gconf)[v]$x - df[i,"y"]
			if(abs(dx)>abs(dy))
			{	if(dx<0)
					etype <- VAL_CONF_TYPE_EST
				else
					etype <- VAL_CONF_TYPE_OUEST
			}
			else
			{	if(dy<0)
					etype <- VAL_CONF_TYPE_NORD
				else
					etype <- VAL_CONF_TYPE_SUD
			}
			
			# add to graph
			u <- which(V(gconf)$name==df[i,"name"])
			tlog(4,"Creating confront ",v,"--",u," (",V(gconf)[v]$name,"--",V(gconf)[u]$name,")")
			gconf <- add_edges(graph=gconf, edges=c(v,u), attr=list(type=etype))
			cnt <- cnt + 1
		}
		i <- i + 1
	}
	
#	plot(gconf, 
##		vertex.label=1:gorder(gconf), vertex.label.cex=0.5,
#		vertex.label=NA,
#		vertex.color=match(V(gconf)$type,unique(V(gconf)$type)), 
#		vertex.size=1,
#		xlim = c(-1, 1), ylim = c(-1, 1), rescale=FALSE
#	)
#	points(V(gconf)[v]$x, V(gconf)[v]$y, col="red")
#	readline(prompt="Press [enter] to continue")
}

plot(gconf, 
#	vertex.label=1:gorder(gconf), vertex.label.cex=0.5,
	vertex.label=NA,
	vertex.color=match(V(gconf)$type,unique(V(gconf)$type)), 
	vertex.size=1,
	xlim = c(-1, 1), ylim = c(-1, 1), rescale=FALSE
)


# TODO 
# - in order to compare with real network extraction methods, must implement options:
#   - ext >> confronts between streets
#   - split >> automatically split streets and walls
# - options controlling the quality of data:
#   - proportion of unknown locations
#   - proportion of missing confronts
#   - add randomly generic confronts instead of N/S/E/W confronts

# - reproduire la distribution de degré des données ? (mais elle est probablement déjà amputée)
# - reproduire le même nbre de biens vs. rues ? 
# - pb: confronts très éloignés, allant vers le centre >> pq ?
