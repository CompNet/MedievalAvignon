# TODO: Add comment
# 
# Vincent Labatut
# 05/2022
###############################################################################




###############################################################################
# parameters
n <- 100
del.rate <- 0.25




###############################################################################
# sample spatial nodes
x <- runif(n, min=0, max=1)
y <- runif(n, min=0, max=1)
# init graph
g <- make_empty_graph(n, directed=TRUE)
V(g)$x <- x
V(g)$y <- y


# create confront edges
dd <- as.matrix(dist(x=cbind(x,y), ))
# four closest nodes (could be a random number instead + distance limit)
for(i in 1:n)
{	idx <- order(dd[i,])[2:5]
	for(j in idx)
	{	dx <- x[i] - x[j]
		dy <- y[i] - y[j]
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
		g <- add_edges(graph=g, edges=c(i,j), attr=list(type=etype))
	}
}

# plot
types <- LK_TYPE_FLATREL_VALS
epal <- get.palette(length(types))
for(i in 1:length(types))
	ecols[E(g)$type==types[i]] <- epal[i]
plot(g, edge.color=ecols, scale=FALSE, axes=TRUE, vertex.size=1)
legend(title="Link type", x="topright",	legend=LONG_NAME[types], col=epal, lty=1, lwd=4, bty="n")# cex=0.8

# randomly remove some edges
idx <- sample(gsize(g), size=round(del.rate*gsize(g)))
g <- delete_edges(graph=g, edges=idx)

# compare spatial vs. graph distance
