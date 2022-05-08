# Random model to study how missing confronts affect the 
# spatial vs. geodesic distances correlation.
# 
# Vincent Labatut
# 05/2022
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/model_remove_edges.R")
###############################################################################
# load other scripts
source("res/common/_include.R")
source("res/analysis/extract_graphs.R")
source("res/measures/_compute_measures.R")




###############################################################################
# parameters
n <- 250
out.folder <- "out/analysis/estate/model/edges"
del.rates <- seq(0,1,by=0.1)




###############################################################################
del.rates <- del.rates[del.rates<1]									# possibly filter deletion rates
dir.create(path=out.folder, showWarnings=FALSE, recursive=TRUE)		# possibly create output folder

# sample spatial nodes
tlog(6,"Sampling spatial nodes")
x <- runif(n, min=0, max=1)
y <- runif(n, min=0, max=1)
# init graph
g <- make_empty_graph(n, directed=TRUE)
V(g)$x <- x
V(g)$y <- y
g <- set_vertex_attr(g, name=COL_LOC_X, value=V(g)$x)
g <- set_vertex_attr(g, name=COL_LOC_Y, value=V(g)$y)
g$type <- GR_TYPE_EST
g$name <- GR_EST_FULL

# create confront edges
tlog(6,"Creating confront edges")
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

# setup edge info
types <- LK_TYPE_FLATREL_VALS
epal <- get.palette(length(types))
#ecols <- rep("gray50", gsize(g1))
#for(i in 1:length(types))
#	ecols[E(g1)$type==types[i]] <- epal[i]

# init correlation table
tlog(6,"Initializing correlation table")
cor.tab <- matrix(NA,nrow=length(del.rates),ncol=6)	
cor.tab <- data.frame(sprintf("%.2f", del.rates), cor.tab)
colnames(cor.tab) <- c("DeletionRate", "PearsonCoef", "PearsonPval", "SpearmanCoef", "SpearmanPval", "KendallCoef", "KendallPval")

tlog(6,"Looping over deletion rates")
for(d in 1:length(del.rates))
{	del.rate <- del.rates[d]
	dr <- sprintf("%.2f", del.rate)
	tlog(8,"Processing del.rate=",dr," (",d,"/",length(del.rates),")")
	
	# randomly remove some edges
	tlog(10,"Removing edges")
	idx <- sample(gsize(g), size=round(del.rate*gsize(g)))
	g1 <- g
	if(length(idx)>0)
		g1 <- delete_edges(graph=g1, edges=idx)
	
	# plot graph
	plot.file <- file.path(out.folder,paste0("graph_del-rate=",dr))
	tlog(10,"Plotting graph in ",plot.file)
	custom.gplot(g=g1, file=plot.file, asp=1, size.att=2, vertex.label.cex=0.1, edge.arrow.mode=0)
	#plot(g1, edge.color=ecols, scale=FALSE, axes=TRUE, vertex.size=1)
	#legend(title="Link type", x="topright",	legend=LONG_NAME[types], col=epal, lty=1, lwd=4, bty="n")# cex=0.8
	
	# compute spatial distance
	tlog(10,"Computing spatial distances")
	coords <- cbind(V(g1)$x, V(g1)$y)
	svals <- as.matrix(dist(x=coords, method="euclidean", diag=TRUE, upper=TRUE))
	svals <- svals[upper.tri(svals)]
	# plot distribution
	plot.file <- file.path(out.folder,paste0("histo_spatial_del-rate=",dr))
	custom.hist(vals=svals, "Spatial distance", file=plot.file)
	
	# compute undirected graph distance
	tlog(10,"Computing undirected graph distance")
	gvals <- distances(graph=g1, mode="all")
	gvals <- gvals[upper.tri(gvals)]
	idx <- !is.infinite(gvals)
	gvals <- gvals[idx]
	svals <- svals[idx]
	# plot distribution
	plot.file <- file.path(out.folder,paste0("histo_graph_del-rate=",dr))
	custom.hist(vals=gvals, "Graph distance", file=plot.file)
	
	# compute correlations
	tlog(10,"Computting correlations:")
	tmp <- cor.test(x=gvals, y=svals, method="pearson")
	cor.tab[d,"PearsonCoef"] <- tmp$estimate
	cor.tab[d,"PearsonPval"] <- tmp$p.value
	tlog(12,"Pearson: ",tmp$estimate," (p=",tmp$p.value,")")
#	tmp <- cor.test(x=gvals, y=svals, method="spearman")
#	cor.tab[d,"SpearmanCoef"] <- tmp$estimate
#	cor.tab[d,"SpearmanPval"] <- tmp$p.value
#	tlog(12,"Spearman: ",tmp$estimate," (p=",tmp$p.value,")")
	tmp <- cor.test(x=gvals, y=svals, method="kendall")
	cor.tab[d,"KendallCoef"] <- tmp$estimate
	cor.tab[d,"KendallPval"] <- tmp$p.value
	tlog(12,"Kendall: ",tmp$estimate,"(p=",tmp$p.value,")")
	# NOTE: null hypothesis=zero correlation >> small p means this hypothesis is rejected
		
	# plot the spatial distance as a function of the graph-based one
	plot.file <- file.path(out.folder, paste0("graph_vs_spatial_del-rate=",dr))
	tlog(10,"Plotting spatial vs. graph distances in ",plot.file)
	for(fformat in FORMAT)
	{	if(fformat=="pdf")
			pdf(paste0(plot.file,".pdf"))
		else if(fformat=="png")
			png(paste0(plot.file,".png"))
			plot(
				x=gvals, y=svals, 
				xlab="Undirected graph distance", ylab="Spatial distance",
				#log="xy", 
				las=1, col=make.color.transparent("RED",75)
			#xlim=c(1,max(deg.vals)*1.1)
			)
			# mean
			avg.dist <- sapply(min(gvals):max(gvals), function(deg) mean(svals[gvals==deg]))
			lines(	
				x=min(gvals):max(gvals), avg.dist,
				col="BLACK"
			)
		dev.off()
	}
		
	# same using degree for colors
	meas <- MEAS_DEGREE
	vals <- igraph::degree(graph=g1, mode="all")
	cb <- t(combn(1:gorder(g1),2))
	vals <- (vals[cb[,1]] * vals[cb[,2]])[idx]
	# set colors
	fine <- 500 									# granularity of the color gradient
	cols <- viridis(fine,direction=-1)[as.numeric(cut(vals,breaks=fine))]
	plot.file <- file.path(out.folder, paste0("graph_vs_spatial_col=",meas,"_del-rate=",dr))
	tlog(10,"Plotting spatial vs. graph distances using ",meas," for color in ",plot.file)
	for(fformat in FORMAT)
	{	if(fformat=="pdf")
			pdf(paste0(plot.file,".pdf"))
		else if(fformat=="png")
			png(paste0(plot.file,".png"))
			plot(
				x=gvals[order(vals)], y=svals[order(vals)], 
				xlab="Undirected graph distance", ylab="Spatial distance",
				#log="xy", 
				las=1, col=cols[order(vals)],
			#xlim=c(1,max(deg.vals)*1.1)
			)
			# mean
			avg.dist <- sapply(min(gvals):max(gvals), function(deg) mean(svals[gvals==deg]))
			lines(	
				x=min(gvals):max(gvals), avg.dist,
				col="BLACK"
			)
			# legend
			gradientLegend(range(vals), color=viridis(fine,direction=-1), inside=TRUE)
		dev.off()
	}
}

# dislay/record correlation results
tab.file <- file.path(out.folder, "correlations.csv")
tlog(6,"Recording correlation results in ",tab.file)
cor.tab <- data.frame(cor.tab)
print(cor.tab)
write.csv(cor.tab, file=tab.file, row.names=FALSE)	
# plot
plot.file <- file.path(out.folder, "correlation_vs_del-rate")
for(fformat in FORMAT)
{	if(fformat=="pdf")
		pdf(paste0(plot.file,".pdf"))
	else if(fformat=="png")
		png(paste0(plot.file,".png"))
	plot(NULL,xlim=range(del.rates),ylim=c(0,1),xlab="Deletion rate",ylab="Correlation value")
	cor.meass <- c("Pearson","Spearman","Kendall")
	for(i in 1:length(cor.meass))
	{	cor.meas <- cor.meass[i]
		lines(x=del.rates, y=cor.tab[,paste0(cor.meas,"Coef")], col=epal[i])
	}
	legend(x="bottomleft",fill=epal[1:length(cor.meass)],legend=cor.meass)
	dev.off()
}
