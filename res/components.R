#
# Vincent Labatut
# 2020/04
#
########################################################################
# graph library
library("igraph")

# color constants
source("res/colors.R")

# data folder
in.folder <- "in"
out.folder <- "out"

# retrieve the tables
const.tab <- read.table(file.path(in.folder,"constraints.txt"),sep="\t",header=TRUE,stringsAsFactors=FALSE)
obj.tab <- read.table(file.path(in.folder,"objects.txt"),sep="\t",header=TRUE,stringsAsFactors=FALSE)

# build the graph
g <- graph_from_edgelist(el=as.matrix(const.tab[,c("Source","Target")]),directed=TRUE)
V(g)$type <- obj.tab[match(V(g)$name,obj.tab[,"Id"]),"Type"]
obj.types <- sort(unique(V(g)$type))
E(g)$type <- const.tab[,"Label"]
const.types <- sort(unique(E(g)$type))

# identify the components
tmp <- components(g, mode="weak")
ncomp <- tmp$no
sizes <- tmp$csize
membership <- tmp$membership

# record results
res.tab <- cbind(1:ncomp, sizes)
colnames(res.tab) <- c("component","size")
write.table(res.tab,file.path(out.folder,"components_sizes.txt"),sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)
res.tab <- cbind(V(g)$name,membership)
colnames(res.tab) <- c("node","component")
write.table(res.tab,file.path(out.folder,"components.txt"),sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)

# plotting each component separately
for(comp in 1:ncomp)
{	#comp <- 2
	cat("Processing component ",comp,"/",ncomp,"\n",sep="")
	# get subgraph corresponding to the component
	idx <- which(membership==comp)
	gcomp <- induced_subgraph(graph=g, vids=idx)
	
	# setup colors 
	vcols <- CAT_COLORS_18[match(V(gcomp)$type, obj.types)]
	ecols <- CAT_COLORS_18[match(E(gcomp)$type,const.types)]
	V(gcomp)$color <- vcols
	E(gcomp)$color <- ecols
	
	# compute centralities and record them
	centr.vals <- matrix(0,nrow=gorder(gcomp),ncol=0)
		# degree
		centr <- degree(gcomp,mode="all")
		V(gcomp)$degree <- centr
		centr.vals <- cbind(centr.vals,centr)
		colnames(centr.vals)[ncol(centr.vals)] <- "Degree"
		# eigencentrality
		centr <- eigen_centrality(gcomp, directed=FALSE, scale=TRUE)$vector
		V(gcomp)$spectral <- centr
		centr.vals <- cbind(centr.vals,centr)
		colnames(centr.vals)[ncol(centr.vals)] <- "Spectral"
		# closeness
		centr <- closeness(gcomp,mode="all", normalized=TRUE)
		V(gcomp)$closeness <- centr
		centr.vals <- cbind(centr.vals,centr)
		colnames(centr.vals)[ncol(centr.vals)] <- "Closeness"
		# betweenness
		centr <- betweenness(gcomp, directed=FALSE, normalized=TRUE)
		V(gcomp)$betweenness <- centr
		centr.vals <- cbind(centr.vals,centr)
		colnames(centr.vals)[ncol(centr.vals)] <- "Betweenness"
	centr.vals <- cbind(V(gcomp)$name, centr.vals)
	colnames(centr.vals)[1] <- "Node"
	centr.file <- file.path(out.folder, paste0("comp_",comp,"_centralities.txt"))
	write.table(centr.vals, file=centr.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
	
	# open plot output
	#pdf(file.path(folder,paste0("comp_",comp,".pdf")))
	png(file.path(out.folder,paste0("comp_",comp,".png")), 
			width=2048, height=2048)
	#par(mar=c(5, 4, 4, 2)+0.1)	# B L T R
	
	# possibly load positions
	pos.file <- file.path(in.folder, paste0("positions_CC",comp,".txt"))
	if(file.exists(pos.file))
	{	# retrieve positions
		pos <- read.table(pos.file, header=TRUE, stringsAsFactors=FALSE)
# delete nodes without position		
map <- match(V(gcomp)$name, pos[,"Name"])
gcomp <- delete_vertices(gcomp, which(is.na(map)))
		map <- match(pos[,"Name"], V(gcomp)$name)
		lay <- matrix(0,nrow=gorder(gcomp),ncol=2)
		lay[map,] <- cbind(as.integer(pos[,"Xpos"]),as.integer(pos[,"Ypos"]))
		# plot component
		if(vcount(gcomp)>20)
			plot(gcomp, 
				vertex.size=3, 
				vertex.color=vcols,
				vertex.label.cex=0.7, #vertex.label=NA, 
				edge.color=ecols,
				edge.arrow.size=0.2,
				layout=lay
			)
		else
			plot(gcomp, 
				vertex.color=vcols,
				edge.color=ecols,
				edge.arrow.size=0.8,
				layout=lay
			)
		# add position to graph
		V(gcomp)$x <- lay[,1]
		V(gcomp)$y <- lay[,2]
	}
	
	else
	{	if(vcount(gcomp)>20)
		{	labels <- V(g)$name
			labels[c.deg<20] <- NA
			lay <- layout_with_graphopt(gcomp, charge=0.010)
			lay <- layout_with_fr(gcomp)
			lay <- layout_with_kk(gcomp)
			plot(gcomp, 
				vertex.size=1+5*centr/max(centr), 
				vertex.color=vcols,
				vertex.label=labels,
				edge.color=ecols,
				edge.arrow.size=0.2,
				layout=lay,
#				frame=TRUE
#				margin=c(0,0,4,4)		# B T L R
			)
			V(gcomp)$x <- lay[,1]
			V(gcomp)$y <- lay[,2]
		}
		else
    		plot(gcomp, 
				vertex.color=vcols,
				edge.color=ecols,
				edge.arrow.size=0.8,
			)
	}
	
	# node legend
	legend(
		title="Objets",								# title of the legend box
		x="bottomleft",								# position
		legend=obj.types,							# text of the legend
		fill=CAT_COLORS_18[1:length(obj.types)],	# color of the nodes
		bty="n",									# no box around the legend
		cex=0.8										# size of the text in the legend
	)
	
	# edge legend
	legend(
		title="Contraintes",							# title of the legend box
		x="bottomright",								# position
		legend=const.types,								# text of the legend
		col=CAT_COLORS_18[1:length(const.types)],		# color of the lines
		lwd=4,											# line thickness
		bty="n",										# no box around the legend
		cex=0.8,										# size of the text in the legend
		seg.len=3										# length of the line in the legend
	)
	
	# close plot output
    dev.off()
	
	# export as graphml
	graph.file <- file.path(out.folder,paste0("comp_",comp,".graphml"))
	write.graph(graph=gcomp, file=graph.file, format="graphml")
}
