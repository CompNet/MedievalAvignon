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
g <- graph_from_edgelist(el=as.matrix(const.tab[,c("Source","Target")]),directed=FALSE)
V(g)$type <- obj.tab[match(V(g)$name,obj.tab[,"Id"]),"Type"]
obj.types <- sort(unique(V(g)$type))
E(g)$type <- const.tab[,"Label"]
const.types <- sort(unique(E(g)$type))

# identify the components
ncomp <- count_components(g)
sizes <- components(g)$csize
membership <- components(g)$membership

# record results
sizes <- components(g)$csize
res.tab <- cbind(1:ncomp, sizes)
colnames(res.tab) <- c("component","size")
write.table(res.tab,file.path(out.folder,"components_sizes.txt"),sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)
res.tab <- cbind(V(g)$name,membership)
colnames(res.tab) <- c("node","component")
write.table(res.tab,file.path(out.folder,"components.txt"),sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)

# plotting each component separately
for(comp in 1:ncomp)
{	cat("Processing component ",comp,"/",ncomp,"\n",sep="")
	# get subgraph corresponding to the component
	idx <- which(membership==comp)
	gcomp <- induced_subgraph(graph=g, vids=idx)
	
	# setup colors 
	vcols <- CAT_COLORS_18[match(V(gcomp)$type, obj.types)]
	ecols <- CAT_COLORS_18[match(E(gcomp)$type,const.types)]
	V(gcomp)$color <- vcols
	E(gcomp)$color <- ecols
	
	# open plot output
	#pdf(file.path(folder,paste0("comp_",comp,".pdf")))
	png(file.path(out.folder,paste0("comp_",comp,".png")), 
			width=1024, height=1024)
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
				vertex.size=3, vertex.color=vcols,
				vertex.label.cex=0.7, #vertex.label=NA, 
				edge.color=ecols,
				layout=lay)
		else
			plot(gcomp, 
				vertex.color=vcols,
				edge.color=ecols,
				layout=lay)
		# add position to graph
		V(gcomp)$x <- lay[,1]
		V(gcomp)$y <- lay[,2]
	}
	
	else
	{	if(vcount(gcomp)>20)
			plot(gcomp, 
				vertex.size=3, vertex.color=vcols,
				vertex.label=NA,
				edge.color=ecols,
#				frame=TRUE
#				margin=c(0,0,4,4)		# B T L R
			)
		else
    		plot(gcomp, 
				vertex.color=vcols,
				edge.color=ecols)
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
