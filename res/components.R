#
# Vincent Labatut
# 2020/04
#
########################################################################

# data folder
in.folder <- "in"
out.folder <- "out"

# graph library
library("igraph")

# retrieve the tables
const.tab <- read.table(file.path(in.folder,"constraints.txt"),sep="\t",header=TRUE,stringsAsFactors=FALSE)
obj.tab <- read.table(file.path(in.folder,"objects.txt"),sep="\t",header=TRUE,stringsAsFactors=FALSE)

# build the graph
g <- graph_from_edgelist(el=as.matrix(const.tab[,c("Source","Target")]),directed=FALSE)
V(g)$type[match(V(g)$name,obj.tab[,"Id"])] <- obj.tab[,"Type"]
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
{	idx <- which(membership==comp)
	gcomp <- induced_subgraph(graph=g, vids=idx)
    #pdf(file.path(folder,paste0("comp_",comp,".pdf")))
    png(file.path(out.folder,paste0("comp_",comp,".png")))
		if(vcount(gcomp)>20)
			plot(gcomp, 
					vertex.size=3, vertex.color=match(V(gcomp)$type,obj.types), vertex.label=NA,
					edge.color=match(E(gcomp)$type,const.types))
		else
    		plot(gcomp, 
					vertex.color=match(V(gcomp)$type,obj.types),
					edge.color=match(E(gcomp)$type,const.types))
    dev.off()
}
