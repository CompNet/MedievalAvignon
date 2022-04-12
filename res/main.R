########################################################################
# Analyzes the social networks.
# 
# Vincent Labatut
# 06/2020
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/main.R")
########################################################################
# load other scripts
source("res/common/_include.R")
source("res/analysis/extract_graphs.R")
source("res/measures/_compute_measures.R")




########################################################################
# start logging
start.rec.log(text="Nets")




########################################################################
## possibly create folder
#dir.create(path=FOLDER_OUT_ANAL_SOC, showWarnings=FALSE, recursive=TRUE)
## load the data and create various versions of the social graph
#extract.social.networks()




########################################################################
## compute topological measures for the extracted networks
#tlog.start.loop(0,length(LK_TYPE_SOC_LST), "Processing the measures for each extracted social graph")
#for(i in 1:length(LK_TYPE_SOC_LST))
#{	tlog.loop(2, i, "Processing the measures for graph ",LK_TYPE_SOC_LST[i]," (",i,"/",length(LK_TYPE_SOC_LST),")")
#	
#	# compute all topological measures
#	g <- analyze.network(gname=LK_TYPE_SOC_LST[i], out.folder=FOLDER_OUT_ANAL_SOC)
#	
#	# same for the augmented graph
#	if(LK_TYPE_SOC_LST[i]==LK_TYPE_PRO || LK_TYPE_SOC_LST[i]==LK_TYPE_ALL)
#	{	tlog.loop(2, i, "Processing the measures for the augmented version of graph ",LK_TYPE_SOC_LST[i]," (",i,"/",length(LK_TYPE_SOC_LST),")")
#		g <- analyze.network(gname=paste0(LK_TYPE_SOC_LST[i],"_c"), out.folder=FOLDER_OUT_ANAL_SOC)
#	}
#}
#tlog.end.loop("Measure computation over")




########################################################################
# possibly create folder
dir.create(path=FOLDER_OUT_ANAL_EST, showWarnings=FALSE, recursive=TRUE)
# load the data and create various versions of the spatial graph
link.types <- extract.estate.networks()




########################################################################
# compute topological measures for the extracted networks
tlog.start.loop(0,length(link.types), "Processing the measures for each extracted estate graph")
for(i in 1:length(link.types))
{	tlog.loop(2, i, "Processing the measures for graph ",link.types[i]," (",i,"/",length(link.types),")")
	
	# compute all topological measures
	g <- analyze.network(gname=link.types[i], out.folder=FOLDER_OUT_ANAL_EST)
	# link.types <- c(LV_ESTATE, LK_TYPE_FLATREL)
	# gname=LV_ESTATE; out.folder=FOLDER_OUT_ANAL_EST
	
	# filtered version
	g <- analyze.network(gname=paste0(link.types[i],"_filtered"), out.folder=FOLDER_OUT_ANAL_EST)
}
tlog.end.loop("Measure computation over")

# additional plot
	algo.name <- "edgebetweenness"
	fname <- paste0("coms_",MEAS_MODE_UNDIR,"_",algo.name)
	# load first graph
	file.path <- file.path(FOLDER_OUT_ANAL_EST, LV_ESTATE, FILE_GRAPH)
	g1 <- load.graphml.file(file=file.path)
	coms.folder <- file.path(FOLDER_OUT_ANAL_EST, g1$name, MEAS_COMMUNITIES, "undirected", algo.name)
	coms <- read.csv(file=file.path(coms.folder,paste0(fname,"_membership.csv")))
	idx <- match(coms[,"Id"], V(g1)$idExterne)
	g1 <- set_vertex_attr(graph=g1, name=fname, index=idx, value=coms)
	# load second graph
	file.path <- file.path(FOLDER_OUT_ANAL_EST, LK_TYPE_FLATREL, FILE_GRAPH)
	g2 <- load.graphml.file(file=file.path)
	V(g2)$label <- paste(vertex_attr(g2,name=COL_LOC_ID), get.location.names(g2),sep="_")
	V(g2)$x <- V(g2)$x2; V(g2)$y <- V(g2)$y2; E(g2)$weight <- 0.5; g2 <- delete_edge_attr(g2, LK_TYPE); g2 <- simplify(g2)
	# use coms from g1
	coms <- rep(NA,gorder(g2))
	idx <- match(V(g1)$idExterne, V(g2)$idExterne)
	idx.u <- which(is.na(idx))
	idx.m <- which(!is.na(idx))
	coms[idx[idx.m]] <- vertex_attr(g1,fname)[idx.m]
	V(g2)$Coms[idx] <- coms
	# plot these coms on g2
	custom.gplot(g=g2, col.att="Coms", cat.att=TRUE, file=file.path(FOLDER_OUT_ANAL_EST,paste0(fname,"_graph_kk")), rescale=FALSE, xlim=range(V(g2)$x), ylim=range(V(g2)$y), edge.arrow.mode=0, vertex.label.cex=0.1)



########################################################################
# end logging
end.rec.log()
