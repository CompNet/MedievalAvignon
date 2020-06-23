########################################################################
# Analyzes the social networks.
# 
# Vincent Labatut
# 06/2020
#
# source("res/analysis/socialnets.R")
########################################################################
# load other scripts
source("res/common/include.R")

# possibly create folder
dir.create(path=FOLDER_OUT_ANAL, showWarnings=FALSE, recursive=TRUE)

########################################################################
# load the data and create the graph

# load relationships
data <- read.table(
	file=file.path(FOLDER_IN_ANAL,"LienEntreIndividu.csv"),
	sep=";",
	header=TRUE,
	stringsAsFactors=FALSE,
	quote=""
)

# build graph
edge.list <- cbind(as.character(data[,"Source"]), as.character(data[,"Target"]))
g <- graph_from_edgelist(el=edge.list, directed=TRUE)
E(g)$type <- data[,"typeLien"]
E(g)$label <- data[,"Label"]

# load personal information
info <- read.table(
	file=file.path(FOLDER_IN_ANAL,"DetailsIndividu.csv"),
	sep=",",
	header=TRUE,
	stringsAsFactors=FALSE,
	na.strings="NULL"
)

# complete graph with personal information
idx <- match(V(g)$name, as.character(info[,"idIndividu"]))
#print(length(which(is.na(idx))))	# verification
atts <- setdiff(colnames(info),"idIndividu")
for(att in atts)
	g <- set_vertex_attr(graph=g, name=att, value=info[idx,att])




########################################################################
