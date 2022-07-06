########################################################################
# Analyzes the social networks.
# 
# Vincent Labatut
# 06/2020
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/main.R")
########################################################################
# load other scripts
source("res/common/_include.R")
source("res/analysis/extract_graphs.R")
source("res/analysis/graph_comp.R")
source("res/measures/_compute_measures.R")




########################################################################
# start logging
start.rec.log(text="Nets")
fast <- TRUE




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
# load the data and create various versions of the spatial graph

# extrat the graphs
	# whole raw
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=FALSE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=FALSE, street.ablation=TRUE)
	# split raw
	graph.types <- extract.estate.networks(split.surf=TRUE, compl.streets=FALSE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=311, compl.streets=FALSE, street.ablation=FALSE)	# 6
	# whole extended
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=TRUE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=TRUE, street.ablation=TRUE)
	# split extended
	graph.types <- extract.estate.networks(split.surf=TRUE, compl.streets=TRUE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=303, compl.streets=TRUE, street.ablation=FALSE)	# 9

#graph.types <- paste0("split_raw/", c(GR_EST_FLAT_REL))
#graph.types <- paste0("split_ext/", c(GR_EST_FLAT_REL))
#graph.types <- paste0("whole_raw/", c(GR_EST_ESTATE_LEVEL, GR_EST_FLAT_REL, GR_EST_FLAT_MINUS))
#graph.types <- paste0("whole_ext/", c(GR_EST_FLAT_REL, GR_EST_FLAT_MINUS))

# plot comparison graphs
##graph.types <- paste0("whole_raw/", c(GR_EST_ESTATE_LEVEL, GR_EST_FLAT_REL, GR_EST_FLAT_MINUS, paste0(GR_EST_ESTATE_LEVEL,"_filtered"), paste0(GR_EST_FLAT_REL,"_filtered"), paste0(GR_EST_FLAT_MINUS,"_filtered"), "flat_minus_6_filtered"))
#plot.graph.comparisons(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)
##graph.types <- paste0("whole_raw/", c(GR_EST_ESTATE_LEVEL, GR_EST_FLAT_REL, GR_EST_FLAT_MINUS, "flat_minus_6"))
#plot.graph.comparisons(graph.names=paste0(graph.types,"_filtered"), folder=FOLDER_OUT_ANAL_EST)
graph.types <- c(
	"split_ext/flat_minus",
	"split_ext/flat_minus_filtered",
	"split_ext/flat_minus_303",
	"split_ext/flat_minus_303_filtered",
	"split_ext/flat_relations",
	"split_ext/flat_relations_filtered",
	#
	"split_raw/flat_minus",
	"split_raw/flat_minus_filtered",
	"split_raw/flat_minus_311",
	"split_raw/flat_minus_311_filtered",
	"split_raw/flat_relations",
	"split_raw/flat_relations_filtered",
	#
	"whole_ext/estate_level",
	"whole_ext/estate_level_filtered",
	"whole_ext/flat_minus",
	"whole_ext/flat_minus_filtered",
	"whole_ext/flat_minus_9",
	"whole_ext/flat_minus_9_filtered",
	"whole_ext/flat_relations",
	"whole_ext/flat_relations_filtered",
	#
	"whole_raw/estate_level",
	"whole_raw/estate_level_filtered",
	"whole_raw/flat_minus",
	"whole_raw/flat_minus_filtered",
	"whole_raw/flat_minus_6",
	"whole_raw/flat_minus_6_filtered",
	"whole_raw/flat_relations",
	"whole_raw/flat_relations_filtered"
)
plot.graph.comparisons(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)

# merge previously computed whole-graph stats
merge.stats(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)

# check street removal step
plot.street.removal(mode="whole_raw")
plot.street.removal(mode="whole_ext")




########################################################################
# compute topological measures for the extracted networks
tlog.start.loop(0,length(graph.types), "Processing the measures for each extracted estate graph")
for(i in 1:length(graph.types))
{	tlog.loop(2, i, "Processing the measures for graph ",graph.types[i]," (",i,"/",length(graph.types),")")
	
	# gname=paste0("whole_raw/", GR_EST_ESTATE_LEVEL); out.folder=FOLDER_OUT_ANAL_EST
	# gname=paste0("whole_raw/", "flat_minus_39_filtered"); out.folder=FOLDER_OUT_ANAL_EST
	
	# compute all topological measures
	g <- analyze.network(gname=graph.types[i], out.folder=FOLDER_OUT_ANAL_EST, fast=fast)
#	g <- analyze.network(gname="", out.folder=FOLDER_OUT_ANAL_EST, fast=FALSE)
	
	# filtered version
	g <- analyze.network(gname=paste0(graph.types[i],"_filtered"), out.folder=FOLDER_OUT_ANAL_EST, fast=fast)
}
tlog.end.loop("Measure computation over")
plot.comstruct.comparison()

# selected version
g <- analyze.network(gname=paste0("whole_raw/", "flat_minus_6_filtered"), out.folder=FOLDER_OUT_ANAL_EST, fast=fast)




########################################################################
# load the data and create various versions of the spatial graph
graph.types <- extract.estate.networks(split.surf=TRUE)
#graph.types <- paste0("split_raw/", c(GR_EST_FLAT_REL))

# compute topological measures for the extracted networks
g <- analyze.network(gname=graph.types[1], out.folder=FOLDER_OUT_ANAL_EST, fast=fast)
g <- analyze.network(gname=paste0(graph.types[1],"_filtered"), out.folder=FOLDER_OUT_ANAL_EST, fast=fast)




########################################################################
# end logging
end.rec.log()

