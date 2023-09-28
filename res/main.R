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
source("res/analysis/ad_hoc.R")
source("res/measures/_compute_measures.R")




########################################################################
# start logging
start.rec.log(text="Nets")




########################################################################
########################################################################




########################################################################
# processing social graphs

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
########################################################################




########################################################################
# load the data and create various versions of the spatial graph

# extract the graphs
	# split extended
	graph.types <- extract.estate.networks(split.surf=TRUE, compl.streets=TRUE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=303,  compl.streets=TRUE, street.ablation=FALSE)		# equiv. 9
	graph.types <- extract.estate.networks(split.surf=311,  compl.streets=TRUE, street.ablation=FALSE)		# equiv. 7
	# whole extended
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=TRUE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=TRUE, street.ablation=TRUE)
	# split raw
	graph.types <- extract.estate.networks(split.surf=TRUE, compl.streets=FALSE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=311,  compl.streets=FALSE, street.ablation=FALSE)		# equiv. 6
	# whole raw
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=FALSE, street.ablation=FALSE)
	graph.types <- extract.estate.networks(split.surf=FALSE, compl.streets=FALSE, street.ablation=TRUE)

# compute and plot street removal-related stats
plot.street.removal(mode="whole_raw")
plot.street.removal(mode="whole_ext")
	
# copy selected street ablation graphs to appropriate folder
sels <- c(7, 6)		# prev. c(9, 6)
fols <- c("whole_ext", "whole_raw")
for(i in 1:length(sels))
{	for(filt in c("","_filtered"))
	{	from.file <- file.path(FOLDER_OUT_ANAL_EST,fols[i],paste0("flat_minus",filt),"_removed_streets","graphs",paste0("graph_rem=",sels[i],".graphml"))
		to.folder <- file.path(FOLDER_OUT_ANAL_EST,fols[i],paste0("flat_minus_",sels[i],filt))
		dir.create(path=to.folder, showWarnings=FALSE, recursive=TRUE)
		tlog(2,"Copying file '",from.file,"' to folder '",to.folder,"'")
		file.copy(
			from=from.file, 
			to=file.path(to.folder,"graph.graphml"),
			overwrite=TRUE
		)
	}
}

# pseudo street-ablation for partial split
partial.street.ablation(mode="split_raw")
plot.street.removal(mode="split_raw")
partial.street.ablation(mode="split_ext")
plot.street.removal(mode="split_ext")




########################################################################
# compute topological measures for the extracted networks
g <- analyze.network(gname="split_ext/full",                    out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_ext/full_filtered",           out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_ext/flat_minus",              out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_ext/flat_minus_filtered",     out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_ext/flat_minus_303",          out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_ext/flat_minus_303_filtered", out.folder=FOLDER_OUT_ANAL_EST, fast=FALSE)
g <- analyze.network(gname="split_ext/flat_minus_311",          out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_ext/flat_minus_311_filtered", out.folder=FOLDER_OUT_ANAL_EST, fast=FALSE)	# selected network
g <- analyze.network(gname="split_ext/flat_relations",          out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_ext/flat_relations_filtered", out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)

g <- analyze.network(gname="split_raw/full",                    out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_raw/full_filtered",			out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_raw/flat_minus",              out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_raw/flat_minus_filtered",     out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_raw/flat_minus_311",          out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_raw/flat_minus_311_filtered", out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_raw/flat_relations",          out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="split_raw/flat_relations_filtered", out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)

g <- analyze.network(gname="whole_ext/full",                    out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/full_filtered",			out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/estate_level",            out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/estate_level_filtered",   out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/flat_minus",              out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/flat_minus_filtered",     out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/flat_minus_9",            out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/flat_minus_9_filtered",   out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/flat_minus_7",            out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/flat_minus_7_filtered",   out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)	# selected network
g <- analyze.network(gname="whole_ext/flat_relations",          out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_ext/flat_relations_filtered", out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)

g <- analyze.network(gname="whole_raw/full",                    out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/full_filtered",			out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/estate_level",            out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/estate_level_filtered",   out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/flat_minus",              out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/flat_minus_filtered",     out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/flat_minus_6",            out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/flat_minus_6_filtered",   out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/flat_relations",          out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)
g <- analyze.network(gname="whole_raw/flat_relations_filtered", out.folder=FOLDER_OUT_ANAL_EST, fast=TRUE)




########################################################################
graph.types <- c(
	"split_ext/full",
	"split_ext/full_filtered",
	"split_ext/flat_minus",
	"split_ext/flat_minus_filtered",
	"split_ext/flat_minus_311",
	"split_ext/flat_minus_311_filtered",
	"split_ext/flat_minus_303",
	"split_ext/flat_minus_303_filtered",
	"split_ext/flat_relations",
	"split_ext/flat_relations_filtered",
	#
	"split_raw/full",
	"split_raw/full_filtered",
	"split_raw/flat_minus",
	"split_raw/flat_minus_filtered",
	"split_raw/flat_minus_311",
	"split_raw/flat_minus_311_filtered",
	"split_raw/flat_relations",
	"split_raw/flat_relations_filtered",
	#
	"whole_ext/full",
	"whole_ext/full_filtered",
	"whole_ext/estate_level",
	"whole_ext/estate_level_filtered",
	"whole_ext/flat_minus",
	"whole_ext/flat_minus_filtered",
	"whole_ext/flat_minus_7",
	"whole_ext/flat_minus_7_filtered",
	"whole_ext/flat_minus_9",
	"whole_ext/flat_minus_9_filtered",
	"whole_ext/flat_relations",
	"whole_ext/flat_relations_filtered",
	#
	"whole_raw/full",
	"whole_raw/full_filtered",
	"whole_raw/estate_level",
	"whole_raw/estate_level_filtered",
	"whole_raw/flat_minus",
	"whole_raw/flat_minus_filtered",
	"whole_raw/flat_minus_6",
	"whole_raw/flat_minus_6_filtered",
	"whole_raw/flat_relations",
	"whole_raw/flat_relations_filtered"
)

# re-generate certain plots with fixed ranges
normalize.distance.plots(graph.types=graph.types, mode=MEAS_MODE_UNDIR, sep.legend=TRUE)

# merge previously computed whole-graph stats
merge.stats(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)

# plot decision figure
plot.stats.comparison()

# plot comparison graphs
plot.graph.comparisons(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)

# comparison of community structures
#plot.comstruct.comparison()

# exporting various data and stats for later use
#export.graphs.as.edgelists(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST)
#extract.vertex.attributes(graph.names=graph.types, folder=FOLDER_OUT_ANAL_EST, attributes=c(COL_LOC_X,COL_LOC_Y,COL_LOC_INTER_X,COL_LOC_INTER_Y,COL_LOC_INTER))




########################################################################
# end logging
end.rec.log()
