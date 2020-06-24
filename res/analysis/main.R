########################################################################
# Analyzes the social networks.
# 
# Vincent Labatut
# 06/2020
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/main.R")
########################################################################
# load other scripts
source("res/common/include.R")
source("res/analysis/extract_graphs.R")
source("res/analysis/compute_measures.R")




########################################################################
# start logging
start.rec.log(text="SOCNET")
# possibly create folder
dir.create(path=FOLDER_OUT_ANAL, showWarnings=FALSE, recursive=TRUE)




########################################################################
# load the data and create various versions of the graph
gnames <- extract.networks()




########################################################################
# compute topological measures for the extracted networks
tlog.start.loop(0,length(gnames), "Processing the measures for each extracted graph")
for(i in 1:length(gnames))
{	tlog.loop(2, i, "Processing the measures for graph ",gnames[i]," (",i,"/",length(gnames),")")
	g <- analyze.network(folder.path==file.path(FOLDER_OUT_ANAL,gnames[i]))
}
tlog.end.loop("Measure computation over")




########################################################################
# end logging
end.rec.log()
