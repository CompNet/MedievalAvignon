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
start.rec.log(text="Nets")




########################################################################
## possibly create folder
#dir.create(path=FOLDER_OUT_ANAL_SOC, showWarnings=FALSE, recursive=TRUE)
## load the data and create various versions of the social graph
#extract.social.networks()




########################################################################
## compute topological measures for the extracted networks
#tlog.start.loop(0,length(LK_TYPE_LST), "Processing the measures for each extracted social graph")
#for(i in 1:length(LK_TYPE_LST))
#{	tlog.loop(2, i, "Processing the measures for graph ",LK_TYPE_LST[i]," (",i,"/",length(LK_TYPE_LST),")")
#	
#	# compute all topological measures
#	g <- analyze.network(gname=LK_TYPE_LST[i], out.folder=FOLDER_OUT_ANAL_SOC)
#	
#	# same for the augmented graph
#	if(LK_TYPE_LST[i]==LK_TYPE_PRO || LK_TYPE_LST[i]==LK_TYPE_ALL)
#		g <- analyze.network(gname=paste0(LK_TYPE_LST[i],"_c"), out.folder=FOLDER_OUT_ANAL_SOC)
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
}
tlog.end.loop("Measure computation over")




########################################################################
# end logging
end.rec.log()
