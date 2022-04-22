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
# link.types <- c(LV_ESTATE, LK_TYPE_FLATREL)




########################################################################
# compute topological measures for the extracted networks
tlog.start.loop(0,length(link.types), "Processing the measures for each extracted estate graph")
for(i in 1:length(link.types))
{	tlog.loop(2, i, "Processing the measures for graph ",link.types[i]," (",i,"/",length(link.types),")")
	
	# gname=LV_ESTATE; out.folder=FOLDER_OUT_ANAL_EST
	
	# compute all topological measures
	g <- analyze.network(gname=link.types[i], out.folder=FOLDER_OUT_ANAL_EST)
	
	# filtered version
	g <- analyze.network(gname=paste0(link.types[i],"_filtered"), out.folder=FOLDER_OUT_ANAL_EST)
}
plot.comstruct.comparison()
tlog.end.loop("Measure computation over")




########################################################################
# end logging
end.rec.log()
