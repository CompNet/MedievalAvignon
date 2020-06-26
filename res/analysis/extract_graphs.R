########################################################################
# Extracts the social networks based on the raw data.
# 
# Vincent Labatut
# 06/2020
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Extraction/MedievalAvignon")
# setwd("~/eclipse/workspaces/Extraction/MedievalAvignon")
# source("res/analysis/extract_graphs.R")
########################################################################




########################################################################
# Loads the raw data, extracts the different types of social networks,
# records them as graphml files, and plots them.
#
# returns: the names of the extracted networks.
########################################################################
extract.networks <- function()
{	# load the data and create various versions of the graph
	tlog(0,"Extracting various versions of the social graph")
	
	# load relationships
	tlog(2,"Loading relational information")
	data <- read.table(
		file=FILE_IN_ANAL_LINKS,
		sep=";",
		header=TRUE,
		stringsAsFactors=FALSE,
		quote=""
	)
	
	# build graph
	tlog(2,"Building graph")
	edge.list <- cbind(as.character(data[,MF_ND_SOURCE]), as.character(data[,MF_ND_TARGET]))
	g <- graph_from_edgelist(el=edge.list, directed=TRUE)
	g <- set_edge_attr(graph=g, name=LK_TYPE, value=MAP_MF2LK[data[,MF_LK_TYPE]])
	g <- set_edge_attr(graph=g, name=LK_SUBTYPE, value=data[,MF_LK_LABEL])
	
	# load personal information
	tlog(2,"Loading individual information")
	info <- read.table(
		file=FILE_IN_ANAL_NODES,
		sep=",",
		header=TRUE,
		stringsAsFactors=FALSE,
		na.strings="NULL"
	)
	# remove empty values
	for(i in 1:ncol(info))
	{	info[which(info[,i]==" "),i] <- ""
		info[which(info[,i]==""), i] <- NA
	}
	
	# complete graph with personal information
	tlog(2,"Adding to graph")
	idx <- match(V(g)$name, as.character(info[,MF_ND_ID]))
	if(length(which(is.na(idx)))>0)
		stop("Problem while matching the tables: NA values", paste(which(is.na(idx)), collapse=", "))
	atts <- setdiff(colnames(info), MF_ND_ID)
	for(att in atts)
		g <- set_vertex_attr(graph=g, name=MAP_MF2ND[att], value=info[idx,att])
	V(g)$label <- vertex_attr(g, ND_NAME_FULL)
	
	# init layout
#	layout <- layout_with_fr(g)
#	layout <- layout_with_fr(g, kkconst=0)
#	layout <- layout_nicely(g)
#	layout <- layout_with_dh(g)		# very slow
#	layout <- layout_with_gem(g)		# extremely slow
	layout <- layout_with_kk(g, kkconst=gorder(g)/4)
#	layout <- layout_with_mds(g)
	# old code used to manually refine the layout
#		tkplot(g, layout=layout)
#		layout <- tk_coords(3)
	# update graph
	V(g)$x <- layout[,1]
	V(g)$y <- layout[,2]
	
	# extract several versions
	tlog(2,"Extracting several variants of the graph")
	etypes <- c(LK_TYPE_ALL, sort(unique(E(g)$type)))
	for(i in 1:length(etypes))
	{	tlog(4,"Extracting graph \"",etypes[i],"\" (",i,"/",length(etypes),")")
		
		# keep only the targeted type of links
		if(etypes[i]==LK_TYPE_ALL)
			g1 <- g
		else
		{	g1 <- delete_edges(graph=g, edges=which(E(g)$type!=etypes[i]))
			#g1 <- delete_vertices(graph=g1, v=which(degree(g, mode="all")==0))
		}
		g1$name <- etypes[i]
		
		# init folder
		graph.folder <- file.path(FOLDER_OUT_ANAL, g1$name)
		dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
		
		# check graph validity
		if(etypes[i]!=LK_TYPE_ALL && any_multiple(graph=g1))
		{	el <- as_edgelist(graph=g1, names=FALSE)
			# loops
			idx.loop <- which(count_multiple(g1)<1)
			tlog(6,"Loops: ",length(idx.loop))
			if(length(idx.loop)>0)
			{	tab <- cbind(V(g1)$name[el[idx.loop,1]], V(g1)$name[el[idx.loop,2]], count_multiple(g1)[idx.loop])
				colnames(tab) <- c("Source","Target","Multiplicity")
				print(tab)
				tab.file <- file.path(graph.folder, "pb_loops.txt")
				write.table(tab, file=tab.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
			}
			# multiple links
			idx.mult <- which(count_multiple(g1)>1)
			tlog(6,"Multiple links: ",length(idx.mult))
			if(length(idx.mult)>0)
			{	tab <- cbind(V(g1)$name[el[idx.mult,1]], V(g1)$name[el[idx.mult,2]], count_multiple(g1)[idx.mult])
				colnames(tab) <- c("Source","Target","Multiplicity")
				print(tab)
				tab.file <- file.path(graph.folder, "pb_multiple_links.txt")
				write.table(tab, file=tab.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
			}
			# stop execution
			#stop("The graph contains multiple edges or loops")
			g1 <- simplify(graph=g1, remove.multiple=TRUE, remove.loops=TRUE, 
					edge.attr.comb=list(type="first", label=function(x) 
							{	x <- x[!is.na(x)]
								if(length(x)>0)
									res <- paste(x, collapse=";")
								else
									res <- NA
								return(res)
							}))
		}
		
		# keep the labels of only top hubs 
		bottom.nbr <- gorder(g1)-5 #round(gorder(g1)*0.95)
		bottom.idx <- order(degree(g1))
		V(g1)$label[bottom.idx[1:bottom.nbr]] <- NA
		
		# plot full graph
		plot.file <- file.path(graph.folder, "graph")
		tlog(4,"Plotting graph in \"",plot.file,"\"")
		custom.gplot(g1, file=plot.file)
		#custom.gplot(g1)
		
		# record graph as a graphml file
		graph.file <- file.path(graph.folder, FILE_GRAPH)
		write.graph(graph=g1, file=graph.file, format="graphml")
	}

	return(etypes)	
}
