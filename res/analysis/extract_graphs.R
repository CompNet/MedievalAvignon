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




#############################################################################################
# Returns the names of the specified persons, using (by priority order): normalized full name, 
# original full name, first+last name, nick+last name, lastname, nickname, first name.
#
# g: considered graph.
# vs: ids of the vertices (default: all of them).
#
# returns: vector of strings corresponding to the vertex names.
#############################################################################################
get.person.names <- function(g, vs=1:gorder(g))
{	# init with normalized full name
	result <- vertex_attr(graph=g, name=COL_PERS_NAME_FULL_NORM, index=vs)
	
	# complete with original full name
	idx <- which(is.na(result))
	if(length(idx)>0)
		result[idx] <- vertex_attr(graph=g, name=COL_PERS_NAME_FULL_LAT, index=vs[idx])
	
	# complete with first+last name
	idx <- which(is.na(result))
	if(length(idx)>0)
	{	fn <- vertex_attr(graph=g, name=COL_PERS_NAME_FIRST, index=vs[idx])
		ln <- vertex_attr(graph=g, name=COL_PERS_NAME_LAST, index=vs[idx])
		idx2 <- which(!is.na(fn) & !is.na(ln))
		result[idx[idx2]] <- paste(fn[idx2], ln[idx2])
	}
	
	# complete with nick+last name
	idx <- which(is.na(result))
	if(length(idx)>0)
	{	nn <- vertex_attr(graph=g, name=COL_PERS_NAME_NICK, index=vs[idx])
		ln <- vertex_attr(graph=g, name=COL_PERS_NAME_LAST, index=vs[idx])
		idx2 <- which(!is.na(nn) & !is.na(ln))
		result[idx[idx2]] <- paste(nn[idx2], ln[idx2])
	}
	
	# complete with last name
	idx <- which(is.na(result))
	if(length(idx)>0)
		result[idx] <- vertex_attr(graph=g, name=COL_PERS_NAME_LAST, index=vs[idx])
	
	# complete with nick name
	idx <- which(is.na(result))
	if(length(idx)>0)
		result[idx] <- vertex_attr(graph=g, name=COL_PERS_NAME_NICK, index=vs[idx])
	
	# complete with first name
	idx <- which(is.na(result))
	if(length(idx)>0)
		result[idx] <- vertex_attr(graph=g, name=COL_PERS_NAME_FIRST, index=vs[idx])
	
	return(result)
}




########################################################################
# Loads the raw data, extracts the different types of social networks,
# records them as graphml files, and plots them.
########################################################################
extract.social.networks <- function()
{	# load the data and create various versions of the graph
	tlog(0,"Extracting various versions of the social graph")
	
	# load relationships
	tlog(2,"Loading relational information")
	data <- read.table(
		file=FILE_IN_ANAL_SOCIAL_LINKS,
		sep=",",
		header=TRUE,
		stringsAsFactors=FALSE,
		quote='"',
		check.names=FALSE
	)
	tlog(4,"Found ",nrow(data)," relations")
	
	# build graph
	tlog(2,"Building graph")
	edge.list <- cbind(as.character(data[,COL_SOC_SRC]), as.character(data[,COL_SOC_TGT]))
	g <- graph_from_edgelist(el=edge.list, directed=TRUE)
	g <- set_edge_attr(graph=g, name=LK_TYPE, value=MAP_TABLE2GRAPH[data[,COL_SOC_TYPE]])
	g <- set_edge_attr(graph=g, name=LK_DESCR, value=data[,COL_SOC_DESCR])
	tlog(4,"Number of edges: ",gsize(g),"/",nrow(data))
	tlog(4,"Edge attributes: ",paste(edge_attr_names(g),collapse=", "))
	
	# load personal information
	tlog(2,"Loading individual information")
	info <- read.table(
		file=FILE_IN_ANAL_PERSON_NODES,
		sep=",",
		header=TRUE,
		stringsAsFactors=FALSE,
		na.strings="NULL",
		quote='"',
		check.names=FALSE
	)
	tlog(4,"Found ",nrow(info)," persons")
	# remove empty values
	for(i in 1:ncol(info))
	{	info[which(info[,i]==" "),i] <- ""
		info[which(info[,i]==""), i] <- NA
	}
	
	# complete graph with personal information
	tlog(2,"Adding to graph")
	idx <- match(V(g)$name, as.character(info[,COL_PERS_ID]))
	if(length(which(is.na(idx)))>0)
		stop("Problem while matching the tables: NA values", paste(which(is.na(idx)), collapse=", "))
	atts <- colnames(info)
	for(i in 1:length(atts))
	{	att <- atts[i]
		tlog(4,"Processing attribute ",att," (",i,"/",length(atts),")")
		g <- set_vertex_attr(graph=g, name=att, value=info[idx,att])
	}
	tlog(4,"Number of nodes: ",gorder(g),"/",nrow(info))
	tlog(4,"Vertex attributes: ",paste(vertex_attr_names(g),collapse=", "))
	
	# add composite name as label
	comp.names <- get.person.names(g)
	V(g)$label <- comp.names
	
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
	layoutC <- matrix(nrow=0,ncol=2)
	
	# extract several versions
	tlog(2,"Extracting several variants of the graph")
	for(i in 1:length(LK_TYPE_LST))
	{	tlog(4,"Extracting graph \"",LK_TYPE_LST[i],"\" (",i,"/",length(LK_TYPE_LST),")")
		
		# keep only the targeted type of links
		if(LK_TYPE_LST[i]==LK_TYPE_ALL)
			g1 <- g
		else
		{	g1 <- delete_edges(graph=g, edges=which(E(g)$type!=LK_TYPE_LST[i]))
			#g1 <- delete_vertices(graph=g1, v=which(degree(g, mode="all")==0))
		}
		g1$name <- LK_TYPE_LST[i]
		
		# init folder
		graph.folder <- file.path(FOLDER_OUT_ANAL, g1$name)
		dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
		
		# check graph validity
		if(LK_TYPE_LST[i]!=LK_TYPE_ALL && any_multiple(graph=g1))
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
		g1 <- update.node.labels(g1, vals=degree(g1))
		
		# plot full graph
		plot.file <- file.path(graph.folder, "graph")
		tlog(4,"Plotting graph in \"",plot.file,"\"")
		custom.gplot(g1, file=plot.file)
		#custom.gplot(g1)
		
		# record graph as a graphml file
		graph.file <- file.path(graph.folder, FILE_GRAPH)
		tlog(4,"Recording graph in \"",graph.file,"\"")
		write.graphml.file(g=g1, file=graph.file)
		
		# if professional links: add the courtier relations to the pope
		if(LK_TYPE_LST[i]==LK_TYPE_PRO || LK_TYPE_LST[i]==LK_TYPE_ALL)
		{	tlog(4,"Adding extra links between the pope and its courtiers")
			
			# get the ids of all courtiers
			courtiers <- c("citoyen-courtisan", "courtisan", "familier")
			idx <- which(!is.na(match(info[,COL_PERS_STATUS_NORM],courtiers)))
			tlog(6,"Found ",length(idx)," courtiers")
			
			# add the missing ones to the graph
			missing.ids <- setdiff(info[idx,COL_PERS_ID], vertex_attr(g1,COL_PERS_ID))
			if(length(missing.ids)>0)
			{	tlog(6,"Among them, ",length(missing.ids)," are not present in the graph yet >> adding them")
				g1 <- add_vertices(graph=g1, nv=length(missing.ids))
				idx2 <- match(missing.ids, info[,COL_PERS_ID])
				atts <- colnames(info)
				for(j in 1:length(atts))
				{	att <- atts[j]
					g1 <- set_vertex_attr(graph=g1, name=att, index=(gorder(g1)-length(idx2)+1):gorder(g1), value=info[idx2,att])
				}
			}
			
			# processing each courtier
			pope <- which(vertex_attr(g1,COL_PERS_ID)==1080)
			tlog(6,"Adding the missing links")
			for(j in 1:length(idx))
			{	#tlog(8,"Courtier id=",info[idx[j],COL_PERS_ID]," (",j,"/",length(idx),")")
				nn <- which(vertex_attr(g1,COL_PERS_ID)==info[idx[j],COL_PERS_ID])
				edges <- get.edge.ids(graph=g1, vp=c(nn,pope), directed=TRUE, multi=TRUE)
				if(length(edges)==1 && edges==0)	# no edge found
					edges <- c()
				#tlog(10,"Found ",length(edges)," existing edge(s) between the pope and this courtier")
				if(length(edges)>0)
					edges <- which(edge_attr(g1, LK_TYPE, edges)==LK_TYPE_PRO)
				if(length(edges)==0)
				{	#tlog(10,"No existing professional link >> adding one")
					g1 <- add_edges(g1, c(nn, pope), attr=list(type=LK_TYPE_PRO))
				}
				#else
				#	tlog(10,"Among them, there is already a professional link, so no link creation here")
			}
			
			# display stats
			tlog(6,"Number of nodes: ",gorder(g1)," (before: ",gorder(g),")")
			tlog(6,"Number of links: ",gsize(g1), " (before: ",gsize(g),")")
			
			# possibly setup layout
			if(nrow(layoutC)==0)
			{	# get coordinates
				coords <- cbind(V(g1)$x, V(g1)$y)
				coords[is.na(coords)] <- 0
				# update layout
				layoutC <- layout_with_kk(
					g1,
#					coords=coords,
					kkconst=gorder(g)/4)
			}
			# update graph
			V(g1)$x <- layoutC[,1]
			V(g1)$y <- layoutC[,2]
			
			# init folder
			g1$name <- paste0(LK_TYPE_LST[i],"_c")
			graph.folder <- file.path(FOLDER_OUT_ANAL, g1$name)
			dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
			
			# keep the labels of only top hubs 
			g1 <- update.node.labels(g1, vals=degree(g1))
			
			# plot full graph
			plot.file <- file.path(graph.folder, "graph")
			tlog(6,"Plotting graph in \"",plot.file,"\"")
			custom.gplot(g1, file=plot.file)
			#custom.gplot(g1)
			
			# record graph as a graphml file
			graph.file <- file.path(graph.folder, FILE_GRAPH)
			tlog(6,"Recording graph in \"",graph.file,"\"")
			write.graphml.file(g=g1, file=graph.file)
		}
	}
}

# TODO extract directly the network with all 8000 nodes?
