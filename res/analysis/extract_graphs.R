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




#############################################################################################
# Function used by get.location.names to update only the names still NA.
#
# g: considered graph.
# vs: ids of the vertices (default: all of them).
# att.name: attribute name to consider.
# result: current names.
#
# returns: updated names.
#############################################################################################
complete.names <- function(g, vs=1:gorder(g), att.name, result)
{	#print(att.name)
	idx <- which(is.na(result))
	if(length(idx)>0)
		result[idx] <- vertex_attr(graph=g, name=att.name, index=vs[idx])
	
	return(result)
}




#############################################################################################
# Returns the names of the specified estates, using (by priority order): normalized qualification,
# latin qualification, translated type, type.
#
# g: considered graph.
# vs: ids of the vertices (default: all of them).
#
# returns: vector of strings corresponding to the vertex names.
#############################################################################################
get.location.names <- function(g, vs=1:gorder(g))
{	result <- rep(NA, length(vs))
	
	att.names <- intersect(
		c(
			# current name
			COL_STREET_NAME_CURR,
			# translated name
			COL_GATE_NAME_FRE, COL_AREA_NAME_FRE, COL_WALL_NAME_FRE, 
			# name
#			COL_FIX_NAME, 
			COL_EDIF_NAME, COL_VILG_NAME, COL_CARD_NAME, COL_LDMRK_NAME, COL_STREET_NAME, 
			# latin name
			COL_GATE_NAME_LAT, COL_AREA_NAME_LAT, COL_WALL_NAME_LAT, 
			
			# normalized qualification
			COL_EST_QUALIF_NORM,
			# latin qualification
			COL_EST_QUALIF_LAT,
			
			# translated type
			COL_EST_TYPE_FRE,
			# type
#			COL_FIX_TYPE, 
			COL_EDIF_TYPE, COL_VILG_TYPE, COL_CARD_TYPE, COL_GATE_TYPE, COL_WALL_TYPE, COL_LDMRK_TYPE, COL_STREET_TYPE, 
			# latin type
			COL_EST_TYPE_LAT,
			
			# detail
			COL_EST_DETAIL,
			
			# id
			COL_EST_ID, #COL_FIX_ID, 
			COL_EDIF_ID, COL_VILG_ID, COL_CARD_ID, COL_GATE_ID, COL_AREA_ID, COL_WALL_ID, COL_LDMRK_ID, COL_STREET_ID
		), 
		vertex_attr_names(g))
	
	# complete with various graph attributes
	for(att.name in att.names)
	{	#print(att.name)
		result <- complete.names(g=g, vs=vs, att.name=att.name, result=result)
		#print(result)
	}
	
	return(result)
}




#############################################################################################
# Returns the names of the specified nodes, using either get.person.names or get.location.names.
#
# g: considered graph.
# vs: ids of the vertices (default: all of them).
#
# returns: vector of strings corresponding to the vertex names.
#############################################################################################
get.names <- function(g, vs=1:gorder(g))
{	link.types <- edge_attr(g, LK_TYPE)
	
	if(graph_attr(g, GR_TYPE)==GR_TYPE_SOC)
		result <- get.person.names(g, vs)
	else
		result <- get.location.names(g, vs)
	
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
	g <- set_graph_attr(graph=g, name=GR_TYPE, value=GR_TYPE_SOC)
	g <- set_edge_attr(graph=g, name=LK_TYPE, value=MAP_TABLE2GRAPH[data[,COL_SOC_TYPE]])
	g <- set_edge_attr(graph=g, name=LK_DESCR, value=data[,COL_SOC_DESCR])
	tlog(4,"Number of edges: ",gsize(g),"/",nrow(data))
	tlog(4,"Edge attributes: ",paste(edge_attr_names(g),collapse=", "))
	
	# load individual information
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
	
	# complete graph with individual information
	tlog(2,"Adding to graph")
	idx <- match(V(g)$name, as.character(info[,COL_PERS_ID]))
	if(length(which(is.na(idx)))>0)
		stop("Problem while matching the tables: NA values ", paste(which(is.na(idx)), collapse=", "))
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
	for(i in 1:length(LK_TYPE_SOC_LST))
	{	tlog(4,"Extracting graph \"",LK_TYPE_SOC_LST[i],"\" (",i,"/",length(LK_TYPE_SOC_LST),")")
		
		# keep only the targeted type of links
		if(LK_TYPE_SOC_LST[i]==LK_TYPE_ALL)
			g1 <- g
		else
		{	g1 <- delete_edges(graph=g, edges=which(E(g)$type!=LK_TYPE_SOC_LST[i]))
			#g1 <- delete_vertices(graph=g1, v=which(degree(g, mode="all")==0))
		}
		g1$name <- LK_TYPE_SOC_LST[i]
		
		# init folder
		graph.folder <- file.path(FOLDER_OUT_ANAL_SOC, g1$name)
		dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
		
		# check graph validity
		if(LK_TYPE_SOC_LST[i]!=LK_TYPE_ALL && any_multiple(graph=g1))
		{	el <- as_edgelist(graph=g1, names=FALSE)
			# loops
			idx.loop <- which(count_multiple(g1)<1)
			tlog(6,"Loops: ",length(idx.loop))
			if(length(idx.loop)>0)
			{	tab <- cbind(V(g1)$name[el[idx.loop,1]], V(g1)$name[el[idx.loop,2]], count_multiple(g1)[idx.loop], 
						sapply(1:length(idx.loop), 
							function(j) edge_attr(g1, LK_DESCR, E(g1)[el[idx.loop[j],1] %->% el[idx.loop[j],2]])))
				colnames(tab) <- c("Source","Target","Multiplicity","Description")
				print(tab)
				tab.file <- file.path(graph.folder, "pb_loops.txt")
				write.table(tab, file=tab.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
			}
			# multiple links
			idx.mult <- which(count_multiple(g1)>1)
			tlog(6,"Multiple links: ",length(idx.mult))
			if(length(idx.mult)>0)
			{	tab <- matrix(nrow=0, ncol=4)
				colnames(tab) <- c("Source","Target","Multiplicity","Description")
				for(j in 1:length(idx.mult))
				{	lids <- E(g1)[el[idx.mult[j],1] %->% el[idx.mult[j],2]]
					descr <- edge_attr(g1,LK_DESCR, E(g1)[lids])
					if(length(descr)>length(unique(descr)))
					{	row <- c(V(g1)$name[el[idx.mult[j],1]], V(g1)$name[el[idx.mult[j],2]], 
								count_multiple(g1)[idx.mult[j]], paste(descr, collapse=":"))
						tab <- rbind(tab, row)
					}
				}
				if(nrow(tab)>0)
				{	print(tab)
					tab.file <- file.path(graph.folder, "pb_multiple_links.txt")
					write.table(tab, file=tab.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
				}
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
		custom.gplot(g=g1, file=plot.file)
		#custom.gplot(g=g1)
		
		# record graph as a graphml file
		graph.file <- file.path(graph.folder, FILE_GRAPH)
		tlog(4,"Recording graph in \"",graph.file,"\"")
		write.graphml.file(g=g1, file=graph.file)
		
		# if professional links: add the courtier relations to the pope
		if(LK_TYPE_SOC_LST[i]==LK_TYPE_PRO || LK_TYPE_SOC_LST[i]==LK_TYPE_ALL)
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
			g1$name <- paste0(LK_TYPE_SOC_LST[i],"_c")
			graph.folder <- file.path(FOLDER_OUT_ANAL_SOC, g1$name)
			dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
			
			# keep the labels of only top hubs 
			g1 <- update.node.labels(g1, vals=degree(g1))
			
			# plot full graph
			plot.file <- file.path(graph.folder, "graph")
			tlog(6,"Plotting graph in \"",plot.file,"\"")
			custom.gplot(g=g1, file=plot.file)
			#custom.gplot(g=g1)
			
			# record graph as a graphml file
			graph.file <- file.path(graph.folder, FILE_GRAPH)
			tlog(6,"Recording graph in \"",graph.file,"\"")
			write.graphml.file(g=g1, file=graph.file)
		}
	}
}

# TODO extract directly the network with all 8000 nodes?




########################################################################
# Loads and cleans the specified table describing various types of
# locations.
#
# tab.file: name of the file to load.
# type: location type.
#
# returns: corresponding table.
########################################################################
load.location.table <- function(tab.file, type, last)
{	# load table
	tlog(2,"Loading ",type," information")
	tab <- read.table(
			file=tab.file,
			sep=",",
			header=TRUE,
			stringsAsFactors=FALSE,
			na.strings="NULL",
			quote='"',
			check.names=FALSE
	)
	tlog(4,"Found ",nrow(tab)," ",type,"(s)")
	
	# remove empty values
	for(i in 1:ncol(tab))
	{	tab[which(tab[,i]==" "),i] <- ""
		tab[which(tab[,i]==""), i] <- NA
	}
	
	return(tab)
}




########################################################################
# Loads the raw data, extracts the different types of estate networks,
# records them as graphml files, and plots them.
#
# returns: vector of all the link types.
########################################################################
extract.estate.networks <- function()
{	# load the data and create various versions of the graph
	tlog(0,"Extracting various versions of the estate graph")
	
	# load estate information
	tlog(2,"Loading estate information")
	info.estate <- load.location.table(FILE_IN_ANAL_ESTATE_NODES,"estate")
info.estate <- info.estate[,-which(colnames(info.estate) %in% c(COL_EST_AREA_ID, COL_EST_STREET_ID, COL_EST_VILLAGE_ID))]
	info.estate <- cbind(paste("Bien:",info.estate[,COL_EST_ID],sep=""),info.estate); colnames(info.estate)[1] <- COL_LOC_ID
	cols <- colnames(info.estate)
	total.nbr <- nrow(info.estate)
	# load area information
	info.area <- load.location.table(FILE_IN_ANAL_AREA_NODES,"area", last=)
	info.area <- cbind(paste("Quartier:",info.area[,COL_AREA_ID],sep=""),info.area); colnames(info.area)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.area))
	total.nbr <- total.nbr + nrow(info.area)
	# load fix information
#	info.fix <- load.location.table(FILE_IN_ANAL_FIX_NODES,"fix")
#	info.fix <- cbind(paste("In",info.fix[,COL_FIX_ID],sep=""),info.fix); colnames(info.fix)[1] <- COL_LOC_ID
#	cols <- c(cols, colnames(info.fix))
#	total.nbr <- total.nbr + nrow(info.fix)
	info.village <- load.location.table(FILE_IN_ANAL_VILG_NODES,"village")
	info.village <- cbind(paste("Bourg:",info.village[,COL_VILG_ID],sep=""),info.village); colnames(info.village)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.village))
	total.nbr <- total.nbr + nrow(info.village)
	info.edifice <- load.location.table(FILE_IN_ANAL_EDIFICE_NODES,"edifice")
	info.edifice <- cbind(paste("Edifice:",info.edifice[,COL_EDIF_ID],sep=""),info.edifice); colnames(info.edifice)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.edifice))
	total.nbr <- total.nbr + nrow(info.edifice)
	info.cardinal <- load.location.table(FILE_IN_ANAL_CARD_NODES,"cardinal")
	info.cardinal <- cbind(paste("Livree:",info.cardinal[,COL_CARD_ID],sep=""),info.cardinal); colnames(info.cardinal)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.cardinal))
	total.nbr <- total.nbr + nrow(info.cardinal)
	info.gate <- load.location.table(FILE_IN_ANAL_GATE_NODES,"gate")
	info.gate <- cbind(paste("Porte:",info.gate[,COL_GATE_ID],sep=""),info.gate); colnames(info.gate)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.gate))
	total.nbr <- total.nbr + nrow(info.gate)
	info.wall <- load.location.table(FILE_IN_ANAL_WALL_NODES,"wall")
	info.wall <- cbind(paste("Rempart:",info.wall[,COL_WALL_ID],sep=""),info.wall); colnames(info.wall)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.wall))
	total.nbr <- total.nbr + nrow(info.wall)
	info.landmark <- load.location.table(FILE_IN_ANAL_LDMRK_NODES,"landmark")
	info.landmark <- cbind(paste("Repere:",info.landmark[,COL_LDMRK_ID],sep=""),info.landmark); colnames(info.landmark)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.landmark))
	total.nbr <- total.nbr + nrow(info.landmark)
	info.street <- load.location.table(FILE_IN_ANAL_STREET_NODES,"street")
	info.street <- cbind(paste("Rue:",info.street[,COL_STREET_ID],sep=""),info.street); colnames(info.street)[1] <- COL_LOC_ID
	cols <- c(cols, colnames(info.street))
	total.nbr <- total.nbr + nrow(info.street)
	
	# merge these tables
	tlog(4,"Merging estate information tables")
	cols <- unique(cols)
	info.all <- data.frame(matrix(NA, nrow=total.nbr, ncol=length(cols), dimnames=list(c(), cols)),stringsAsFactors=F)
	last <- 0
	# add estate info
	com.cols <- intersect(cols,colnames(info.estate))
	info.all[(last+1):(last+nrow(info.estate)),com.cols] <- info.estate[,com.cols]
	last <- last + nrow(info.estate)
	# add area info
	com.cols <- intersect(cols,colnames(info.area))
	info.all[(last+1):(last+nrow(info.area)),com.cols] <- info.area[,com.cols]
	last <- last + nrow(info.area)
	# add fix info
#	com.cols <- intersect(cols,colnames(info.fix))
#	info.all[(last+1):(last+nrow(info.fix)),com.cols] <- info.fix[,com.cols]
#	last <- last + nrow(info.fix)
	com.cols <- intersect(cols,colnames(info.village))
	info.all[(last+1):(last+nrow(info.village)),com.cols] <- info.village[,com.cols]
	last <- last + nrow(info.village)
	com.cols <- intersect(cols,colnames(info.edifice))
	info.all[(last+1):(last+nrow(info.edifice)),com.cols] <- info.edifice[,com.cols]
	last <- last + nrow(info.edifice)
	com.cols <- intersect(cols,colnames(info.cardinal))
	info.all[(last+1):(last+nrow(info.cardinal)),com.cols] <- info.cardinal[,com.cols]
	last <- last + nrow(info.cardinal)
	com.cols <- intersect(cols,colnames(info.gate))
	info.all[(last+1):(last+nrow(info.gate)),com.cols] <- info.gate[,com.cols]
	last <- last + nrow(info.gate)
	com.cols <- intersect(cols,colnames(info.wall))
	info.all[(last+1):(last+nrow(info.wall)),com.cols] <- info.wall[,com.cols]
	last <- last + nrow(info.wall)
	com.cols <- intersect(cols,colnames(info.landmark))
	info.all[(last+1):(last+nrow(info.landmark)),com.cols] <- info.landmark[,com.cols]
	last <- last + nrow(info.landmark)
	com.cols <- intersect(cols,colnames(info.street))
	info.all[(last+1):(last+nrow(info.street)),com.cols] <- info.street[,com.cols]
	last <- last + nrow(info.street)
	
	# remove empty columns
	empty.cols <- which(apply(info.all, 2, function(col) all(is.na(col))))
	tlog(4,"Found ",length(empty.cols)," all-NA columns: removing them")
	info.all <- info.all[,-empty.cols]
	
	# load relationships
	tlog(2,"Loading relational information")
	data <- read.table(
		file=FILE_IN_ANAL_CONFR_LINKS,
		sep=",",
		header=TRUE,
		stringsAsFactors=FALSE,
		na.strings="NULL",
		quote='"',
		check.names=FALSE
	)
	tlog(4,"Found ",nrow(data)," relations")
	
	# collapse the ids from 3 to 2 columns and convert them to internal ids
	edge.list <- t(sapply(1:nrow(data), function(r)
	{	print(r)
		# get the source id
		if(is.na(data[r,COL_CONF_EST1_ID]))
			stop(paste0("ERROR: found no source id in row #",r))
		src.id <- which(info.all[,COL_EST_ID]==data[r,COL_CONF_EST1_ID])
		if(is.na(src.id))
			stop(paste0("ERROR: could not find an estate matching source id in row #",r))
		
		# get the second estate id
		tgt.id <- c()
		if(!is.na(data[r,COL_CONF_EST2_ID]))
			tgt.id <- c(tgt.id, which(info.all[,COL_EST_ID]==data[r,COL_CONF_EST2_ID]))
		if(!is.na(data[r,COL_CONF_FIX_ID]))
			tgt.id <- c(tgt.id, which(
#								info.all[,COL_FIX_ID]==data[r,COL_CONF_FIX_ID]))
								info.all[,COL_VILG_ID]==data[r,COL_CONF_FIX_ID]
								| info.all[,COL_EDIF_ID]==data[r,COL_CONF_FIX_ID]
								| info.all[,COL_CARD_ID]==data[r,COL_CONF_FIX_ID]
								| info.all[,COL_GATE_ID]==data[r,COL_CONF_FIX_ID]
								| info.all[,COL_WALL_ID]==data[r,COL_CONF_FIX_ID]
								| info.all[,COL_LDMRK_ID]==data[r,COL_CONF_FIX_ID]
								| info.all[,COL_STREET_ID]==data[r,COL_CONF_FIX_ID]
							))
		if(!is.na(data[r,COL_CONF_AREA_ID]))
			tgt.id <- c(tgt.id, which(info.all[,COL_AREA_ID]==data[r,COL_CONF_AREA_ID]))		
		
		if(length(tgt.id)==0)
			stop(paste0("ERROR: found no destinations id in row #",r))
		if(length(tgt.id)>1)
			stop(paste0("ERROR: found several destinations ids in row #",r))
		if(is.na(tgt.id))
			stop(paste0("ERROR: could not match the destination id in row #",r))
		
		src.ext.id <- info.all[src.id, COL_LOC_ID]
		tgt.ext.id <- info.all[tgt.id, COL_LOC_ID]
		result <- as.character(c(src.ext.id, tgt.ext.id))
		return(result)
	}))
	
	
	# manual corrections and simplifications
	# VAL_CONF_TYPE_EGALE
	idx <- which(data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_EGALE)
	if(length(idx>0))
	{	for(r in 1:length(idx))
		{	old.id <- edge.list[r,1]
			new.id <- edge.list[r,2]
			edge.list[edge.list[,1]==old.id,1] <- new.id
			edge.list[edge.list[,2]==old.id,2] <- new.id
		}
		data <- data[-idx,]
		edge.list <- edge.list[-idx,]
	}
	# keep relations targetting parts of streets
	street.angles <- which(data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_ANGLE & startsWith(edge.list[,2], "Rue:"))
	street.starts <- which(data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DEBUT & startsWith(edge.list[,2], "Rue:"))
	# VAL_CONF_TYPE_COTE
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_COTE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_ANGLE
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_ANGLE & !startsWith(edge.list[,2], "Livree:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_ANGLE & startsWith(edge.list[,2], "Livree:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_INTERIEUR
	# VAL_CONF_TYPE_ENTREE
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_ENTREE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_DEBUT
	# VAL_CONF_TYPE_INTERIEUR
	# VAL_CONF_TYPE_OPPOSE
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_OPPOSE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_DEBUT
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DEBUT & startsWith(edge.list[,2], "Rue:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DEBUT & startsWith(edge.list[,2], "Livree:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_INTERIEUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DEBUT & startsWith(edge.list[,2], "Bourg:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_INTERIEUR
	# VAL_CONF_TYPE_MILIEU
	#data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_MILIEU, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_EST
	# VAL_CONF_TYPE_OUEST
	# VAL_CONF_TYPE_NORD
	# VAL_CONF_TYPE_SUD
	# VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_MULT2
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_MULT2, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_MULT3
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_MULT3, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_ARRIERE
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_ARRIERE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_AVANT
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_AVANT, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_INFERIEUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_INFERIEUR, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_LATERAL
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_LATERAL, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_POSTERIEUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_POSTERIEUR, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_POSSIBLE
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_POSSIBLE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_CONTIGU
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_CONTIGU, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_DANS
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Bien:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Edifice:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Rempart:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Repere:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Rue:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Bourg:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_INTERIEUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Livree:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_INTERIEUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DANS & startsWith(edge.list[,2], "Quartier:"), COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_INTERIEUR
	# VAL_CONF_TYPE_DERRIERE
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DERRIERE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_DEVANT
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DEVANT, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_ENTRE
	#data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_ENTRE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_EXTERIEUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_EXTERIEUR, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_SOUS
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_SOUS, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_SUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_SUR, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_VERS
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_VERS, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	
	# sort(unique(data[,COL_CONF_LOC_NORM]))
	# ii <- which(E(g)$type==VAL_CONF_TYPE_EGALE); cbind(get.edgelist(g)[ii,], E(g)$type[ii])
	# ii <- match(sort(unique(data[,COL_CONF_LOC_NORM])), data[,COL_CONF_LOC_NORM]); data[ii,c(COL_CONF_LOC_LAT,COL_CONF_LOC_NORM)]
	
	# build graph
	tlog(2,"Building graph")
	link.type.attr <- COL_CONF_LOC_NORM	# COL_CONF_LOC_LAT
	alt.link.type.attr <- COL_CONF_LOC_LAT	# COL_CONF_LOC_NORM 
	g <- graph_from_edgelist(el=edge.list, directed=TRUE)
	g <- set_graph_attr(graph=g, name=GR_TYPE, value=GR_TYPE_EST)
	g <- set_edge_attr(graph=g, name=LK_TYPE, value=data[,link.type.attr])
	g <- set_edge_attr(graph=g, name=COL_CONF_AREA_ID, value=data[,COL_CONF_AREA_ID])
	g <- set_edge_attr(graph=g, name=alt.link.type.attr, value=data[,alt.link.type.attr])
	tlog(4,"Number of edges: ",gsize(g),"/",nrow(data))
	tlog(4,"Edge attributes (",length(edge_attr_names(g)),"): ",paste(edge_attr_names(g),collapse=", "))
	link.types <- sort(unique(data[,link.type.attr]))
	tlog(4,"Link types (",length(link.types),"): ",paste(link.types,collapse=", "))
	
	# complete graph with individual information
	tlog(2,"Adding to graph")
	idx <- match(V(g)$name, info.all[,COL_LOC_ID])
	atts <- colnames(info.all)
	for(i in 1:length(atts))
	{	att <- atts[i]
		tlog(4,"Processing attribute ",att," (",i,"/",length(atts),")")
		g <- set_vertex_attr(graph=g, name=att, value=info.all[idx,att])
	}
	tlog(4,"Number of nodes: ",gorder(g),"/",nrow(info.all))
	tlog(4,"Vertex attributes (",length(vertex_attr_names(g)),"): ",paste(vertex_attr_names(g),collapse=", "))
	
	# add composite name as label
	comp.names <- get.location.names(g)
	#print(which(is.na(comp.names)))
	V(g)$label <- comp.names
	
#	# init layout
##	layout <- layout_with_fr(g)
##	layout <- layout_with_fr(g, kkconst=0)
##	layout <- layout_nicely(g)
##	layout <- layout_with_dh(g)		# very slow
##	layout <- layout_with_gem(g)		# extremely slow
	layout <- layout_with_kk(g, kkconst=gorder(g)/16)
##	layout <- layout_with_mds(g)
##	layout <- layout_with_lgl(g)
#	layout <- layout_with_graphopt(g, charge=0.01, spring.length=3)
#	# old code used to manually refine the layout
##		tkplot(g, layout=layout)
##		layout <- tk_coords(3)
	# update graph
	V(g)$x2 <- layout[,1]
	V(g)$y2 <- layout[,2]
#	#V(g)$label <- NA
#	#custom.gplot(=gg)
	
	# use spatial coordinates for layout
	tlog(2,"Using spatial coordinates to define layout")
	V(g)$x <- vertex_attr(g, name=COL_LOC_X)
	V(g)$y <- vertex_attr(g, name=COL_LOC_Y)
	# missing coordinates: use the average of the neighbors
	changed <- TRUE
	while(changed)
	{	changed <- FALSE
		idx <- which(is.na(V(g)$x))
		tlog(4,"Nodes without position: ",length(idx),"/",gorder(g))
		if(length(idx)>0)
		{	neighs <- ego(graph=g, order=1, nodes=idx, mode="all", mindist=1)
			rx <- range(V(g)$x, na.rm=TRUE)
			gapx <- 2*(rx[2]-rx[1])/100
			ry <- range(V(g)$y, na.rm=TRUE)
			gapy <- 2*(ry[2]-ry[1])/100
			tmp <- sapply(1:length(idx), function(v)
			{	ns <- as.integer(neighs[[v]])
				vals.x <- V(g)$x[ns]
				vals.y <- V(g)$y[ns]
				vals.x <- vals.x[!is.na(vals.x)]
				vals.y <- vals.y[!is.na(vals.y)]
				if(length(vals.x)>1)
				{	res <- c(mean(vals.x), mean(vals.y))
					changed <<- TRUE
				}
				else if(length(vals.x)==1)
				{	res <- c(vals.x+runif(1,0,gapx), vals.y+runif(1,0,gapy))
					changed <<- TRUE
				}
				else
				{	#res <- c(runif(1,rx[1],rx[2]), runif(1,ry[1],ry[2]))
					res <- c(NA, NA)
				}
				return(res)
			})
			V(g)$x[idx] <- tmp[1,]
			V(g)$y[idx] <- tmp[2,]
		}
	}
	##V(g)$x[which(is.na(V(g)$x))] <- min(V(g)$x, na.rm=TRUE)
	##V(g)$y[which(is.na(V(g)$y))] <- min(V(g)$y, na.rm=TRUE)
	#V(g)$label <- NA
	#custom.gplot(g=g)
	#custom.gplot(g=g, col.att="x", cat.att=FALSE, color.isolates=TRUE)
	#custom.gplot(g=g, col.att=COL_LOC_X, cat.att=FALSE, color.isolates=TRUE, file="temp.png")
	
	# get additional info on the streets and other stuff
	short.tab <- read.table(
		file=FILE_IN_ANAL_STRT_SHORT,
		sep=",",
		header=TRUE,
		stringsAsFactors=FALSE,
		na.strings="NULL",
		quote='"',
		check.names=FALSE
	)
	short.street.flag <- vertex_attr(graph=g, name=COL_LOC_ID) %in% paste("Rue:",short.tab[,COL_STREET_ID],sep="")
	
	# extract one graph for each type of relation
	tlog(2,"Extracting several variants of the graph")
	link.types <- LV_ESTATE # c(LK_TYPE_ALL, LV_ESTATE, link.types)
	for(i in 1:length(link.types))
	{	tlog(4,"Extracting graph \"",link.types[i],"\" (",i,"/",length(link.types),")")
		
		# keep only the targeted type of links
		if(link.types[i]==LK_TYPE_ALL)
			g1 <- g
		else if(link.types[i]==LV_ESTATE)
		{	g1 <- g
			# change the name of certain streets whose only a part is targeted in the confronts
			streets.all <- union(street.angles, street.starts)
			strts <- sort(unique(edge.list[streets.all,2]))						# concerned parts of streets
			tlog(6,"Detected ",length(streets.all)," confronts with ",length(strts), " parts of streets")
			streets.all.flag <- V(g1)$name %in% strts							# mark them for later (to not remove them)
			rem.idx <- setdiff(which(edge.list[,2] %in% strts), streets.all)	# relations with these streets (but as complete streets)
			g1 <- delete_edges(g1, edges=rem.idx)								# remove these links
			V(g1)$name[match(strts,V(g1)$name)] <- 								# change the name of the remaining streets
				paste(V(g1)$name[match(strts,V(g1)$name)], "_part", sep="")
			# remove streets not considered as short
			idx <-  startsWith(V(g1)$name,"Rue:") & short.street.flag & !streets.all.flag
			g1 <- delete_vertices(graph=g1, v=idx)
			# remove nodes of innapropriate type (areas, villages, walls)
			idx <- startsWith(V(g1)$name,"Quartier:") | startsWith(V(g1)$name,"Bourg:")	 | startsWith(V(g1)$name,"Rempart:")
			g1 <- delete_vertices(graph=g1, v=idx)
		}
		else
		{	g1 <- delete_edges(graph=g, edges=which(E(g)$type!=link.types[i]))
			#g1 <- delete_vertices(graph=g1, v=which(degree(g, mode="all")==0))
		}
		g1$name <- link.types[i]
		
		# init folder
		graph.folder <- file.path(FOLDER_OUT_ANAL_EST, g1$name)
		dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
		
		# check graph validity
		if(link.types[i]!=LK_TYPE_ALL && link.types[i]!=LV_ESTATE && any_multiple(graph=g1))
		{	el <- as_edgelist(graph=g1, names=FALSE)
			# loops
			idx.loop <- which(count_multiple(g1)<1)
			tlog(6,"Loops: ",length(idx.loop))
			if(length(idx.loop)>0)
			{	tab <- cbind(V(g1)$name[el[idx.loop,1]], 
						V(g1)$name[el[idx.loop,2]], 
						count_multiple(g1)[idx.loop], 
						sapply(1:length(idx.loop), function(j) 
								edge_attr(g1, LK_TYPE, E(g1)[el[idx.loop[j],1] %->% el[idx.loop[j],2]])))
				colnames(tab) <- c("Source","Target","Multiplicity","Type")
				print(tab)
				tab.file <- file.path(graph.folder, "pb_loops.txt")
				write.table(tab, file=tab.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
			}
			# multiple links
			idx.mult <- which(count_multiple(g1)>1)
			tlog(6,"Multiple links: ",length(idx.mult))
			if(length(idx.mult)>0)
			{	tab <- matrix(nrow=0, ncol=4)
				colnames(tab) <- c("Source","Target","Multiplicity","Type")
				for(j in 1:length(idx.mult))
				{	lids <- E(g1)[el[idx.mult[j],1] %->% el[idx.mult[j],2]]
					descr <- edge_attr(g1,LK_TYPE, E(g1)[lids])
					if(length(descr)>length(unique(descr)))
					{	row <- c(V(g1)$name[el[idx.mult[j],1]], 
								V(g1)$name[el[idx.mult[j],2]], 
								count_multiple(g1)[idx.mult[j]], 
								paste(descr, collapse=":"))
						tab <- rbind(tab, row)
					}
				}
				if(nrow(tab)>0)
				{	print(tab)
					tab.file <- file.path(graph.folder, "pb_multiple_links.txt")
					write.table(tab, file=tab.file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
				}
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
		
		# plot full graph using the specified (x,y)
		plot.file <- file.path(graph.folder, "graph")
		tlog(4,"Plotting graph in \"",plot.file,"\"")
		custom.gplot(g=g1, file=plot.file)
		#custom.gplot(g=g1)
		#
		# and plot using a spatialization method 
		plot.file <- file.path(graph.folder, "graph_kk")
		tlog(4,"Plotting graph in \"",plot.file,"\"")
		g2 <- g1; V(g2)$x <- V(g2)$x2; V(g2)$y <- V(g2)$y2
		custom.gplot(g=g2, file=plot.file)
		#custom.gplot(g=g2)
		
		# record graph as a graphml file
		graph.file <- file.path(graph.folder, FILE_GRAPH)
		tlog(4,"Recording graph in \"",graph.file,"\"")
		write.graphml.file(g=g1, file=graph.file)
	}
	
	return(link.types)
}

# TODO y a t il une correlation entre la taille du composant et les différents attributs ?

# TODO que veut-on exprimer avec le réseau de confronts ? la proximité spatiale ?
#      donc peut-être transformer rues en lien si deux biens sont localisés au même niveau, 
# 	   et possiblement appliquer le même principe à d'autres relations.
