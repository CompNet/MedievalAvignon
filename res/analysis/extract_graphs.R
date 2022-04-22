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
# Function used by get.location.names to update only the names that are still NA.
#
# g: considered graph.
# vs: ids of the vertices (default: all of them).
# att.names: attribute names to consider.
# cur.names: current names.
#
# returns: updated names.
#############################################################################################
complete.names <- function(g, vs=1:gorder(g), att.names, cur.names)
{	#print(att.name)
	g.atts <- vertex_attr_names(g)
	idx <- which(is.na(cur.names))
	if(length(idx)>0)
	{	v.names <- rep(NA,length(idx))
		for(att.name in att.names)
		{	if(att.name %in% g.atts)
			{	tmp <- vertex_attr(graph=g, name=att.name, index=vs[idx])
				na.idx <- is.na(v.names)
				val.idx <- !is.na(v.names) & !is.na(tmp)
				v.names[na.idx] <- tmp[na.idx]
				v.names[val.idx] <- paste0(v.names[val.idx], "_", tmp[val.idx])
			}
		}
		cur.names[idx] <- v.names
	}
	
	return(cur.names)
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
	
	att.names <- list()
	# name
	att.names <- c(att.names, unique(c(
		# current name
		COL_STREET_NAME_CURR,
		# translated name
		COL_GATE_NAME_FRE, COL_AREA_NAME_FRE, COL_WALL_NAME_FRE, 
		# name
#		COL_FIX_NAME, 
		COL_EDIF_NAME, COL_VILG_NAME, COL_CARD_NAME, COL_LDMRK_NAME, COL_STREET_NAME, 
		# latin name
		COL_GATE_NAME_LAT, COL_AREA_NAME_LAT, COL_WALL_NAME_LAT
	)))
#	# qualification
#	att.names <- c(att.names, unique(c(
#		# normalized qualification
#		COL_EST_QUALIF_NORM,
#		# latin qualification
#		COL_EST_QUALIF_LAT
#	)))
#	# type
#	att.names <- c(att.names, unique(c(
#		# translated type
#		COL_EST_TYPE_FRE,
#		# type
##		COL_FIX_TYPE, 
#		COL_EDIF_TYPE, COL_VILG_TYPE, COL_CARD_TYPE, COL_GATE_TYPE, COL_WALL_TYPE, COL_LDMRK_TYPE, COL_STREET_TYPE, 
#		# latin type
#		COL_EST_TYPE_LAT
#	)))
	# components
	att.names[[length(att.names)+1]] <- c(COL_EST_COMP_LAB1, COL_EST_COMP_LAB2, COL_EST_COMP_LAB3, COL_EST_COMP_LAB4, COL_EST_COMP_LAB5, COL_EST_COMP_LAB6)
#	# others
#	att.names <- c(att.names, unique(c(
#		# detail
#		COL_EST_DETAIL
#	)))
	# id
	att.names <- c(att.names, unique(c(
		COL_LOC_ID,
		COL_EST_ID, #COL_FIX_ID, 
		COL_EDIF_ID, COL_VILG_ID, COL_CARD_ID, COL_GATE_ID, COL_AREA_ID, COL_WALL_ID, COL_LDMRK_ID, COL_STREET_ID
	)))
	
	# complete with various graph attributes
	for(att.name in att.names)
	{	#print(att.name)
		result <- complete.names(g=g, vs=vs, att.names=att.name, cur.names=result)
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
	
	# display types of edge
	tlog(4,"Types of edges:")
	etypes <- sort(unique(edge_attr(graph=g, name=LK_TYPE)))
	for(etype in etypes)
	{	tlog(6, "Type \"",etype,"\"")
		idx <- which(edge_attr(graph=g, name=LK_TYPE)==etype)
		edescrs <- sort(unique(edge_attr(graph=g, name=LK_DESCR, index=idx)))
		for(edescr in edescrs)
			tlog(8, edescr)
	}
	
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
			#sort(unique(edge_attr(g1,LK_TYPE)))
		}
		g1$name <- LK_TYPE_SOC_LST[i]
		
#		# remove isolated nodes
#		idx <- igraph::degree(g1) < 1
#		tlog(6,"Removing ",length(which(idx))," isolated nodes")
#		g1 <- delete_vertices(graph=g1, v=idx)
#		#
#		tlog(6,"Remaining: n=",gorder(g1)," m=",gsize(g1))
		
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
			#sort(unique(edge_attr(g1,LK_DESCR)))
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
load.location.table <- function(tab.file, type)
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
	
	# remove unbreakable spaces (does not work)
#	for(i in 1:ncol(tab))
#	{	idx <- which(grepl(" ", tab[, i], fixed=TRUE))
#		tab[idx, i] <- gsub(" ", " ", tab[idx, i])
#	}
	
	return(tab)
}




########################################################################
# Converts amount of money using the smaller available currency unit.
#
# values: vector of money amounts.
#
# returns: converted amounts.
########################################################################
convert.currency <- function(values)
{	# conversion map
	# 1 lb = 20 sol.
	# 1 flor. = 12 sol.
	# 1 sol. = 12 den.
	# 1 den. = 2 ob.
	# 1 ob. = 2 pict.
	conv.map <- c(
		"lb"=20*12*2*2, 
		"flor"=12*12*2*2, 
		"sol"=12*2*2, 
#"gross"=2*2*20,	# mentioned in the text, but this conversion rule is not always true
		"den"=2*2, 
		"ob"=2,
		"pict"=1
	)

#	vals <- sort(unique(info.fees[,"montantN"]))	# debug
#	vals <- info.fees[,"montantN"]					# debug
	vals <- values
	total.vals <- rep(0, length(vals))
	
	tlog(0,"Converting the currency values")
	for(currency in names(conv.map))
	{	curr <- paste0(currency,".")
		tlog(2,"Treating currency \"",currency,"\"")
		#vals <- trimws(vals, whitespace="[\\h\\v]")
		vals <- trimws(vals)
		
		pos <- str_locate(vals,paste0(" ",curr))
		num.vals <- rep(0,nrow(pos))
		for(r in 1:nrow(pos))
		{	if(!is.na(vals[r]))
			{	old.val <- vals[r]
				if(!is.na(pos[r,1]))
				{	str <- substr(vals[r], 1, pos[r,1]-1)
					value <- suppressWarnings(as.numeric(str))
					if(is.na(value))
					{	#tlog(6,"WARNING: Could not convert \"",str,"\" into numerical a ",currency," values")
					}
					else
					{	num.vals[r] <- value
						if(pos[r,2]==nchar(vals[r]))
							vals[r] <- ""
						else
							vals[r] <- substr(vals[r], pos[r,2]+1, nchar(vals[r]))
					}
				}
				if(old.val==vals[r])
					tlog(4,"row ",r,": \"",old.val,"\" >>(",num.vals[r],")>> no change")
				else
					tlog(4,"row ",r,": \"",old.val,"\" >>(",num.vals[r],")>> \"",vals[r],"\"")
			}
		}
		
		total.vals <- total.vals + num.vals*conv.map[currency]
	}
	#print(vals)
	
	# if text remains, then no (usable) monetary value
	vals <- trimws(vals)
	#vals <- trimws(vals, whitespace="[\\h\\v]")
	print(vals)
	idx <- which(vals!="" | is.na(values))
	total.vals[idx] <- NA
	
	return(total.vals)
}




########################################################################
# Normalizes real-estate components.
#
# comps: original components.
#
# returns: same matrix, but with normalized categories.
########################################################################
normalize.components <- function(comps)
{	# define conversion map
	map <- c(
		"anniversaire"="anniversaire",
		"arc"="arc-gallerie",
		"galerie"="arc-gallerie",
		"arriere cour"="arriere-cour",
		"atelier"="atelier-auvent",
		"auvant"="atelier-auvent",
		"bistour"="bistour",
		"boucherie"="boucherie",
		"bourg"="bourg",
		"bourguet"="bourg",
		"part de bourguet"="bourg",
		"boutique"="boutique",
		"cave"="cave-cellier",
		"cellier"="cave-cellier",
		"cimetiere"="cimetiere",
		"cloitre"="cloitre",
		"conduit d eau"="conduit eau",
		"edifice"="edifice",
		"emplacement occupe par des batiments ou destine a etre bati"="emplacement bat",
		"lieu place"="emplacement bat",
		"part d emplacement occupe par des batiments ou destine a etre bati"="emplacement bat",
		"forge"="forge",
		"four"="four",
		"grange"="grange-grenier",
		"grenier a foin"="grange-grenier",
		"hopital"="hopital",
		"enclos ou petite cour fermee"="jardin",
		"jardin"="jardin",
		"part de jardin"="jardin",
		"loge"="loge",
		"magasin"="magasin",
		"habitation"="maison",
		"maison"="maison",
		"mur ou fondation de maison"="maison",
		"part de maison"="maison",
		"rez de chaussee"="maison",
		"etage"="masure-salle-piece",
		"masure"="masure-salle-piece",
		"piece"="masure-salle-piece",
		"salle"="masure-salle-piece",
		"mur"="mur",
		"part de fosse"="part de fosse",
		"part de lices"="part de lices",
		"part de moulin"="part de moulin",
		"part de rempart"="part de rempart",
		"part de sorgue"="part de sorgue",
		"parvis de l eglise"="parvis eglise",
		"angle"="place-angle-plan",
		"place"="place-angle-plan",
		"plan"="place-angle-plan",
		"cour"="place-cour",
		"place cour"="place-cour",
		"pont"="pont",
		"portail"="portail",
		"sommet de portail"="portail",
		"porte dans le rempart"="porte rempart",
		"part de puit"="puit",
		"puit"="puit",
		"table"="table",
		"taverne"="taverne",
		"pre"="terre-pre",
		"terrain"="terre-pre",
		"terre"="terre-pre",
		"tinel"="tinel",
		"toit"="toit",
		"tour"="tour",
		"verger"="verger",
		"treille"="vigne",
		"vigne"="vigne",
		"chemin"="voie-traverse-passage-entree",
		"entree de bourguet"="voie-traverse-passage-entree",
		"entree de maison"="voie-traverse-passage-entree",
		"passage"="voie-traverse-passage-entree",
		"traverse"="voie-traverse-passage-entree",
		"voie"="voie-traverse-passage-entree"
	)
	# normalize each column
	res <- apply(as.matrix(comps), 2, function(col) map[col])
	col <- 1; which(!is.na(comps[,col]) & is.na(res[,col]))
	return(res)
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
info.estate <- info.estate[,-which(colnames(info.estate) %in% c(COL_EST_STREET_ID))] # COL_EST_AREA_ID, COL_EST_VILLAGE_ID were in this list, but we can use them as attributes too (in addition to membership relation)
	info.estate <- cbind(paste("Bien:",info.estate[,COL_EST_ID],sep=""),info.estate); colnames(info.estate)[1] <- COL_LOC_ID
	info.estate <- cbind(rep("Bien",nrow(info.estate)),info.estate); colnames(info.estate)[1] <- COL_LOC_TYPE
		# complete estate information
		info.fees <- load.location.table(FILE_IN_ANAL_ESTATE_FEE,"fee")
		mids <- match(info.estate[,COL_EST_FEE_ID], info.fees[,COL_FEE_ID])
		taxes <- convert.currency(info.fees[mids,COL_FEE_AMOUNT_NORM1])
		info.estate <- cbind(info.estate, taxes)
		colnames(info.estate)[ncol(info.estate)] <- COL_FEE_AMOUNT_NORM1
		FEE_BREAKS <- c(0, 34, 60, 144, 336, 624, 1296, 3600, 30000)
		FEE_CATS <- c(paste0("[",FEE_BREAKS[1],";",FEE_BREAKS[2],"]"), sapply(3:length(FEE_BREAKS), function(b) paste0("]",FEE_BREAKS[b-1],";",FEE_BREAKS[b],"]")))
		taxCats <- sapply(taxes, function(fee) if(is.na(fee)) NA else FEE_CATS[min(which(FEE_BREAKS>fee))-1])
		info.estate <- cbind(info.estate, taxCats)
		colnames(info.estate)[ncol(info.estate)] <- COL_FEE_AMOUNT_CAT1
		comp.norm <- normalize.components(info.estate[,c(COL_EST_COMP_LAB1,COL_EST_COMP_LAB2,COL_EST_COMP_LAB3,COL_EST_COMP_LAB4,COL_EST_COMP_LAB5,COL_EST_COMP_LAB6)])
		colnames(comp.norm) <- c(COL_EST_COMP_NORM1, COL_EST_COMP_NORM2, COL_EST_COMP_NORM3, COL_EST_COMP_NORM4, COL_EST_COMP_NORM5, COL_EST_COMP_NORM6)
		info.estate <- cbind(info.estate, comp.norm)
	cols <- colnames(info.estate)
	total.nbr <- nrow(info.estate)
	# load area information
	info.area <- load.location.table(FILE_IN_ANAL_AREA_NODES,"area")
	info.area <- cbind(paste("Quartier:",info.area[,COL_AREA_ID],sep=""),info.area); colnames(info.area)[1] <- COL_LOC_ID
	info.area <- cbind(rep("Quartier",nrow(info.area)),info.area); colnames(info.area)[1] <- COL_LOC_TYPE
	cols <- c(cols, colnames(info.area))
	total.nbr <- total.nbr + nrow(info.area)
	# load fix information
#	info.fix <- load.location.table(FILE_IN_ANAL_FIX_NODES,"fix")
#	info.fix <- cbind(paste("In",info.fix[,COL_FIX_ID],sep=""),info.fix); colnames(info.fix)[1] <- COL_LOC_ID
#	info.fix <- cbind(rep("In",nrow(info.fix)),info.fix); colnames(info.fix)[1] <- COL_LOC_TYPE
#	cols <- c(cols, colnames(info.fix))
#	total.nbr <- total.nbr + nrow(info.fix)
	info.village <- load.location.table(FILE_IN_ANAL_VILG_NODES,"village")
	info.village <- cbind(paste("Bourg:",info.village[,COL_VILG_ID],sep=""),info.village); colnames(info.village)[1] <- COL_LOC_ID
	info.village <- cbind(rep("Bourg",nrow(info.village)),info.village); colnames(info.village)[1] <- COL_LOC_TYPE
	cols <- c(cols, colnames(info.village))
	total.nbr <- total.nbr + nrow(info.village)
	info.edifice <- load.location.table(FILE_IN_ANAL_EDIFICE_NODES,"edifice")
	info.edifice <- cbind(paste("Edifice:",info.edifice[,COL_EDIF_ID],sep=""),info.edifice); colnames(info.edifice)[1] <- COL_LOC_ID
	info.edifice <- cbind(rep("Edifice",nrow(info.edifice)),info.edifice); colnames(info.edifice)[1] <- COL_LOC_TYPE
	cols <- c(cols, colnames(info.edifice))
	total.nbr <- total.nbr + nrow(info.edifice)
	info.cardinal <- load.location.table(FILE_IN_ANAL_CARD_NODES,"cardinal")
	info.cardinal <- cbind(paste("Livree:",info.cardinal[,COL_CARD_ID],sep=""),info.cardinal); colnames(info.cardinal)[1] <- COL_LOC_ID
	info.cardinal <- cbind(rep("Livree",nrow(info.cardinal)),info.cardinal); colnames(info.cardinal)[1] <- COL_LOC_TYPE
	cols <- c(cols, colnames(info.cardinal))
	total.nbr <- total.nbr + nrow(info.cardinal)
	info.gate <- load.location.table(FILE_IN_ANAL_GATE_NODES,"gate")
	info.gate <- cbind(paste("Porte:",info.gate[,COL_GATE_ID],sep=""),info.gate); colnames(info.gate)[1] <- COL_LOC_ID
	info.gate <- cbind(rep("Porte",nrow(info.gate)),info.gate); colnames(info.gate)[1] <- COL_LOC_TYPE
	cols <- c(cols, colnames(info.gate))
	total.nbr <- total.nbr + nrow(info.gate)
	info.wall <- load.location.table(FILE_IN_ANAL_WALL_NODES,"wall")
	info.wall <- cbind(paste("Rempart:",info.wall[,COL_WALL_ID],sep=""),info.wall); colnames(info.wall)[1] <- COL_LOC_ID
	info.wall <- cbind(rep("Rempart",nrow(info.wall)),info.wall); colnames(info.wall)[1] <- COL_LOC_TYPE
	cols <- c(cols, colnames(info.wall))
	total.nbr <- total.nbr + nrow(info.wall)
	info.landmark <- load.location.table(FILE_IN_ANAL_LDMRK_NODES,"landmark")
	info.landmark <- cbind(paste("Repere:",info.landmark[,COL_LDMRK_ID],sep=""),info.landmark); colnames(info.landmark)[1] <- COL_LOC_ID
	info.landmark <- cbind(rep("Repere",nrow(info.landmark)),info.landmark); colnames(info.landmark)[1] <- COL_LOC_TYPE
	cols <- c(cols, colnames(info.landmark))
	total.nbr <- total.nbr + nrow(info.landmark)
	info.street <- load.location.table(FILE_IN_ANAL_STREET_NODES,"street")
	info.street <- cbind(paste("Rue:",info.street[,COL_STREET_ID],sep=""),info.street); colnames(info.street)[1] <- COL_LOC_ID
	info.street <- cbind(rep("Rue",nrow(info.street)),info.street); colnames(info.street)[1] <- COL_LOC_TYPE
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
	tlog(4,"Found ",length(empty.cols)," all-NA columns: removing them (",paste(colnames(info.all)[empty.cols],collapse=", "),")")
	info.all <- info.all[,-empty.cols]
	
	# load relationships
	tlog(2,"Loading relational information")
	data <- read.table(
#		file=FILE_IN_ANAL_CONFR_LINKS_ALL,
		file=FILE_IN_ANAL_CONFR_LINKS_14c,
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
	{	#print(r)
		# get the source id
		if(is.na(data[r,COL_CONF_EST1_ID]))
			stop(paste0("ERROR: found no source id in row #",r))
		src.id <- which(info.all[,COL_LOC_ID]==paste0("Bien:",data[r,COL_CONF_EST1_ID]))
		if(is.na(src.id))
			stop(paste0("ERROR: could not find an estate matching source id in row #",r))
		
		# get the second estate id
		tgt.id <- c()
		if(!is.na(data[r,COL_CONF_EST2_ID]))
			tgt.id <- c(tgt.id, which(info.all[,COL_LOC_ID]==paste0("Bien:",data[r,COL_CONF_EST2_ID])))
		if(!is.na(data[r,COL_CONF_FIX_ID]))
			tgt.id <- c(tgt.id, which(
#				info.all[,COL_LOC_ID]==paste0("In",data[r,COL_CONF_FIX_ID])
				info.all[,COL_LOC_ID]==paste0("Bourg:",data[r,COL_CONF_FIX_ID])
				| info.all[,COL_LOC_ID]==paste0("Edifice:",data[r,COL_CONF_FIX_ID])
				| info.all[,COL_LOC_ID]==paste0("Livree:",data[r,COL_CONF_FIX_ID])
				| info.all[,COL_LOC_ID]==paste0("Porte:",data[r,COL_CONF_FIX_ID])
				| info.all[,COL_LOC_ID]==paste0("Rempart:",data[r,COL_CONF_FIX_ID])
				| info.all[,COL_LOC_ID]==paste0("Repere:",data[r,COL_CONF_FIX_ID])
				| info.all[,COL_LOC_ID]==paste0("Rue:",data[r,COL_CONF_FIX_ID])
			))
		if(length(tgt.id)==0 && !is.na(data[r,COL_CONF_AREA_ID]))
			tgt.id <- c(tgt.id, which(info.all[,COL_LOC_ID]==paste0("Quartier:",data[r,COL_CONF_AREA_ID])))	
		
		if(length(tgt.id)==0)
		{	print(data[r,])		
			result <- c(NA,NA)
#			stop(paste0("ERROR: found no destination id in row #",r))
		}
		else if(length(tgt.id)>1)
		{	stop(paste0("ERROR: found several destination ids in row #",r))
		}
		else if(is.na(tgt.id))
		{	stop(paste0("ERROR: could not match the destination id in row #",r))
		}
		else
		{	src.ext.id <- info.all[src.id, COL_LOC_ID]
			tgt.ext.id <- info.all[tgt.id, COL_LOC_ID]
			result <- as.character(c(src.ext.id, tgt.ext.id))
		}
		return(result)
	}))
	
	# manual corrections and simplifications
	# VAL_CONF_TYPE_EGALE
	tlog(2,"Dealing with \"equal\" relationships")
	idx <- which(data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_EGALE)
	if(length(idx>0))
	{	# multiple equal relationships: create clique if some edges are missing
		tt <- table(c(edge.list[idx,]))
		if(any(tt>1))
		{	tlog(4,"Detected ",length(which(tt>1))," multiple relationships")
			ids <- names(tt[tt>1])
			for(id in ids)
			{	tlog(6,"Dealing with ",paste(ids,collapse=", "))
				vtx <- which(edge.list[idx,1]==id | edge.list[idx,2]==id)
				vertices <- unique(c(edge.list[idx[vtx],]))
				tlog(8,"Complete list of neighbors: ",paste(vertices,collapse=", "))
				for(v1 in 1:(length(vertices)-1))
				{	for(v2 in (v1+1):length(vertices))
					{	# possibly add edge if missing
						if(any(edge.list[,1]==vertices[v1] & edge.list[,2]==vertices[v2]
							| edge.list[,1]==vertices[v2] & edge.list[,2]==vertices[v1]))
							tlog(10,"Edge ",vertices[v1],"--",vertices[v2]," already exists")
						else
						{	tlog(10,"Could not find edge ",vertices[v1],"--",vertices[v2]," >> adding it")
							edge.list <- rbind(edge.list, c(vertices[v1],vertices[v2]))
							df <- data.frame(0, "egal", NA, NA, NA, NA, "egal")
							colnames(df) <- colnames(data)
							data <- rbind(data, df)
						}
						
					}
				}
				idx <- idx[-vtx]
			}
		}
		
		# only pairwise equal relationships: merge estate into other vertex
		tlog(4,"Detected ",length(idx)," single relationships >> merging vertices")
		for(r in idx)
		{	# first node
			ext.id1 <- edge.list[r,1]
			id1 <- which(info.all[,COL_LOC_ID]==ext.id1)
			type1 <- info.all[id1,COL_LOC_TYPE]
			# second node
			ext.id2 <- edge.list[r,2]
			id2 <- which(info.all[,COL_LOC_ID]==ext.id2)
			type2 <- info.all[id2,COL_LOC_TYPE]
			
			# compare types, put kept node as first
			if(type1=="Bien")
			{	if(type2=="Bien")
					stop(paste0("ERROR: cannot merge two real estate nodes"))
				else
				{	ext.id1 <- edge.list[r,2]
					id1 <- which(info.all[,COL_LOC_ID]==ext.id1)
					ext.id2 <- edge.list[r,1]
					id2 <- which(info.all[,COL_LOC_ID]==ext.id2)
				}
			}
			else
			{	if(type2=="Bien")
				{	# nothing to do
				}
				else
					stop(paste0("ERROR: cannot merge two non-real estate nodes"))
			}
			new.id <- paste(ext.id1,ext.id2,sep="/")
			
			# substitute node id in edge list
			edge.list[edge.list[,1]==ext.id1,1] <- new.id
			edge.list[edge.list[,2]==ext.id1,2] <- new.id
			edge.list[edge.list[,1]==ext.id2,1] <- new.id
			edge.list[edge.list[,2]==ext.id2,2] <- new.id
			# merge rows in info table
			for(col in 1:ncol(info.all))
			{	if(is.na(info.all[id1,col]))
					val <- info.all[id2,col]
				else
					val <- info.all[id1,col]
				info.all[id1,col] <- val
			}
			info.all[id1,COL_LOC_ID] <- new.id
			info.all <- info.all[-id2,]
		}
		# remove "equal" relationships
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
	# VAL_CONF_TYPE_DELA
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DELA, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_MILIEU
#	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_MILIEU, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
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
#	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_ENTRE, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_EXTERIEUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_EXTERIEUR, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_DESSOUS
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DESSOUS, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_SOUS
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_SOUS, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_DESSUS
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_DESSUS, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_SUR
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_SUR, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	# VAL_CONF_TYPE_VERS
	data[data[,COL_CONF_LOC_NORM]==VAL_CONF_TYPE_VERS, COL_CONF_LOC_NORM] <- VAL_CONF_TYPE_MISC
	
	# sort(unique(data[,COL_CONF_LOC_NORM]))
	# ii <- which(E(g)$type==VAL_CONF_TYPE_INTERIEUR); cbind(get.edgelist(g)[ii,], E(g)$type[ii])
	# ii <- match(sort(unique(data[,COL_CONF_LOC_NORM])), data[,COL_CONF_LOC_NORM]); data[ii,c(COL_CONF_LOC_LAT,COL_CONF_LOC_NORM)]
	
	# build whole graph
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
	tlog(4,"Edge types (",length(link.types),"): ",paste(link.types,collapse=", "))
	
	# complete graph with nodal information
	tlog(2,"Adding to graph")
	idx <- match(V(g)$name, info.all[,COL_LOC_ID])
	atts <- colnames(info.all)
	for(i in 1:length(atts))
	{	att <- atts[i]
		tlog(4,"Processing attribute ",att," (",i,"/",length(atts),")")
		g <- set_vertex_attr(graph=g, name=att, value=info.all[idx,att])
	}
	tlog(4,"Number of vertices: ",gorder(g),"/",nrow(info.all))
	tlog(4,"Vertex attributes (",length(vertex_attr_names(g)),"): ",paste(vertex_attr_names(g),collapse=", "))
	
	# add composite name as label
	comp.names <- get.location.names(g)
	#print(which(is.na(comp.names)))
	V(g)$label <- comp.names
	
	# init layout
	tlog(2,"Set up layout")
	lay.file <- file.path(FOLDER_OUT_ANAL_EST,"layout.txt")
	# compute layout directly from igraph
##	layout <- layout_with_fr(g)
##	layout <- layout_with_fr(g, kkconst=0)
##	layout <- layout_nicely(g)
##	layout <- layout_with_dh(g)		# very slow
##	layout <- layout_with_gem(g)		# extremely slow
#	layout <- layout_with_kk(g, kkconst=gorder(g)/16)
##	layout <- layout_with_mds(g)
##	layout <- layout_with_lgl(g)
#	layout <- layout_with_graphopt(g, charge=0.01, spring.length=3)
#	# old code used to manually refine the layout
##		tkplot(g, layout=layout)
##		layout <- tk_coords(3)
	######
#	# export to graphml and use gephi, then import back
#	file <- file.path(FOLDER_OUT_ANAL_EST,"graph_kk.graphml")
#	write.graphml.file(g=g, file=file)
#	# <do your magic with gephi, then record graph with new layout>
#	g0 <- read.graph(file, format="graphml")
#	layout <- data.frame(V(g0)$idExterne, V(g0)$x, V(g0)$y)
#	colnames(layout) <- c("idExterne", "x","y")
#	scale <- max(abs(layout[,c("x","y")]))/7
#	layout[,c("x","y")] <- layout[,c("x","y")]/scale
#	write.table(x=layout, file=lay.file, sep="\t", row.names=FALSE, col.names=TRUE)
	######
	# update graph
	tlog(4,"Reading layout from file ",lay.file)
	layout <- read.table(file=lay.file, sep="\t", header=TRUE, check.names=FALSE)
	lay.idx <- match(V(g)$idExterne, layout[,"idExterne"])
	if(any(is.na(lay.idx))) stop("Could not match node ids with ids from the layout file")
	V(g)$x2 <- layout[lay.idx,"x"]; V(g)$y2 <- layout[lay.idx,"y"]
	# plot graph
	plot.file <- file.path(FOLDER_OUT_ANAL_EST,"graph_kk")
	tlog(4,"Plotting in file ",plot.file)
	g0 <- g
	V(g0)$x <- layout[lay.idx,"x"]; V(g0)$y <- layout[lay.idx,"y"]
	#V(g0)$label <- NA
	#idx <- which(degree(g)>5)
	#V(g0)[idx]$label <- comp.names[idx]
	V(g0)$label <- paste(vertex_attr(g0,name=COL_LOC_ID), get.location.names(g0),sep="_")
	custom.gplot(g=g0, file=plot.file, axes=FALSE, rescale=FALSE, xlim=range(V(g0)$x), ylim=range(V(g0)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
	write.graphml.file(g=g0, file=paste0(plot.file,".graphml"))
	
	# use spatial coordinates for layout
	tlog(2,"Using spatial coordinates to define layout")
	V(g)$x <- vertex_attr(g, name=COL_LOC_X)	# COL_LOC_X	COL_LOC_HYP_LON
	V(g)$y <- vertex_attr(g, name=COL_LOC_Y)	# COL_LOC_Y	COL_LOC_HYP_LAT
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
	# put completely disconnected nodes in bottom left corner
	V(g)$x[which(is.na(V(g)$x))] <- min(V(g)$x, na.rm=TRUE)
	V(g)$y[which(is.na(V(g)$y))] <- min(V(g)$y, na.rm=TRUE)
	# copy interpolated coordinates in new attribute
	V(g)$lonEst <- V(g)$x
	V(g)$latEst <- V(g)$y
	# plot full graph with these positions
	g1 <- g
#	V(g1)$label <- NA
	V(g1)$label <- paste(vertex_attr(g1,name=COL_LOC_ID), get.location.names(g1),sep="_")
	plot.file <- file.path(FOLDER_OUT_ANAL_EST,"graph_lambert")
	custom.gplot(g=g1, file=plot.file, size.att=2, vertex.label.cex=0.1)
	#custom.gplot(g=g1)
	write.graphml.file(g=g1, file=paste0(plot.file,".graphml"))

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
	
#	# possibly filter to focus on a single source
#	sources <- list(
#		S1=list(src.ids=c(1, 2, 3, 4, 5, 7, 18, 19, 20), re.ids=c(1:3999,6001:9999,60001:60999))
#	)
#	keep.idx <- which(is.na(V(g)$idBien) | V(g)$idBien %in% sources[[1]]$re.ids)
	
	#################
	# extract one graph for each predefined modality
	#################
	tlog(2,"Extracting several variants of the graph")
	link.types <- c(LV_ESTATE, LK_TYPE_FLATREL)		# c(LK_TYPE_ALL, LV_ESTATE, LK_TYPE_FLATREL, link.types)
	for(i in 1:length(link.types))
	{	tlog(4,"Extracting graph \"",link.types[i],"\" (",i,"/",length(link.types),")")
		
		# keep all links and nodes
		if(link.types[i]==LK_TYPE_ALL)
			g1 <- g
		# keep only the estate level (which includes short streets)
		else if(link.types[i]==LV_ESTATE)
		{	g1 <- g
			tlog(6,"Cleaning the graph (n=",gorder(g1),", m=",gsize(g1),")")
			# change the name of certain streets whose only a part is targeted in the confronts
			streets.all <- union(street.angles, street.starts)
			strts <- sort(unique(edge.list[streets.all,2]))						# concerned parts of streets
			tlog(8,"Detected ",length(streets.all)," confronts with ",length(strts), " parts of streets")
			streets.all.flag <- V(g1)$name %in% strts							# mark them for later (to not remove them)
			rem.idx <- setdiff(which(edge.list[,2] %in% strts), streets.all)	# relations with these streets (but as complete streets)
			g1 <- delete_edges(g1, edges=rem.idx)								# remove these links
			V(g1)$name[match(strts,V(g1)$name)] <- 								# change the name of the remaining streets
				paste(V(g1)$name[match(strts,V(g1)$name)], "_part", sep="")
			# remove streets not considered as short
			tlog(8,"Detected ",length(which(short.street.flag))," short streets")
			idx <-  startsWith(V(g1)$name,"Rue:") & !short.street.flag & !streets.all.flag
			tlog(8,"Removing ",length(which(idx))," street nodes")
			g1 <- delete_vertices(graph=g1, v=idx)
			# remove nodes of higher type (areas, villages, walls)
			idx <- startsWith(V(g1)$name,"Quartier:") | startsWith(V(g1)$name,"Bourg:") | startsWith(V(g1)$name,"Rempart:")
			tlog(8,"Removing ",length(which(idx))," areas/villages/walls")
			g1 <- delete_vertices(graph=g1, v=idx)
			# remove certain geological objects
			idx <- startsWith(V(g1)$name,"Repere:") & vertex_attr(g1,COL_LDMRK_TYPE)!="Rocher"
			tlog(8,"Removing ",length(which(idx))," geological object")
			g1 <- delete_vertices(graph=g1, v=idx)
		}
		# keep everything but the membership relations (and remove isolates)
		else if(link.types[i]==LK_TYPE_FLATREL)
		{	g1 <- g
			tlog(6,"Cleaning the graph (n=",gorder(g1),", m=",gsize(g1),")")
			# removing membership relations
			idx <- which(E(g1)$type==VAL_CONF_TYPE_INTERIEUR)
			tlog(8,"Removing ",length(idx)," \"inside\" confronts")
			g1 <- delete_edges(graph=g1, edges=idx)
		}
		# keep only one type of link
		else
		{	# delete the links of the other types
			g1 <- delete_edges(graph=g, edges=which(E(g)$type!=link.types[i]))
			#g1 <- delete_vertices(graph=g1, v=which(degree(g, mode="all")==0))
		}
		g1$name <- link.types[i]
		
		# remove isolated nodes
		idx <- igraph::degree(g1) < 1
		tlog(6,"Removing ",length(which(idx))," isolated nodes")
		g1 <- delete_vertices(graph=g1, v=idx)
		#
		tlog(6,"Remaining: n=",gorder(g1)," m=",gsize(g1))
		
		# init folder
		graph.folder <- file.path(FOLDER_OUT_ANAL_EST, g1$name)
		dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
		
		# check graph validity
		if(!(link.types[i] %in% c(LK_TYPE_ALL, LV_ESTATE, LK_TYPE_FLATREL)) && any_multiple(graph=g1))
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
		
		# plot the graph using the geographic coordinates
		#g1 <- update.node.labels(g1, vals=degree(g1))
		V(g1)$label <- paste(vertex_attr(g1,name=COL_LOC_ID), get.location.names(g1),sep="_")
		plot.file <- file.path(graph.folder, "graph_lambert")
		tlog(4,"Plotting graph using geographic coordinates in \"",plot.file,"\"")
		custom.gplot(g=g1, file=plot.file, asp=1, size.att=2, vertex.label.cex=0.1)
		#custom.gplot(g=g1)
		write.graphml.file(g=g1, file=paste0(plot.file,".graphml"))

		# plot the graph using a layouting algorithm 
		g2 <- g1#; V(g2)$x <- V(g2)$x2; V(g2)$y <- V(g2)$y2
		#V(g2)$label <- NA; idx <- which(degree(g2)>3); V(g2)[idx]$label <- comp.names[idx]
		V(g2)$label <- paste(vertex_attr(g2,name=COL_LOC_ID), get.location.names(g2),sep="_")
		plot.file <- file.path(graph.folder, "graph_kk")
		tlog(4,"Plotting graph using layouting algorithm in \"",plot.file,"\"")
		lay.file <- file.path(graph.folder, "layout.txt")
		###### init layout quasi-manually
#		# export to graphml and use gephi, then import back
#		g0 <- g1
#		layout <- layout_with_kk(g0, kkconst=5)
#		V(g0)$x <- layout[,1]; V(g0)$y <- layout[,2]; 
#		custom.gplot(g=g0, file=plot.file, axes=FALSE, rescale=FALSE, xlim=range(V(g0)$x), ylim=range(V(g0)$y), vertex.label.cex=0.1, size.att=6)
#		write.graphml.file(g=g0, file=paste0(plot.file,".graphml"))
#		# <do your magic with gephi, then record graph with new layout>
#		g0 <- read.graph(paste0(plot.file,".graphml"), format="graphml")
#		layout <- data.frame(V(g0)$idExterne, V(g0)$x, V(g0)$y)
#		colnames(layout) <- c("idExterne", "x","y")
#		scale <- max(abs(layout[,c("x","y")]))/7
#		layout[,c("x","y")] <- layout[,c("x","y")]/scale
#		write.table(x=layout, file=lay.file, sep="\t", row.names=FALSE, col.names=TRUE)
		######
		layout <- read.table(file=lay.file, sep="\t", header=TRUE, check.names=FALSE)
		lay.idx <- match(V(g2)$idExterne, layout[,"idExterne"])
		if(any(is.na(lay.idx))) stop("Could not match node ids with ids from the layout file")
		V(g2)$x <- layout[lay.idx,"x"]; V(g2)$y <- layout[lay.idx,"y"]
		E(g2)$weight <- 0.5
		#
		custom.gplot(g=g2, file=plot.file, axes=FALSE, rescale=FALSE, xlim=range(V(g2)$x), ylim=range(V(g2)$y), vertex.label.cex=0.1, size.att=6)
		#custom.gplot(g=g2)
		write.graphml.file(g=g2, file=paste0(plot.file,".graphml"))
		
		# record graph as a graphml file
		V(g1)$x2 <- V(g2)$x
		V(g1)$y2 <- V(g2)$y
		graph.file <- file.path(graph.folder, FILE_GRAPH)
		tlog(4,"Recording graph in \"",graph.file,"\"")
		write.graphml.file(g=g1, file=graph.file)
		
		# record the filtered version keeping only the main components
		if(link.types[i]==LV_ESTATE)
			cmp.thre <- 15
		else if(link.types[i]==LK_TYPE_FLATREL)
			cmp.thre <- 25
		tmp <- components(graph=g1, mode="weak")
		cmps <- which(tmp$csize<cmp.thre)
		idx <- which(tmp$membership %in% cmps)
		if(length(idx)>1)
		{	# filter graph
			g1 <- delete_vertices(graph=g1, v=idx)
			g1$name <- paste0(g1$name,"_filtered")
			g2 <- delete_vertices(graph=g2, v=idx)
			g2$name <- paste0(g2$name,"_filtered")
			# record as graphml
			graph.folder <- file.path(FOLDER_OUT_ANAL_EST, g1$name)
			dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
			graph.file <- file.path(graph.folder, FILE_GRAPH)
			tlog(4,"Recording filtered graph in \"",graph.file,"\"")
			write.graphml.file(g=g1, file=graph.file)
			# plot
			plot.file <- file.path(graph.folder, "graph_lambert")
			custom.gplot(g=g1, file=plot.file, asp=1, size.att=2, vertex.label.cex=0.1)
			write.graphml.file(g=g1, file=paste0(plot.file,".graphml"))
			plot.file <- file.path(graph.folder, "graph_kk")
			custom.gplot(g=g2, file=plot.file, axes=FALSE, rescale=FALSE, xlim=range(V(g2)$x), ylim=range(V(g2)$y), vertex.label.cex=0.1, size.att=6)
			write.graphml.file(g=g2, file=paste0(plot.file,".graphml"))
		}
	}
	
	return(link.types)
}

###### GÉNÉRAL ######
# TODO calculer plutot la moyenne harmonique pour la distance ?
#
# TODO similarité structurelle : généraliser en tenant compte des labels des liens?



###### IMMOBILIER ######
# TODO utiliser MDS pour positionner sur la base de la distance géodésique 
# les noeuds dont on ne connait pas la position spatiale exacte 
#
# TODO plusieurs terriers représentant les mêmes biens à des époques différentes
# >> extraire autant de graphes (en intégrant les noeuds issus des autres terriers, constants)
# >> faire du matching de noeud pr mettre les biens en correspondance 
#	 similarité structurelle + similarité d'attributs ?
#
# TODO la closeness semble distribuée de façon gaussienne : est-ce dû à une contrainte spatiale ?
#      comparer avec la distrib du degré (et autres centralités)
#
# TODO besoin de tenir compte du degré pour interpréter sim struct (et trans)
#      plus facile pour petits noeuds d'avoir des valeurs élevées pour ces mesures
#
# TODO la transitivité pourrait-elle permettre de gagner de l'information spatiale ?
#      > si deux noeuds sont proches (connectés ?) et ont une transitivité élevée, sont ils proches spatialement ?
#      > différence avec sim structurelle ?

# TODO
# + graphiques des graphes
#   + rajouter les noms dans la version géo des plots de graphes
#   + utiliser les coordonnées lambert93 à la place des coordonnées actuelles
#   + tous les noms n'apparaissent pas systématiquement (ex. kk racine)
#   + générer les graphiques indiv avec la spatialisation algo (en plus de la géo)
#   + manque l'id du noeud dans les noms de fichiers individuels (ex. distance)
# + ne pas calculer les mesures orientées
# + liens de type "égale" :
#   + pb égale :
#     + si traité à la construction du graphe, pq tjrs dans la légende ?
#     + pq apparait en gris et pas en rose (Bien:2015_cimetiere -- Edifice:720_Cimetiere juif)
#     + pq il reste des liens égale alors qu'on les traite avant
#     > "egal" dans les données au lieu de "egale"
#   + fusion des liens "égale" : 
#     + conserver le noeud qui n'est pas un bien, garder un max d'attributs
#     + concaténer les id dans le nom affiché dans les graphiques
#     + si plusieurs biens >> pas de fusion
# + utiliser les catégories de montants définies avec margot
# - structure de communautés
#   + accord entre algos >> calculer les mesures de comparaison de partitions standard
#   - lien hiérarchique entre communautés detectées sur graphes avec vs. sans rues ?
#     > pas les mêmes noeuds : difficile à comparer
#   + comparer struct com avec seigneurie (partitions)
#     > déjà fait en partie avec la pureté (calculée pour chaque attribut)
#   + calculer la centralisation des communautés (sont-elles construites autour d'invariants ?)
#     ? traiter chaque communauté comme un graphe à part, histoire de produire toutes les stats ?
#   - spatialisation du graphe sans rue, avec coms, on fixe leurs coordonnées puis on rajoute les rues (du graphe plat) et on étudie leur position (est elle intermédiaire entre coms ?)
#     + alt : prendre le graphe plat et représenter coms sans rues vs. coms graphe plat, voir où sont les noeuds sans com (=rues) 
#   > choix de la méthode de détection: si com construite autour d'une seule rue, ou correspond à une division admin, alors trop évident. il faut qqch entre les deux.
#   > observations :
#     > edgebetweenness/fastgreedy/louvain: 40aine de coms, semble pertinent
#     > eigenvector: pertinent mais plus grosses coms
#     > infomap/labelprop/walktrap: très nombreuses coms (100--200)
#
# - margot:
#   + map de conversion pour les composants de maison
#   + spécification de couleurs spécifiques pour certaines attributs
#   + compléter les liens "égale" manquants
#   - vérifier les liens à longue distance suspects

# TODO
# + supprimer les nbres de composants des attributs traités
# + vérifier les couleurs des attributs
# + les tags continuent à inclure les biens non déclarés (NA) lors du plot des coms
# + les couleurs appropriées sont elles utilisées pour plotter les tags dans les coms ?
# + vérifier le fichier de comparaison entre coms et attr : pq plusieurs ?
# - augmenter la taille des noeuds dans kk



###### SOCIAL ######
#
# TODO régler le pb de relations symétriques dans les données 
#      (au moins pour les relations ecclésiastiques)
#
# TODO voir comment gérer les relations familiales
# - doit-on représenter les relations symétriques (père vs. fils) ? 
#   >> plutôt non
# - doit on représenter les relations indirectes ?
#   >> celles de famille proche : frère/soeur, peut être grand-père/petits-fils
# - doit on déduire toutes les relations manquantes ?
#   - voire rajouter des noeuds pour représenter une relation indirecte dont un noeud intermédiaire manque ou est inconnu ?
#   >> pareil, plutot pour la famille proche
#
# TODO simplifier les relations genrées
