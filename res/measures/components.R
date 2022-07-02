#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_COMPONENTS <- "components"
MEAS_NBR_COMPONENTS <- "component_nbr"
MEAS_NBR_NODES <- "node_nbr"
MEAS_NBR_LINKS <- "link_nbr"
MEAS_LONG_NAMES[MEAS_COMPONENTS] <- "Components"
MEAS_LONG_NAMES[MEAS_NBR_COMPONENTS] <- "Component number"
MEAS_LONG_NAMES[MEAS_NBR_NODES] <- "Node number"
MEAS_LONG_NAMES[MEAS_NBR_LINKS] <- "Link number"




#############################################################
# Identifies the weak and strong components of the graph.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.components <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	# numbers of nodes and edges
	stats[paste0(MEAS_NBR_NODES), ] <- list(Value=gorder(g), Mean=NA, Stdv=NA)
	stats[paste0(MEAS_NBR_LINKS), ] <- list(Value=gsize(g), Mean=NA, Stdv=NA)
	
	# retrieve the list of vertex attributes
	nodal.atts <- list.vertex.attributes(g)
	att.list <- nodal.atts[!startsWith(nodal.atts,"_")]
	
	# computing components
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	for(mode in modes)
	{	tlog(2,"Computing components: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_COMPONENTS,"_",mode)
		comp.folder <- file.path(out.folder, g$name, MEAS_COMPONENTS, mode)
		dir.create(path=comp.folder, showWarnings=FALSE, recursive=TRUE)
		
		# detect components
		cmp <- components(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "weak" else "strong")
		mbrs <- cmp$membership
		comp.nbr <- cmp$no
		tlog(4,"Number of components: ",comp.nbr)
		
		# component size distribution
		plot.file <- file.path(comp.folder,paste0(fname,"_size_bars"))
		tlog(4,"Plotting distribution in '",plot.file,"'")
		sizes <- table(mbrs,useNA="ifany")
		custom.barplot(sizes, text=names(sizes), xlab="Component", ylab="Size", file=plot.file)
		
		# export CSV with component membership
		tab.file <- file.path(comp.folder,paste0(fname,"_membership.csv"))
		tlog(4,"Exporting as CSV in '",tab.file,"'")
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), mbrs)
		colnames(df) <- c("Id","Name","Component") 
		write.csv(df, file=tab.file, row.names=FALSE)
		
		# add results to the graph (as attributes) and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=mbrs)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_nbr"), value=comp.nbr)
		stats[paste0(fname,"_nbr"), ] <- list(Value=comp.nbr, Mean=NA, Stdv=NA)
		
		# continue with only the largest components
#		idx <- which(cmp$csize >= 0.1*gorder(g))	# keep only the components containing at least 10% of the nodes
		idx <- which(cmp$csize >= 10)				# too strict: switched to 10 nodes
		tlog(4,"Number of large components: ",length(idx))
		mbrs.big <- mbrs
		mbrs.big[is.na(match(mbrs.big,idx))] <- NA
		g <- set_vertex_attr(graph=g, name=fname, value=mbrs.big)
		
		# plot graph using color for components
		plot.file <- file.path(comp.folder,paste0(fname,"_graph"))
		tlog(4,"Plotting graph in '",plot.file,"'")
		#V(g)$label <- rep(NA, gorder(g))
		V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
		g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, cat.att=TRUE, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
		g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
		custom.gplot(g=g1, col.att=fname, cat.att=TRUE, file=paste0(plot.file,"_kk"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
		g <- set_vertex_attr(graph=g, name=fname, value=cmp$membership)
	
		# plot components separately
		sep.folder <- file.path(comp.folder, "_comps")
		dir.create(path=sep.folder, showWarnings=FALSE, recursive=TRUE)
		for(i in idx)
		{	# plot subgraph
			plot.file <- file.path(sep.folder,paste0("component_",i,"_lambert"))
			tlog(6,"Plotting component #",i,"/",length(idx)," in '",plot.file,"'")
			g2 <- induced_subgraph(graph=g, vids=which(mbrs.big==i))
#			if(gorder(g2)>20)
#				V(g2)$label <- rep(NA, gorder(g2))
#			else
#				V(g2)$label <- get.names(g2)
			V(g2)$label <- paste(vertex_attr(g2,name=COL_LOC_ID), get.location.names(g2),sep="_")
			g1 <- g2; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
			g1 <- g2; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, file=paste0(plot.file,"_kk"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
	
			# export subgraph
			graph.file <- file.path(sep.folder,paste0("component_",i,".graphml"))
			tlog(6,"Recording component #",i,"/",length(idx)," as graph in '",graph.file,"'")
			write.graphml.file(g=g, file=graph.file)
		}
		
		# assess component purity for all attributes
		g <- analyze.net.comstruct.attributes(g, comp.folder, mbrs)
	}
	
	# export CSV with results
	tlog(2,"Updating stat file '",stat.file,"'")
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	tlog(2,"Updating graph file '",graph.file,"'")
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Compute the correlation between component size and attribute values.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.components.corr <- function(g, out.folder)
{	# indices of real estate vertices
	est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)=="Bien")
	non.est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)!="Bien")
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	
	# init result table
	val.tab <- matrix(nrow=0,ncol=length(modes))
	colnames(val.tab) <- modes
	
	
	#############################
	# gathering categorical attributes
	tlog(4,"Gathering categorical attributes")
	cat.data <- NA
	
	# gather regular categorical attributes 
	attrs <- intersect(COL_CAT_SELECT, vertex_attr_names(g))
	for(attr in attrs)
	{	# get values only for real-estate vertices
		g0 <- delete_vertices(graph=g, v=non.est.idx)
		tmp <- vertex_attr(g0, attr)
		
		# add to matrix
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- intersect(names(COL_TAG_SELECT), vertex_attr_names(g))
	for(attr in attrs)
	{	g0 <- delete_vertices(graph=g, v=non.est.idx)
		tmp <- intersect(COL_TAG_SELECT[[attr]], vertex_attr_names(g))
		m <- sapply(tmp, function(att) vertex_attr(g0, att))
		# create a NA vs. rest attribute
		cat.data <- cbind(cat.data, apply(m, 1, function(v) if(all(is.na(v))) 1 else 2))
		colnames(cat.data)[ncol(cat.data)] <- paste0(attr,"_NAvsRest")
		# decompose into a set of boolean attributes
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	cat.data <- cbind(cat.data, as.integer(factor(apply(m, 1, function(v) uval %in% v[!is.na(v)]))))
			colnames(cat.data)[ncol(cat.data)] <- paste(attr,uval,sep="_")
		}
	}
	
	
	#############################
	# gathering numerical attributes
	tlog(4,"Gathering numerical attributes")
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM_SELECT, vertex_attr_names(g))
	for(attr in attrs)
	{	# get values only for real-estate vertices
		g0 <- delete_vertices(graph=g, v=non.est.idx)
		tmp <- vertex_attr(g0, attr)
		
		# add to matrix
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	
	#############################
	# computing correlation between component size and attribute values
	for(mode in modes)
	{	tlog(2,"Computing components: mode=",mode)
		
		# detect components
		cmp <- components(graph=g, mode=if(mode==MEAS_MODE_UNDIR) "weak" else "strong")
		mbrs <- cmp$membership[est.idx]
		comp.sizes <- cmp$csize[mbrs]
		
		# deal with categorical attributes
		tlog(4,"Dealing with categorical attributes")
		for(i in 1:ncol(cat.data))
		{	# compute the correlation
			attr <- colnames(cat.data)[i]
			tlog(6,"Computing attribute ",attr," (",i,"/",ncol(cat.data),")")
			
			# if there are some NAs
			if(any(is.na(cat.data[,i])))
			{	# explicitly represent NAs as a class
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)) || length(unique(cd))==1)
					cor <- NA
				else
				{	cd[is.na(cd)] <- max(cd,na.rm=TRUE) + 1
					fit <- aov(comp.sizes~as.factor(cd))
					cor <- eta_sq(fit)$etasq
					if(is.null(cor)) cor <- NA
				}
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,") when representing NAs explicitly: ",cor)
				name <- paste0(attr,"_expNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
				# just ignore NAs
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)) || length(unique(cd))==1)
					cor <- NA
				else
				{	cs <- comp.sizes[!is.na(cd)]
					cd <- cd[!is.na(cd)]
					if(length(unique(cd))==1)
						cor <- NA
					else
					{	fit <- aov(cs~as.factor(cd))
						cor <- eta_sq(fit)$etasq
						if(is.null(cor)) cor <- NA
					}
				}
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",cor)
				name <- paste0(attr,"_noNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
				# do NA vs. the rest
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)) || length(unique(cd))==1)
					cor <- NA
				else
				{	cd[!is.na(cd)] <- 1
					cd[is.na(cd)] <- 2
					fit <- aov(comp.sizes~as.factor(cd))
					cor <- eta_sq(fit)$etasq
					if(is.null(cor)) cor <- NA
				}
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,") when considering NAs vs the rest: ",cor)
				name <- paste0(attr,"_NAvsRest")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
			}
			
			# no NA at all
			else
			{	cd <- as.integer(cat.data[,i])
				if(length(unique(cd))==1)
					cor <- NA
				else
				{	fit <- aov(comp.sizes~as.factor(cd))
					cor <- eta_sq(fit)$etasq
					if(is.null(cor)) cor <- NA
				}
				tlog(8,"Association for attribute \"",attr,"\" (mode=",mode,"): ",cor)
				name <- paste0(attr)
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- cor
			}
		}
		
		# deal with numerical attributes
		tlog(4,"Dealing with numerical attributes")
		if(!is.null(ncol(num.data)))
		{	for(i in 1:ncol(num.data))
			{	# compute the correlation
				attr <- colnames(num.data)[i]
				tlog(10,"Computing attribute ",attr," (",i,"/",ncol(num.data),")")
				
				# if there are some NAs
				if(any(is.na(num.data[,i])))
				{	# explicitly represent them as zeroes
					cd <- num.data[,i]
					if(all(is.na(cd)))
						cor <- NA
					else
					{	cd[is.na(cd)] <- 0
						cor <- cor(cd, comp.sizes)
					}
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,") when replacing NAs by 0: ",cor)
					name <- paste0(attr,"_expNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
					# ignore them
					cd <- num.data[,i]
					if(all(is.na(cd)))
						cor <- NA
					else
					{	cs <- comp.sizes[!is.na(cd)]
						cd <- cd[!is.na(cd)]
						cor <- cor(cd, cs)
					}
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",cor)
					name <- paste0(attr,"_noNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
					# do NA vs. the rest
					cd <- num.data[,i]
					if(all(is.na(cd)))
						cor <- NA
					else
					{	cd[!is.na(cd)] <- 1
						cd[is.na(cd)] <- 2
						fit <- aov(comp.sizes~as.factor(cd))
						cor <- eta_sq(fit)$etasq
						if(is.null(cor)) cor <- NA
					}
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,") when considering NAs vs the rest: ",cor)
					name <- paste0(attr,"_NAvsRest")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
				}
				# no NA at all
				else
				{	cor <- cor(num.data[,i], comp.sizes)
					tlog(12,"Correlation for attribute \"",attr,"\" (mode=",mode,"): ",cor)
					name <- paste0(attr)
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- cor
				}
			}
		}
	}
	
	# record the results
	res.file <- file.path(out.folder, g$name, "component-size_attribute_corr.csv")
	tlog(4,"Recording results in file '",res.file,"'")
	write.csv(val.tab, file=res.file, row.names=TRUE)
	
	return(g)
}
