#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_COMMUNITIES <- "communities"
MEAS_LONG_NAMES[MEAS_COMMUNITIES] <- "Communities"




#############################################################
# Detects the community structure of the network.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.comstruct <- function(g, out.folder)
{	# get the stat table
	stat.file <- file.path(out.folder, g$name, "stats.csv")
	stats <- retrieve.stats(stat.file)
	
	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Detecting community structure: mode=",mode)
		
		# possibly create folder
		fname <- paste0(MEAS_COMMUNITIES,"_",mode)
		communities.folder <- file.path(out.folder,g$name,MEAS_COMMUNITIES,mode)
		dir.create(path=communities.folder, showWarnings=FALSE, recursive=TRUE)
		
		# detect communities
		#coms <- cluster_optimal(graph=simplify(g))		# much slower, obviously
		#coms <- cluster_spinglass(graph=simplify(g))
		#coms <- cluster_infomap(graph=simplify(g))
		coms <- cluster_edge_betweenness(graph=simplify(g), directed=mode)
		mod <- modularity(coms)
		mbrs <- as.integer(membership(coms))
		com.nbr <- length(unique(mbrs))
		tlog(4,"Number of communities: ",com.nbr)
		tlog(4,"Modularity: ",mod)
		
		# community size distribution
		sizes <- table(mbrs,useNA="ifany")
		custom.barplot(sizes, text=names(sizes), xlab="Community", ylab="Size", file=file.path(communities.folder,paste0(fname,"_size_bars")))
		
		# export CSV with community membership
		df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), mbrs)
		colnames(df) <- c("Id","Name","Community") 
		write.csv(df, file=file.path(communities.folder,paste0(fname,"_membership.csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and stats table
		g <- set_vertex_attr(graph=g, name=fname, value=mbrs)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_nbr"), value=com.nbr)
		g <- set_graph_attr(graph=g, name=paste0(fname,"_mod"), value=mod)
		stats[paste0(fname,"_nbr"), ] <- list(Value=com.nbr, Mean=NA, Stdv=NA)
		stats[paste0(fname,"_mod"), ] <- list(Value=mod, Mean=NA, Stdv=NA)
		
		# plot graph using color for communities
		V(g)$label <- rep(NA, gorder(g))
		custom.gplot(g=g, col.att=fname,cat.att=TRUE, file=file.path(communities.folder,paste0(fname,"_graph")))
		#custom.gplot(g=g, col.att=fname,cat.att=TRUE)
		
		# assess community purity for all attributes
		g <- analyze.net.comstruct.attributes(g, mode, out.folder)
	}
	
	# export CSV with average degree
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Compares the previously detected community structure and
# the attributes of the nodes.
#
# g: original graph to process.
# mode: directed or undirected.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.comstruct.attributes <- function(g, mode, out.folder)
{	tlog(2,"Comparing nodal attributes and communities")
	# possibly create folders
	coms.folder <- file.path(out.folder,g$name,"communities",mode)
	dir.create(path=coms.folder, showWarnings=FALSE, recursive=TRUE)
	
	# retrieve the list of vertex attributes
	nodal.atts <- list.vertex.attributes(g)
	att.list <- nodal.atts[!startsWith(nodal.atts,"_")]
	
	# retrieve community
	membership <- vertex_attr(graph=g, name=paste0(MEAS_COMMUNITIES,"_",mode))
	coms <- sort(unique(membership))
	
	# build community graph
	cg <- contract.vertices(g, mapping=membership)	# TODO we could keep edges of different types separted (familial, professional, etc.)
	E(cg)$weight <- 1
	cg <- simplify(cg, remove.loops=FALSE)
	for(eattr in edge_attr_names(graph=cg))
		cg <- delete_edge_attr(graph=cg, name=eattr)
	# setup its attributes
	V(cg)$name <- paste("C",coms,sep="")
	V(cg)$label <- paste("C",coms,sep="")
	V(cg)$size <- sapply(coms, function(i) length(which(membership==i)))
	V(cg)$x <- sapply(coms, function(i) mean(V(g)$x[membership==i]))
	V(cg)$y <- sapply(coms, function(i) mean(V(g)$y[membership==i]))
	cg2 <- cg
	
	# only one community
	if(length(unique(coms))==1)
	{	tlog(4,"Single community: nothing to do")
	}
	
	# several communities
	else
	{	#############################
		# deal with categorical attributes
		
		# gather regular categorical attributes
		attrs <- intersect(COL_CAT, vertex_attr_names(g))
		for(attr in attrs)
		{	# attribute folder
			attr.folder <- file.path(coms.folder, attr)
			dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
			
			# get values
			tmp <- vertex_attr(g, attr)
			
			# export community-wise distributions as csv
			tlog(4,"Exporting community-wise distribution for attribute \"",attr,"\"")
			tmp <- factor(tmp)
			tt <- t(sapply(coms, function(i) table(tmp[membership==i], useNA="always"), simplify="array"))
			colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
			if(nrow(tt)==1 && ncol(tt)>1)
			{	tt <- t(tt)
				colnames(tt)[1] <- "NA"
				rownames(tt) <- NULL
			}
			tab <- as.data.frame(tt)
			tab <- cbind(coms, tab)
			colnames(tab)[1] <- "Community"
			tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
			
			# plot as graph with pie-charts as nodes
			tlog(4,"Plotting community graph with the distribution of \"",attr,"\"")
			for(c in 1:ncol(tt))
				cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
			plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
			V(cg2)$label <- rep(NA, gorder(cg2))
			#plot(cg2, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
			custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file)
			#custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
			
			# compute purity for each community
			pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
			tab <- as.data.frame(pur.tab)
			tab <- cbind(coms, tab)
			colnames(tab) <- c("Community", "Purity")
			tab.file <- file.path(attr.folder, paste0(attr,"_purity.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
			
			# compute global measures
			vals <- c()
			meas <- c()
				# purity
				pur.total <- sum(rowSums(tt)/gorder(g)*pur.tab)
				vals <- c(vals, pur.total)
				meas <- c(meas, "Purity")
				# chi-squared test of independence (dpt if p<0.05)
				if(all(is.na(tmp)) || length(unique(tmp))==1 || any(is.na(tmp)) && length(unique(tmp))==2)
					chisq <- NA
				else
					chisq <- suppressWarnings(chisq.test(tmp, membership, correct=FALSE))$p.value # warning=communities too small
				vals <- c(vals, chisq)
				meas <- c(meas, "Chi2_pval")
				# Cramér's V
				if(all(is.na(tmp)) || length(unique(tmp))==1 || any(is.na(tmp)) && length(unique(tmp))==2)
					cram <- NA
				else
					cram <- CramerV(x=tmp, y=membership)
				vals <- c(vals, cram)
				meas <- c(meas, "C_V")
				# Goodman’s Kruskal Tau
				tau <- GKtau(membership, tmp)
				vals <- c(vals, tau$tauxy, tau$tauyx)
				meas <- c(meas, "GK_tau_Com->Att", "GK_tau_Att->Com")
			# record as a table
			tab <- data.frame(meas, vals)
			colnames(tab) <- c("Measure","Value")
			tab.file <- file.path(attr.folder, paste0(attr,"_association.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
		}
		
		# convert tag-type attributes
		attrs <- intersect(names(COL_TAG), vertex_attr_names(g))
		for(attr in attrs)
		{	tlog(4,"Processing attribute-tag \"",attr,"\"")
			
			# attribute folder
			attr.folder <- file.path(coms.folder, attr)
			dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
			
			# compute values
			attrc <- intersect(COL_TAG[[attr]], vertex_attr_names(g))
			m <- sapply(attrc, function(att) vertex_attr(g, att))
			idx.nas <- which(apply(m,1,function(r) all(is.na(r))))	# detect individuals with only NAs
			uvals <- sort(unique(c(m)))
			
			# processing all values at once
			tlog(6,"Exporting community-wise distribution for attribute-tag \"",attr,"\"")
			all.nas <- apply(m, 1, function(row) all(is.na(row)))
			tt <- t(sapply(coms, function(i) 
			{	idx <- which(membership==i & !all.nas)
				tmp <- factor(c(m[idx,], rep(NA, length(which(membership==i & all.nas)))), levels=uvals)
				table(tmp, useNA="always")
			}))
			colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
			if(nrow(tt)==1 && ncol(tt)>1)
			{	tt <- t(tt)
				colnames(tt)[1] <- "NA"
				rownames(tt) <- NULL
			}
			tab <- as.data.frame(tt)
			tab <- cbind(coms, tab)
			colnames(tab)[1] <- "Community"
			tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
			
			# plot as graph with pie-charts as nodes
			tlog(6,"Plotting community graph with the distribution of \"",attr,"\"")
			for(c in 1:ncol(tt))
				cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
			plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
			V(cg2)$label <- rep(NA, gorder(cg2))
			#plot(cg2, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
			custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file)
			#custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
			
			# processing each value separately
			for(uval in uvals)
			{	# binarize tags
				tmp <- apply(m, 1, function(v) uval %in% v[!is.na(v)])
				idxt <- which(tmp)
				idxf <- which(!tmp)
				tmp[idxt] <- VAL_TRUE
				tmp[idxf] <- VAL_FALSE
				tmp[idx.nas] <- NA
				
				# setup folder
				short_val <- trimws(substr(uval,1,30))
				attr_val <- paste0(attr,"_",short_val)
				attrval.folder <- file.path(attr.folder, short_val)
				dir.create(path=attrval.folder, showWarnings=FALSE, recursive=TRUE)
				
				# export community-wise distributions as csv
				tlog(6,"Exporting community-wise distribution for attribute-value \"",attr_val,"\"")
				tmp <- factor(tmp)
				tt <- t(sapply(coms, function(i) table(tmp[membership==i], useNA="always")))
				colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
				if(nrow(tt)==1 && ncol(tt)>1)
				{	tt <- t(tt)
					colnames(tt)[1] <- "NA"
					rownames(tt) <- NULL
				}
				tab <- as.data.frame(tt)
				tab <- cbind(coms, tab)
				colnames(tab)[1] <- "Community"
				tab.file <- file.path(attrval.folder, paste0("distribution.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# plot as graph with pie-charts as nodes
				tlog(6,"Plotting community graph with the distribution of \"",attr_val,"\"")
				for(c in 1:ncol(tt))
					cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
				plot.file <- file.path(attrval.folder, paste0("comgraph"))
				V(cg2)$label <- rep(NA, gorder(cg2))
				#plot(cg2, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
				custom.gplot(cg2, col.att=colnames(tt), col.att.cap=paste0(attr," : ",uval), size.att="size", cat.att=TRUE, file=plot.file)
				#custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr_val, size.att="size", cat.att=TRUE)
				
				# compute purity for each community
				pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
				tab <- as.data.frame(pur.tab)
				tab <- cbind(coms, tab)
				colnames(tab) <- c("Community", "Purity")
				tab.file <- file.path(attrval.folder, paste0("purity.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# compute global measures
				vals <- c()
				meas <- c()
				# purity
				pur.total <- sum(rowSums(tt)/gorder(g)*pur.tab)
				vals <- c(vals, pur.total)
				meas <- c(meas, "Purity")
				# chi-squared test of independence (dpt if p<0.05)
				chisq <- suppressWarnings(chisq.test(tmp, membership, correct=FALSE))$p.value # warning=communities too small
				vals <- c(vals, chisq)
				meas <- c(meas, "Chi2_pval")
				# Cramér's V
				cram <- CramerV(x=tmp, y=membership)
				vals <- c(vals, cram)
				meas <- c(meas, "C_V")
				# Goodman’s Kruskal Tau
				tau <- GKtau(membership, tmp)
				vals <- c(vals, tau$tauxy, tau$tauyx)
				meas <- c(meas, "GK_tau_Com->Att", "GK_tau_Att->Com")
				# record as a table
				tab <- data.frame(meas, vals)
				colnames(tab) <- c("Measure","Value")
				tab.file <- file.path(attrval.folder, paste0("association.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
			}
		}
		
		#############################
		# deal with numerical attributes
		
		# gather regular numerical attributes
		attrs <- intersect(COL_NUM, vertex_attr_names(g))
		for(attr in attrs)
		{	tlog(4,"Processing attribute \"",attr,"\"")
			
			# attribute folder
			attr.folder <- file.path(coms.folder, attr)
			dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
			
			# get values
			att.vals <- vertex_attr(g, attr)
			# only NAs, nothing to do
			if(all(is.na(att.vals)))
				tlog(4,"Only NAs: nothing to do")
			# non-NA values
			else
			{	# exports basic stats for each community
				tab <- t(sapply(coms, function(i) quantile(att.vals[membership==i], na.rm=TRUE)))
				tab <- cbind(tab, sapply(coms, function(i) mean(att.vals[membership==i])))
				colnames(tab)[ncol(tab)] <- "Mean"
				tab <- cbind(tab, sapply(coms, function(i) sd(att.vals[membership==i], na.rm=TRUE)))
				colnames(tab)[ncol(tab)] <- "Stdev"
				tab <- cbind(coms, tab)
				colnames(tab)[1] <- "Community"
				tab.file <- file.path(attr.folder, paste0(attr,"_stats.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# export community-wise distributions as csv
				tlog(6,"Exporting community-wise distribution for attribute \"",attr,"\"")
				tmp <- cut(att.vals, 5)
				tt <- t(sapply(coms, function(i) table(tmp[membership==i], useNA="always")))
				colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
				tab <- as.data.frame(tt)
				tab <- cbind(coms, tab)
				colnames(tab)[1] <- "Community"
				tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# plot as graph with pie-charts as nodes
				tlog(6,"Plotting community graph with the distribution of \"",attr,"\"")
				for(c in 1:ncol(tt))
					cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
				plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
				V(cg2)$label <- rep(NA, gorder(cg2))
				#plot(cg, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
				custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file)
				#custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
			
#				# compute purity for each community
#				pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
#				tab <- as.data.frame(pur.tab)
#				tab <- cbind(coms, tab)
#				colnames(tab) <- c("Community", "Purity")
#				tab.file <- file.path(attr.folder, paste0(attr,"_purity.csv"))
#				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# compute global measures
				vals <- c()
				meas <- c()
					# anova
					if(length(unique(membership[!is.na(membership)]))<2 || length(unique(membership[!is.na(att.vals)]))<2)
						pval <- NA
					else
					{	fit <- suppressWarnings(aov(att.vals[!is.na(att.vals)]~as.factor(membership[!is.na(att.vals)])))	# warning=perfect fit
						pval <- summary(fit)[[1]][["Pr(>F)"]][1]	# dirty workaround to get the p-value, see https://stackoverflow.com/questions/3366506/extract-p-value-from-aov
					}
					vals <- c(vals, pval)
					meas <- c(meas, "ANOVA_pval")
					# eta 
					etas <- suppressWarnings(eta_sq(fit)$etasq)	# warning=perfect fit
					vals <- c(vals, etas)
					meas <- c(meas, "Eta")
				# record as a table
				tab <- data.frame(meas, vals)
				colnames(tab) <- c("Measure","Value")
				tab.file <- file.path(attr.folder, paste0(attr,"_association.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
			}
		}
		
		#############################
		# compute nodal topological measures for each community
		tlog(4,"Average nodal topological measures for each community")
		meas.list <- nodal.atts[startsWith(nodal.atts,"_")]
		meas.starts <- c(MEAS_ARTICULATION, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_CLOSENESS_HARMO, MEAS_CONNECTIVITY, MEAS_DEGREE, MEAS_ECCENTRICITY, MEAS_EIGENCENTR, MEAS_TRANSITIVITY)
		
		# get the list of measures already processed before
		meass <- c()
		for(meas.start in meas.starts)
			meass <- c(meass, startsWith(meas.list, meas.start))
		
		# init stat table
		tab <- matrix(coms, ncol=1)
		colnames(tab)[ncol(tab)] <- "Community"
		
		# compute average/stdev for each community
		for(meas in meass)
		{	# average
			avg <- sapply(coms, function(i) mean(vertex_attr(graph=g, name=meas, index=which(membership==i)), na.rm=TRUE))
			mn <- paste0(meas,"_avg")
			cg <- set_vertex_attr(graph=cg, name=mn, value=avg)
			tab <- cbind(tab, avg); colnames(tab)[ncol(tab)] <- mn 
			# standard-deviation
			stdev <- sapply(coms, function(i) sd(vertex_attr(graph=g, name=meas, index=which(membership==i)), na.rm=TRUE))
			mn <- paste0(meas,"_stdv")
			cg <- set_vertex_attr(graph=cg, name=mn, value=stdev)
			tab <- cbind(tab, stdev); colnames(tab)[ncol(tab)] <- mn 
		}
		
		# measures that need to be computed again (by community, this time)
		tlog(4,"Compute certain nodal topological measures for each community")
		for(i in 1:length(coms))
		{	com <- coms[i]
			gcom <- induced_subgraph(graph=g, vids=which(membership==com))
			
			# TODO assortativity by community
			#gcom <- analyze.net.assortativity(gcom)
			
			# number of nodes
			meas <- MEAS_NBR_NODES
			if(!(meas %in% colnames(tab)))
			{	tab <- cbind(tab, rep(NA,nrow(tab)))
				colnames(tab)[ncol(tab)] <- meas
			}			
			tab[i,meas] <- gorder(gcom)
			
			# number of links
			meas <- MEAS_NBR_LINKS
			if(!(meas %in% colnames(tab)))
			{	tab <- cbind(tab, rep(NA,nrow(tab)))
				colnames(tab)[ncol(tab)] <- meas
			}			
			tab[i,meas] <- gsize(gcom)
			
			modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
			for(mode in modes)
			{	# number of components
				meas <- paste(MEAS_COMPONENTS,"_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				tab[i,meas] <- components(graph=gcom, mode=if(mode==MEAS_MODE_UNDIR) "weak" else "strong")$no
	
				# diameter
				meas <- paste(MEAS_DIAMETER,"_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				tab[i,meas] <- diameter(gcom, directed=mode==MEAS_MODE_DIR)
			}
			
			modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
			for(mode in modes)
			{	# radius
				meas <- paste(MEAS_RADIUS,"_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				tab[i,meas] <- radius(g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
			}
		}		
		
		# TODO add community-specific measures
		
		# record stat table
		tab.file <- file.path(coms.folder, "stats.csv")
		write.csv(tab, file=tab.file, row.names=FALSE)
	}
	
	#############################
	# community attributes over
	
	# record community graph
	graph.file <- file.path(coms.folder, "comgraph.graphml")
	write.graphml.file(g=cg, file=graph.file)
	
	return(g)
}
