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
	
	# community detection algorithms
	algos <- list()
	algos[["edgebetweenness"]] <- list(
		fun=function(g, mode) 
			cluster_edge_betweenness(graph=g,
					weights=NULL,
					directed=mode==MEAS_MODE_DIR,
					edge.betweenness=FALSE,
					merges=FALSE,
					bridges=FALSE,
					modularity=TRUE,
					membership=TRUE),
		modes=c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	)
	algos[["fastgreedy"]] <- list(
		fun=function(g, mode) 
			cluster_fast_greedy(graph=as.undirected(g),
					merges=FALSE,
					modularity=TRUE,
					membership=TRUE,
					weights=NULL),
		modes=c(MEAS_MODE_UNDIR)
	)
	algos[["infomap"]] <- list(
			fun=function(g, mode) 
				cluster_infomap(graph=if(mode==MEAS_MODE_UNDIR) as.undirected(g) else g,
					e.weights=NULL,
					v.weights=NULL,
					nb.trials=10,
					modularity=TRUE),
			modes=c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	)
	algos[["labelprop"]] <- list(
			fun=function(g, mode) 
				cluster_label_prop(graph=as.undirected(g),
					weights=NA, 
					initial=NULL, 
					fixed=NULL),
			modes=c(MEAS_MODE_UNDIR)
	)
	algos[["leadingeigen"]] <- list(
			fun=function(g, mode) 
				cluster_leading_eigen(graph=as.undirected(g),
						weights=NULL,
						start=NULL,
						options=list(maxiter=1000000)),
			modes=c(MEAS_MODE_UNDIR)
	)
	algos[["louvain"]] <- list(
			fun=function(g, mode) 
				cluster_louvain(graph=as.undirected(g),
						weights=NULL),
			modes=c(MEAS_MODE_UNDIR)
	)
#	algos[["spinglass"]] <- list(	# does not work with disconnected graphs
#			fun=function(g, mode) 
#				cluster_spinglass(graph=if(mode==MEAS_MODE_UNDIR) as.undirected(g) else g,
#						weights=NA),
#			modes=c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
#	)
	algos[["walktrap"]] <- list(
			fun=function(g, mode) 
				cluster_walktrap(graph=if(mode==MEAS_MODE_UNDIR) as.undirected(g) else g,
						weights=NULL,
						steps=4,
						merges=TRUE,
						modularity=TRUE,
						membership=TRUE),
			modes=c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	)
	
	tab.memb <- NA
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Detecting community structure: mode=",mode)
		
		# apply each appropriate algo
		for(a in 1:length(algos))
		{	algo.name <- names(algos)[a]
			tlog(4,"Dealing with algorithm ",algo.name)
			
			if(mode %in% algos[[a]]$modes)
			{	# possibly create folder
				fname <- paste0("coms_",mode,"_",algo.name)
				coms.folder <- file.path(out.folder, g$name, MEAS_COMMUNITIES, mode, algo.name)
				dir.create(path=coms.folder, showWarnings=FALSE, recursive=TRUE)
				
				# detect communities
				##coms <- cluster_optimal(graph=simplify(g))		# much slower, obviously
				##coms <- cluster_spinglass(graph=simplify(g))
				##coms <- cluster_infomap(graph=simplify(g))
				#coms <- cluster_edge_betweenness(graph=simplify(g), directed=mode)
				coms <- algos[[a]]$fun(simplify(g), mode)
				mod <- modularity(coms)
				mbrs <- as.integer(membership(coms))
				com.nbr <- length(unique(mbrs))
				tlog(6,"Number of communities: ",com.nbr)
				tlog(6,"Modularity: ",mod)
				
				# add to main table
				if(all(is.na(tab.memb)))
					tab.memb <- matrix(mbrs,ncol=1)
				else
					tab.memb <- cbind(tab.memb, mbrs)
				colnames(tab.memb)[ncol(tab.memb)] <- paste0(algo.name,"_",mode)
				
				# community size distribution
				sizes <- table(mbrs,useNA="ifany")
				custom.barplot(sizes, text=names(sizes), xlab="Community", ylab="Size", file=file.path(coms.folder,paste0(fname,"_size_bars")))
				
				# export CSV with community membership
				df <- data.frame(vertex_attr(g, ND_NAME), get.names(g), mbrs)
				colnames(df) <- c("Id","Name","Community") 
				write.csv(df, file=file.path(coms.folder,paste0(fname,"_membership.csv")), row.names=FALSE)
				
				# add results to the graph (as attributes) and stats table
				g <- set_vertex_attr(graph=g, name=fname, value=mbrs)
				g <- set_graph_attr(graph=g, name=paste0(fname,"_nbr"), value=com.nbr)
				g <- set_graph_attr(graph=g, name=paste0(fname,"_mod"), value=mod)
				stats[paste0(fname,"_nbr"), ] <- list(Value=com.nbr, Mean=NA, Stdv=NA)
				stats[paste0(fname,"_mod"), ] <- list(Value=mod, Mean=NA, Stdv=NA)
				
				# plot graph using color for communities
				#V(g)$label <- rep(NA, gorder(g))
				V(g)$label <- paste(vertex_attr(g,name=COL_LOC_ID), get.location.names(g),sep="_")
				g1 <- g; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=fname,cat.att=TRUE, file=file.path(coms.folder,paste0(fname,"_graph_lambert")), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
				g1 <- g; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=fname,cat.att=TRUE, file=file.path(coms.folder,paste0(fname,"_graph_kk")), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1)
				
				# assess community purity for all attributes
				g <- analyze.net.comstruct.attributes(g=g, coms.folder=coms.folder, membership=mbrs)
			}
		}
	}
	
	# export CSV stat file
	write.csv(stats, file=stat.file, row.names=TRUE)
	
	# compare community structures
	score.nmi <- matrix(NA,nrow=ncol(tab.memb),ncol=ncol(tab.memb))
	rownames(score.nmi) <- colnames(score.nmi) <- colnames(tab.memb)
	score.ri <- matrix(NA,nrow=ncol(tab.memb),ncol=ncol(tab.memb))
	rownames(score.ri) <- colnames(score.ri) <- colnames(tab.memb)
	score.ari <- matrix(NA,nrow=ncol(tab.memb),ncol=ncol(tab.memb))
	rownames(score.ari) <- colnames(score.ari) <- colnames(tab.memb)
	for(c1 in 1:(ncol(tab.memb)-1))
	{	for(c2 in (c1+1):ncol(tab.memb))
		{	score.nmi[c1,c2] <- score.nmi[c2,c1] <- compare(comm1=tab.memb[,c1], comm2=tab.memb[,c2], method="nmi")
			score.ri[c1,c2]  <- score.nmi[c2,c1] <- compare(comm1=tab.memb[,c1], comm2=tab.memb[,c2], method="rand")
			score.ari[c1,c2] <- score.nmi[c2,c1] <- compare(comm1=tab.memb[,c1], comm2=tab.memb[,c2], method="adjusted.rand")
		}
	}
	# record the resulting similarity matrices
	tab.file <- file.path(out.folder, g$name, MEAS_COMMUNITIES, "comparison_nmi.csv")
	write.csv(score.nmi, file=tab.file, row.names=TRUE)
	tab.file <- file.path(out.folder, g$name, MEAS_COMMUNITIES, "comparison_ri.csv")
	write.csv(score.ri, file=tab.file, row.names=TRUE)
	tab.file <- file.path(out.folder, g$name, MEAS_COMMUNITIES, "comparison_ari.csv")
	write.csv(score.ari, file=tab.file, row.names=TRUE)
	
	# compare community structures with certain attributes
	atts <- c(COL_EST_AREA_ID, COL_EST_VILLAGE_ID, COL_EST_LORDSHIP_ID)
	score.nmi <- matrix(NA,nrow=ncol(tab.memb),ncol=length(atts))
	rownames(score.nmi) <- colnames(tab.memb)
	colnames(score.nmi) <- atts
	score.ri <- matrix(NA,nrow=ncol(tab.memb),ncol=length(atts))
	rownames(score.ri) <- colnames(tab.memb)
	colnames(score.ri) <- atts
	score.ari <- matrix(NA,nrow=ncol(tab.memb),ncol=length(atts))
	rownames(score.ari) <- colnames(tab.memb)
	colnames(score.ari) <- atts
	for(att in atts)
	{	att.mbrs <- as.integer(as.factor(vertex_attr(graph=g, name=att)))
		for(c in 1:ncol(tab.memb))
		{	score.nmi[c,att] <- compare(comm1=tab.memb[!is.na(att.mbrs),c], comm2=att.mbrs[!is.na(att.mbrs)], method="nmi")
			score.ri[c,att]  <- compare(comm1=tab.memb[!is.na(att.mbrs),c], comm2=att.mbrs[!is.na(att.mbrs)], method="rand")
			score.ari[c,att] <- compare(comm1=tab.memb[!is.na(att.mbrs),c], comm2=att.mbrs[!is.na(att.mbrs)], method="adjusted.rand")
		}
	}
	# record the resulting similarity matrices
	tab.file <- file.path(out.folder, g$name, MEAS_COMMUNITIES, paste0("attributes_nmi.csv"))
	write.csv(score.nmi, file=tab.file, row.names=TRUE)
	tab.file <- file.path(out.folder, g$name, MEAS_COMMUNITIES, paste0("attributes_ri.csv"))
	write.csv(score.ri, file=tab.file, row.names=TRUE)
	tab.file <- file.path(out.folder, g$name, MEAS_COMMUNITIES, paste0("attributes_ari.csv"))
	write.csv(score.ari, file=tab.file, row.names=TRUE)
	
	# record graph and return it
	graph.file <- file.path(out.folder, g$name, FILE_GRAPH)
	write.graphml.file(g=g, file=graph.file)
	return(g)
}




#############################################################
# Compares the previously detected groupes (communites, components)
# and the attributes of the nodes.
#
# g: original graph to process.
# coms.folder: folder to write the group-related files.
# membership: group membership vector.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.comstruct.attributes <- function(g, coms.folder, membership)
{	tlog(2,"Comparing nodal attributes and groups")
	
	# indices of real estate vertices
	est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)=="Bien")
	non.est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)!="Bien")
	
	# retrieve the list of vertex attributes
	nodal.atts <- list.vertex.attributes(g)
	att.list <- nodal.atts[!startsWith(nodal.atts,"_")]
	
	# retrieve group membership
	coms <- sort(unique(membership))
	
	# build group graph
	cg <- contract.vertices(g, mapping=membership)	# TODO we could keep edges of different types separted (familial, professional, etc.)
	E(cg)$weight <- 1
	cg <- simplify(cg, remove.loops=TRUE)	# keeping the loops makes the plot difficult to read
	for(eattr in edge_attr_names(graph=cg))
		cg <- delete_edge_attr(graph=cg, name=eattr)
	# setup its attributes
	V(cg)$name <- paste("C",coms,sep="")
	V(cg)$label <- paste("C",coms,sep="")
	V(cg)$size <- sapply(coms, function(i) length(which(membership==i)))
	V(cg)$x <- sapply(coms, function(i) mean(V(g)$x[membership==i]))
	V(cg)$y <- sapply(coms, function(i) mean(V(g)$y[membership==i]))
	cg2 <- cg
	
	# only one group
	if(length(unique(coms))==1)
	{	tlog(4,"Single group: nothing to do")
	}
	
	# several groups
	else
	{	#############################
		# deal with categorical attributes
		
		# gather regular categorical attributes
		attrs <- intersect(COL_CAT_SELECT, vertex_attr_names(g))
		for(attr in attrs)
		{	# attribute folder
			attr.folder <- file.path(coms.folder, attr)
			dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
			
			# get values only for real-estate vertices
			g0 <- delete_vertices(graph=g, v=non.est.idx)
			tmp <- vertex_attr(g0, attr)
			
			# only NAs, nothing to do
			if(all(is.na(tmp)))
				tlog(4,"Only NAs: nothing to do (",attr,")")
			# non-NA values
			else
			{	# export group-wise distributions as csv
				tlog(4,"Exporting group-wise distribution for categorical attribute \"",attr,"\"")
				tmp <- factor(tmp)
				tt <- t(sapply(coms, function(i) table(tmp[membership[est.idx]==i], useNA="no"), simplify="array"))
				colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
				if(nrow(tt)==1 && ncol(tt)>1)
				{	tt <- t(tt)
					colnames(tt)[1] <- "NA"
					rownames(tt) <- NULL
				}
				tab <- as.data.frame(tt)
				tab <- cbind(coms, tab)
				colnames(tab)[1] <- "Group"
				tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# plot as graph with pie-charts as nodes
				tlog(4,"Plotting group graph with the distribution of \"",attr,"\"")
				for(c in 1:ncol(tt))
					cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
				plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
				V(cg2)$label <- rep(NA, gorder(cg2))
				#plot(cg2, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
				custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file, color.isolates=TRUE)
				#custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
				
				# compute group purity for each group
				grp.pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
				tab <- as.data.frame(grp.pur.tab)
				tab <- cbind(coms, tab)
				colnames(tab) <- c("Group", "GrpPurity")
				tab.file <- file.path(attr.folder, paste0(attr,"_grp-purity.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				# compute attribute purity for each attribute value (ignoring NAs)
				if(ncol(tt)>1)
				{	att.pur.tab <- apply(tt[,colnames(tt)!="NA",drop=FALSE], 2, function(col) max(col)/sum(col))
					tab <- as.data.frame(att.pur.tab)
					tab <- cbind(colnames(tt[,colnames(tt)!="NA",drop=FALSE]), tab)
					colnames(tab) <- c("Value", "ValPurity")
					rownames(tab) <- NULL
					tab.file <- file.path(attr.folder, paste0(attr,"_val-purity.csv"))
					write.csv(tab, file=tab.file, row.names=FALSE)
				}
				
				# compute global measures
				vals <- c()
				meas <- c()
					# purity measures
					grp.pur.total <- sum(rowSums(tt)/gorder(g0)*grp.pur.tab)
					att.pur.total <- sum(colSums(tt[,colnames(tt)!="NA",drop=FALSE])/gorder(g0)*att.pur.tab)
					vals <- c(vals, grp.pur.total, att.pur.total)
					meas <- c(meas, "GrpPurity", "ValPurity")
					# chi-squared test of independence (dpt if p<0.05)
					if(all(is.na(tmp)) || length(unique(tmp))==1 || any(is.na(tmp)) && length(unique(tmp))==2)
						chisq <- NA
					else
						chisq <- suppressWarnings(chisq.test(tmp, membership[est.idx], correct=FALSE))$p.value # warning=groups too small
					vals <- c(vals, chisq)
					meas <- c(meas, "Chi2_pval")
					# Cramér's V
					if(all(is.na(tmp)) || length(unique(tmp))==1 || any(is.na(tmp)) && length(unique(tmp))==2)
						cram <- NA
					else
						cram <- CramerV(x=tmp, y=membership[est.idx])
					vals <- c(vals, cram)
					meas <- c(meas, "C_V")
					# Goodman’s Kruskal Tau
					tau <- GKtau(membership[est.idx], tmp)
					vals <- c(vals, tau$tauxy, tau$tauyx)
					meas <- c(meas, "GK_tau_Com->Att", "GK_tau_Att->Com")
				# record as a table
				tab <- data.frame(meas, vals)
				colnames(tab) <- c("Measure","Value")
				tab.file <- file.path(attr.folder, paste0(attr,"_association.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
			}
		}
		
		# convert tag-type attributes
		attrs <- intersect(names(COL_TAG_SELECT), vertex_attr_names(g))
		for(attr in attrs)
		{	tlog(4,"Processing attribute-tag \"",attr,"\"")
			
			# get values only for real-estate vertices
			g0 <- delete_vertices(graph=g, v=non.est.idx)
			
			# attribute folder
			attr.folder <- file.path(coms.folder, attr)
			dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
			
			# compute values
			attrc <- intersect(COL_TAG_SELECT[[attr]], vertex_attr_names(g))
			m <- sapply(attrc, function(att) vertex_attr(g0, att))
			idx.nas <- which(apply(m,1,function(r) all(is.na(r))))	# detect individuals with only NAs
			uvals <- sort(unique(c(m)))
			
			# processing all values at once
			tlog(6,"Exporting group-wise distribution for attribute-tag \"",attr,"\"")
			all.nas <- apply(m, 1, function(row) all(is.na(row)))
			tt <- t(sapply(coms, function(i) 
			{	idx <- which(membership[est.idx]==i & !all.nas)
				tmp <- factor(c(m[idx,], rep(NA, length(which(membership[est.idx]==i & all.nas)))), levels=uvals)
				table(tmp, useNA="no")
			}))
			colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
			if(nrow(tt)==1 && ncol(tt)>1)
			{	tt <- t(tt)
				colnames(tt)[1] <- "NA"
				rownames(tt) <- NULL
			}
			tab <- as.data.frame(tt)
			tab <- cbind(coms, tab)
			colnames(tab)[1] <- "Group"
			tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
			
			# plot as graph with pie-charts as nodes
			tlog(6,"Plotting group graph with the distribution of \"",attr,"\"")
			for(c in 1:ncol(tt))
				cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
			plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
			V(cg2)$label <- rep(NA, gorder(cg2))
			#plot(cg2, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
			custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file, color.isolates=TRUE)
			#custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
			
			# compute group purity for each group
			grp.pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
			tab <- as.data.frame(grp.pur.tab)
			tab <- cbind(coms, tab)
			colnames(tab) <- c("Group", "GrpPurity")
			tab.file <- file.path(attr.folder, paste0(attr,"_grp-purity.csv"))
			write.csv(tab, file=tab.file, row.names=FALSE)
			# compute attribute purity for each attribute value (ignoring NAs)
			if(ncol(tt)>1)
			{	att.pur.tab <- apply(tt[,colnames(tt)!="NA",drop=FALSE], 2, function(col) max(col)/sum(col))
				tab <- as.data.frame(att.pur.tab)
				tab <- cbind(colnames(tt[,colnames(tt)!="NA",drop=FALSE]), tab)
				colnames(tab) <- c("Value", "ValPurity")
				rownames(tab) <- NULL
				tab.file <- file.path(attr.folder, paste0(attr,"_val-purity.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
			}
			
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
				
				# export group-wise distributions as csv
				tlog(6,"Exporting group-wise distribution for attribute-value \"",attr_val,"\"")
				tmp <- factor(tmp)
				tt <- t(sapply(coms, function(i) table(tmp[membership[est.idx]==i], useNA="always")))
				colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
				if(nrow(tt)==1 && ncol(tt)>1)
				{	tt <- t(tt)
					colnames(tt)[1] <- "NA"
					rownames(tt) <- NULL
				}
				tab <- as.data.frame(tt)
				tab <- cbind(coms, tab)
				colnames(tab)[1] <- "Group"
				tab.file <- file.path(attrval.folder, paste0("distribution.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# plot as graph with pie-charts as nodes
				tlog(6,"Plotting group graph with the distribution of \"",attr_val,"\"")
				for(c in 1:ncol(tt))
					cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
				plot.file <- file.path(attrval.folder, paste0("comgraph"))
				V(cg2)$label <- rep(NA, gorder(cg2))
				#plot(cg2, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
				custom.gplot(cg2, col.att=colnames(tt), col.att.cap=paste0(attr," : ",uval), size.att="size", cat.att=TRUE, file=plot.file, color.isolates=TRUE)
				#custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr_val, size.att="size", cat.att=TRUE)
				
				# compute group purity for each group
				grp.pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
				tab <- as.data.frame(grp.pur.tab)
				tab <- cbind(coms, tab)
				colnames(tab) <- c("Group", "GrpPurity")
				tab.file <- file.path(attrval.folder, paste0(attr,"_grp-purity.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				# compute attribute purity for each attribute value (ignoring NAs)
				if(ncol(tt)>1)
				{	att.pur.tab <- apply(tt[,colnames(tt)!="NA",drop=FALSE], 2, function(col) max(col)/sum(col))
					tab <- as.data.frame(att.pur.tab)
					tab <- cbind(colnames(tt[,colnames(tt)!="NA",drop=FALSE]), tab)
					colnames(tab) <- c("Value", "ValPurity")
					rownames(tab) <- NULL
					tab.file <- file.path(attrval.folder, paste0(attr,"_val-purity.csv"))
					write.csv(tab, file=tab.file, row.names=FALSE)
				}
				
				# compute global measures
				vals <- c()
				meas <- c()
				# purity measures
				grp.pur.total <- sum(rowSums(tt)/gorder(g0)*grp.pur.tab)
				att.pur.total <- sum(colSums(tt[,colnames(tt)!="NA",drop=FALSE])/gorder(g0)*att.pur.tab)
				vals <- c(vals, grp.pur.total, att.pur.total)
				meas <- c(meas, "GrpPurity", "ValPurity")
				# chi-squared test of independence (dpt if p<0.05)
				chisq <- suppressWarnings(chisq.test(tmp, membership[est.idx], correct=FALSE))$p.value # warning=groups too small
				vals <- c(vals, chisq)
				meas <- c(meas, "Chi2_pval")
				# Cramér's V
				cram <- CramerV(x=tmp, y=membership[est.idx])
				vals <- c(vals, cram)
				meas <- c(meas, "C_V")
				# Goodman’s Kruskal Tau
				tau <- GKtau(membership[est.idx], tmp)
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
		attrs <- intersect(COL_NUM_SELECT, vertex_attr_names(g))
		for(attr in attrs)
		{	tlog(4,"Processing attribute \"",attr,"\"")
			
			# attribute folder
			attr.folder <- file.path(coms.folder, attr)
			dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
			
			# get values only for real-estate vertices
			g0 <- delete_vertices(graph=g, v=non.est.idx)
			att.vals <- vertex_attr(g0, attr)
			
			# only NAs, nothing to do
			if(all(is.na(att.vals)))
				tlog(4,"Only NAs: nothing to do")
			# non-NA values
			else
			{	# exports basic stats for each group
				tab <- t(sapply(coms, function(i) quantile(att.vals[membership[est.idx]==i], na.rm=TRUE)))
				tab <- cbind(tab, sapply(coms, function(i) mean(att.vals[membership[est.idx]==i])))
				colnames(tab)[ncol(tab)] <- "Mean"
				tab <- cbind(tab, sapply(coms, function(i) sd(att.vals[membership[est.idx]==i], na.rm=TRUE)))
				colnames(tab)[ncol(tab)] <- "Stdev"
				tab <- cbind(coms, tab)
				colnames(tab)[1] <- "Group"
				tab.file <- file.path(attr.folder, paste0(attr,"_stats.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# export group-wise distributions as csv
				tlog(6,"Exporting group-wise distribution for numerical attribute \"",attr,"\"")
				if(length(unique(att.vals[!is.na(att.vals)]))==1)
				{	tmp<-factor(att.vals)
					tt <- t(sapply(coms, function(i) table(tmp[membership[est.idx]==i], useNA="always")))
				}
				else	
				{	qt <- seq(from=0, to=1, by=0.2)
					quant <- quantile(att.vals, probs=qt, na.rm=TRUE)													# compute quantiles
					if(length(unique(quant))<length(qt))
						quant <- seq(from=min(att.vals,na.rm=T), to=max(att.vals,na.rm=T), length.out=length(qt))
					quant <- cbind(quant[1:(length(quant)-1)], quant[2:length(quant)])									# build intervales
					rownames(quant) <- NULL
					prec <- 2
					goOn <- TRUE
					while(goOn) 
					{	qlabs <- apply(quant, 1, function(row) paste0("]",sprintf(paste0("%.",prec,"f"),row[1]),";",sprintf(paste0("%.",prec,"f"),row[2]),"]"))	# define labels
						prec <- prec + 1
						goOn <- length(unique(qlabs))<length(qlabs)
					}
					qlabs[1] <- paste0("[",substr(qlabs[1], start=2, stop=nchar(qlabs[1])))
					quant[1,1] <- quant[1,1] - 1
					tmp <- sapply(att.vals, function(val) if(is.na(val)) NA else which(quant[,1]<val & quant[,2]>=val)[1])
					tmp <- factor(qlabs[tmp], levels=qlabs)
					#tmp <- cut(att.vals, breaks=5, include.lowest=TRUE, dig.lab=2)										# old version, less control
					tt <- t(sapply(coms, function(i) table(tmp[membership[est.idx]==i], useNA="always")))
				}
				colnames(tt)[which(is.na(colnames(tt)))] <- "NA"
				tab <- as.data.frame(tt)
				tab <- cbind(coms, tab)
				colnames(tab)[1] <- "Group"
				tab.file <- file.path(attr.folder, paste0(attr,"_distribution.csv"))
				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# plot as graph with pie-charts as nodes
				tlog(6,"Plotting group graph with the distribution of \"",attr,"\"")
				for(c in 1:ncol(tt))
					cg2 <- set_vertex_attr(graph=cg2, name=colnames(tt)[c], value=tt[,c])
				plot.file <- file.path(attr.folder, paste0(attr,"_comgraph"))
				V(cg2)$label <- rep(NA, gorder(cg2))
				#plot(cg, vertex.shape="pie", vertex.pie=split(tt,1:nrow(tt)), vertex.pie.color=list(colors))
				custom.gplot(cg2, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE, file=plot.file, color.isolates=TRUE)
				#custom.gplot(cg, col.att=colnames(tt), col.att.cap=attr, size.att="size", cat.att=TRUE)
			
#				# compute purity for each group
#				pur.tab <- apply(tt, 1, function(row) max(row)/sum(row))
#				tab <- as.data.frame(pur.tab)
#				tab <- cbind(coms, tab)
#				colnames(tab) <- c("Group", "Purity")
#				tab.file <- file.path(attr.folder, paste0(attr,"_purity.csv"))
#				write.csv(tab, file=tab.file, row.names=FALSE)
				
				# compute global measures
				vals <- c()
				meas <- c()
					# anova
					if(length(unique(membership[!is.na(membership[est.idx])]))<2 || length(unique(membership[!is.na(att.vals)]))<2)
						pval <- NA
					else
					{	fit <- suppressWarnings(aov(att.vals[!is.na(att.vals)]~as.factor(membership[est.idx][!is.na(att.vals)])))	# warning=perfect fit
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
		# compute nodal topological measures for each group
		tlog(4,"Average nodal topological measures for each group")
		meas.list <- nodal.atts[startsWith(nodal.atts,"_")]
		meas.starts <- c(MEAS_ARTICULATION, MEAS_BETWEENNESS, MEAS_CLOSENESS, MEAS_CLOSENESS_HARMO, MEAS_CONNECTIVITY, MEAS_DEGREE, MEAS_ECCENTRICITY, MEAS_EIGENCENTR, MEAS_TRANSITIVITY)
		
		# get the list of measures already processed before
		meass <- c()
		for(meas.start in meas.starts)
			meass <- c(meass, startsWith(meas.list, meas.start))
		
		# init stat table
		tab <- matrix(coms, ncol=1)
		colnames(tab)[ncol(tab)] <- "Group"
		
		# compute average/stdev for each group
		for(meas in meass)
		{	# average
			avg <- sapply(coms, function(i) mean(vertex_attr(graph=g, name=meas, index=which(membership[est.idx]==i)), na.rm=TRUE))
			mn <- paste0(meas,"_avg")
			cg <- set_vertex_attr(graph=cg, name=mn, value=avg)
			tab <- cbind(tab, avg); colnames(tab)[ncol(tab)] <- mn 
			# standard-deviation
			stdev <- sapply(coms, function(i) sd(vertex_attr(graph=g, name=meas, index=which(membership[est.idx]==i)), na.rm=TRUE))
			mn <- paste0(meas,"_stdv")
			cg <- set_vertex_attr(graph=cg, name=mn, value=stdev)
			tab <- cbind(tab, stdev); colnames(tab)[ncol(tab)] <- mn 
		}
		
		# measures that need to be computed again (by group, this time)
		tlog(4,"Compute certain nodal topological measures for each group")
		for(i in 1:length(coms))
		{	com <- coms[i]
			gcom <- induced_subgraph(graph=g, vids=which(membership==com))
			
			# TODO assortativity by group
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
			
#			modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
			modes <- c(MEAS_MODE_UNDIR)
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
				
				# betwenness centralization
				meas <- paste(MEAS_BETWEENNESS,"_centralization_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				tab[i,meas] <- centr_betw(graph=gcom, directed=mode=="directed", normalized=TRUE)$centralization
				
				# eigenvector centralization
				meas <- paste(MEAS_EIGENCENTR,"_centralization_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				if(mode==MEAS_MODE_DIR && is_dag(gcom))
					tab[i,meas] <- 0
				else
					tab[i,meas] <- centr_eigen(graph=gcom, directed=mode==MEAS_MODE_DIR, scale=FALSE, normalized=TRUE)$centralization
			}
			
#			modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_IN, MEAS_MODE_OUT)
			modes <- c(MEAS_MODE_UNDIR)
			for(mode in modes)
			{	# radius
				meas <- paste(MEAS_RADIUS,"_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				tab[i,meas] <- radius(g, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode)
				
				# degree centralization
				meas <- paste(MEAS_DEGREE,"_centralization_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				tab[i,meas] <- centr_degree(graph=gcom, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, loops=TRUE, normalized=TRUE)$centralization
				
				# closeness centralization
				meas <- paste(MEAS_CLOSENESS,"_centralization_",mode)
				if(!(meas %in% colnames(tab)))
				{	tab <- cbind(tab, rep(NA,nrow(tab)))
					colnames(tab)[ncol(tab)] <- meas
				}			
				tab[i,meas] <- centr_clo(graph=gcom, mode=if(mode==MEAS_MODE_UNDIR) "all" else mode, normalized=TRUE)$centralization
			}
		}		
		
		# TODO add community-specific measures
		
		# record stat table
		tab.file <- file.path(coms.folder, "stats.csv")
		write.csv(tab, file=tab.file, row.names=FALSE)
	}
	
	#############################
	# group attributes over
	
	# record group graph
	graph.file <- file.path(coms.folder, "comgraph.graphml")
	write.graphml.file(g=cg, file=graph.file)
	
	return(g)
}




#############################################################
# Plots files exhibiting the communities detected on the estate level
# graph, but shown on the flat graph.
#############################################################
plot.comstruct.comparison <- function()
{	# considered community detection methods
	algo.names <- c(
		"edgebetweenness",
		"fastgreedy",
		"infomap",
		"labelprop",
		"leadingeigen",
		"louvain",
#		"spinglass",
		"walktrap"
	)
	
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	for(i in 1:length(modes))
	{	mode <- modes[i]
		tlog(2,"Dealing with mode=",mode)
		
		for(algo.name in algo.names)
		{	tlog(4,"Dealing with community detection method '",algo.name,"'")
			#algo.name <- "edgebetweenness"
			fname <- paste0("coms_",mode,"_",algo.name)
			
			for(filt in c(FALSE, TRUE))
			{	tlog(6,"Dealing with the ",if(filt) "" else "un"," filtered version of the networks")
				
				# load first graph
				file.path <- file.path(FOLDER_OUT_ANAL_EST, if(filt) paste0(LV_ESTATE,"_filtered") else LV_ESTATE, FILE_GRAPH)
				tlog(8,"Loading first graph '",file.path,"'")
				g1 <- load.graphml.file(file=file.path)
				coms.folder <- file.path(FOLDER_OUT_ANAL_EST, g1$name, MEAS_COMMUNITIES, mode, algo.name)
				coms <- read.csv(file=file.path(coms.folder,paste0(fname,"_membership.csv")))
				idx <- match(gsub("_part","",coms[,"Id"],fixed=TRUE), V(g1)$idExterne)
				g1 <- set_vertex_attr(graph=g1, name=fname, index=idx, value=coms[,"Community"])
				
				# load second graph
				file.path <- file.path(FOLDER_OUT_ANAL_EST, if(filt) paste0(LK_TYPE_FLATREL,"_filtered") else LK_TYPE_FLATREL, FILE_GRAPH)
				tlog(8,"Loading second graph '",file.path,"'")
				g2 <- load.graphml.file(file=file.path)
				
				# add g1 coms to g2
				coms <- rep(NA,gorder(g2))
				idx <- match(V(g1)$idExterne, V(g2)$idExterne)
				idx.u <- which(is.na(idx))
				idx.m <- which(!is.na(idx))
				coms[idx[idx.m]] <- vertex_attr(g1,fname)[idx.m]
				V(g2)$Coms <- coms
				
				# plot g2 with these coms
				coms.folder <- file.path(FOLDER_OUT_ANAL_EST, g2$name, MEAS_COMMUNITIES, mode, algo.name)
				plot.file <- file.path(coms.folder, paste0(fname,"_graph_comparison"))
				tlog(8,"Plotting graph in '",plot.file,"'")
				V(g2)$label <- paste(vertex_attr(g2,name=COL_LOC_ID), get.location.names(g2),sep="_")
				# lambert plot
				g2 <- delete_edge_attr(g2, LK_TYPE); g2 <- simplify(g2)
				custom.gplot(g=g2, col.att="Coms", cat.att=TRUE, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
				# kk plot
				V(g2)$x <- V(g2)$x2; V(g2)$y <- V(g2)$y2; E(g2)$weight <- 0.5
				custom.gplot(g=g2, col.att="Coms", cat.att=TRUE, file=paste0(plot.file,"_kk"), rescale=FALSE, xlim=range(V(g2)$x), ylim=range(V(g2)$y), edge.arrow.mode=0, vertex.label.cex=0.1)
			}
		}
	}
}
