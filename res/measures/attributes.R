#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#
# source("res/measures/attributes.R")
#############################################################################################




#############################################################
# measure name
MEAS_ATTRIBUTES <- "attributes"
MEAS_LONG_NAMES[MEAS_ATTRIBUTES] <- "Attributes"




#############################################################
# Computes stats related to the node attributes.
#
# g: original graph to process.
# out.folder: main output folder.
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.attributes <- function(g, out.folder, fast)
{	tlog(2,"Computing nodal attribute stats")
	# possibly create folders
	graph.folder <- file.path(out.folder, g$name)
	attr.folder <- file.path(graph.folder, MEAS_ATTRIBUTES)
	dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
	comp.folder <- file.path(attr.folder, "_comparison")
	dir.create(path=comp.folder, showWarnings=FALSE, recursive=TRUE)
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	# indices of real estate vertices
	est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)=="Bien")
	non.est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)!="Bien")
	
	
	#############################
	# deal with categorical attributes
	tlog(3,"Dealing with categorical attributes")
	cat.data <- NA
	
	# gather regular categorical attributes
	attrs <- intersect(COL_CAT_SELECT, vertex_attr_names(g))
	for(attr in attrs)
	{	# possibly focus only on the most frequent values (if too many)
		again <- TRUE		# should we go through the loop?
		main.vals <- c()	# most frequent values
		while(again)
		{	# considering all vertices for certain attributes
			if(attr==COL_LOC_TYPE)
			{	sel.idx <- 1:gorder(g)
				non.sel.idx <- c()
			}
			else
			{	sel.idx <- est.idx
				non.sel.idx <- non.est.idx
			}
			# get values only for real-estate vertices
			tmp <- vertex_attr(graph=g, name=attr, index=sel.idx)
			if(is.numeric(tmp)) 
				tmp <- as.character(tmp)
			suffix <- ""
			if(length(main.vals)>0)	# repetition for the top values
			{	tmp[!(tmp %in% main.vals) & !is.na(tmp)] <- "Other"
				suffix <- "_main"
			}
			g00 <- set_vertex_attr(graph=g, name=attr, index=sel.idx, value=tmp)
			g00 <- set_vertex_attr(graph=g00, name=attr, index=non.sel.idx, value=rep(NA,length(non.sel.idx)))
			attr.name <- paste0(attr,suffix)
			
			# plot the attribute distribution as a barplot
			tlog(4,"Bar-plotting attribute \"",attr.name,"\"")
			cols <- retrieve.palette.cat(values=tmp, attr=attr)$pal.cols
			tmp2 <- tmp; tmp2[is.na(tmp2)] <- rep("NA",length(which(is.na(tmp2))))
			tt <- table(factor(tmp2, levels=names(cols)))
			plot.folder <- file.path(attr.folder, attr)
			dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
			plot.file <- file.path(plot.folder, paste0(attr.name,"_bars"))
			tlog(6,"Plot in file '",plot.file,"'")
			custom.barplot(
				tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence",
				file=plot.file,
				cols=cols
			)
			# record as a table
			tab <- as.data.frame(tt)
			colnames(tab) <- c("Value","Frequency")
			table.file <- file.path(plot.folder, paste0(attr.name,"_vals",".csv"))
			tlog(6,"Record data in file '",table.file,"'")
			write.csv(tab, file=table.file, row.names=FALSE)
			
			# get the most frequent values (for next iteration)
			if(nrow(tab)>17)
				main.vals <- names(sort(tt[names(tt)!="NA"],decreasing=TRUE)[1:15])
			else
				again <- FALSE
			
			# barplot again, but without the NA values (if any)
			if("NA" %in% names(cols) && length(tt)>1)
			{	# remove NAs
				tt <- tt[-which(names(tt)=="NA")]
				cols <- cols[-which(names(cols)=="NA")]
				# plot again
				plot.file <- file.path(plot.folder, paste0(attr.name,"_bars_withoutNA"))
				tlog(6,"Plot in file '",plot.file,"'")
				custom.barplot(
					tt, 
					text=names(tt), 
					xlab=LONG_NAME[attr], ylab="Frequence",
					file=plot.file,
					cols=cols
				)
			}
			
			# plot the graph using colors for attribute values
			plot.file <- file.path(plot.folder, paste0(attr.name,"_graph"))
			tlog(4,"Graph-plotting attribute \"",attr.name,"\" in '",plot.file,"'")
			V(g00)$label <- paste(vertex_attr(g00,name=COL_LOC_ID), get.location.names(g00),sep="_")
			g1 <- g00; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=attr, cat.att=TRUE, color.isolates=TRUE, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
			g1 <- g00; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=attr, cat.att=TRUE, color.isolates=TRUE, file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
			
			# add to matrix
			tlog(4,"Adding attribute \"",attr.name,"\" to data matrix")
			if(attr==COL_LOC_TYPE)
				tmp <- tmp[est.idx]
			if(all(is.na(cat.data)))
				cat.data <- matrix(tmp,ncol=1)
			else
				cat.data <- cbind(cat.data, tmp)
			colnames(cat.data)[ncol(cat.data)] <- attr.name
		}
	}
	
	# convert tag-type attributes
	tlog(3,"Dealing with tag-type attributes")
	attrs <- intersect(names(COL_TAG_SELECT), vertex_attr_names(g))
	for(attr in attrs)
	{	# possibly focus only on the most frequent values (if too many)
		again <- TRUE		# should we go through the loop?
		main.vals <- c()	# most frequent values
		while(again)
		{	# get values only for real-estate vertices
			g0 <- delete_vertices(graph=g, v=non.est.idx)
			attrc <- intersect(COL_TAG_SELECT[[attr]], vertex_attr_names(g))
			if(length(main.vals)>0)	# repetition for the top values
			{	suffix <- "_main"
				for(att in attrc)
				{	tmp <- vertex_attr(graph=g0, name=att)
					tmp[!(tmp %in% main.vals) & !is.na(tmp)] <- "Other"
					g0 <- set_vertex_attr(graph=g0, name=att, value=tmp)
				}
			}
			else
				suffix <- ""
			m <- sapply(attrc, function(att) vertex_attr(g0, att))
			attr.name <- paste0(attr,suffix)
			
			# count tag distribution
			idx.nas <- which(apply(m,1,function(r) all(is.na(r))))	# detect individuals with only NAs
			nbr.nas <- length(idx.nas) 								# count them
			dt <- c(m)[!is.na(c(m))]								# handles non-NA values
			dt <- c(dt,rep(NA,nbr.nas))								# insert the appropriate number of NAs
			
			# colors
			cols <- retrieve.palette.cat(values=dt, attr=attr)$pal.cols
			dt[is.na(dt)] <- rep("NA",length(which(is.na(dt))))
			tt <- table(factor(dt, levels=names(cols)))
			
			ymax <- max(tt)											# compute highest frequency for later use (to handle plot y-scale)
			
			# possibly remove empty values
			if(length(main.vals)>0)
			{	idx <- which(tt==0)
				if(length(idx)>0)
				{	tt <- tt[-idx]
					cols <- cols[-idx]
				}
			}
			
			# plot tag distribution as barplot
			tlog(4,"Bar-plotting attributes containing \"",attr.name,"\"")
			plot.folder <- file.path(attr.folder, attr)
			dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
			plot.file <- file.path(plot.folder, paste0(attr.name,"_bars"))
			tlog(6,"Plot distribution in '",plot.file,"'")
			custom.barplot(
				tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				file=plot.file,
				cols=cols
			)
			# record tag distribution as table
			if(length(tt)==1)
				tab <- data.frame(names(tt),tt)
			else
				tab <- as.data.frame(tt)
			colnames(tab) <- c("Value","Frequency")
			rownames(tab) <- NULL
			table.file <- file.path(plot.folder, paste0(attr.name,"_vals.csv"))
			tlog(6,"Record data in '",table.file,"'")
			write.csv(tab, file=table.file, row.names=FALSE)
			
			# get the most frequent values (used later)
			if(nrow(tab)>17)
				main.vals <- names(sort(tt[names(tt)!="NA"],decreasing=TRUE)[1:15])
			else
				again <- FALSE
			
			# barplot again, but without the NA values (if any)
			if("NA" %in% names(cols) && length(tt)>1)
			{	# remove NAs
				tt <- tt[-which(names(tt)=="NA")]
				cols <- cols[-which(names(cols)=="NA")]
				# plot again
				plot.file <- file.path(plot.folder, paste0(attr.name,"_bars_withoutNA"))
				tlog(6,"Plot distribution in '",plot.file,"'")
				custom.barplot(
					tt, 
					text=names(tt), 
					xlab=LONG_NAME[attr], ylab="Frequence", 
					file=plot.file,
					cols=cols
				)
			}
			
			# plot tags on a graph
			g00 <- g
			for(a in colnames(m))
			{	vals <- vertex_attr(g0,a)
				#vals[which(!is.na(match(vals,unfrequent)))] <- VAL_OTHER	# represents all unfrequent values under an umbrella name
				g00 <- set_vertex_attr(graph=g00, name=a, index=non.est.idx, value=rep(NA,length(non.est.idx)))
				g00 <- set_vertex_attr(graph=g00, name=a, index=est.idx, value=vals)
			}
			plot.file <- file.path(plot.folder, paste0(attr.name,"_graph"))
			tlog(6,"Plot graph in '",plot.file,"'")
			V(g00)$label <- paste(vertex_attr(g00,name=COL_LOC_ID), get.location.names(g00),sep="_")
			g1 <- g00; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=attrc, cat.att=TRUE, color.isolates=TRUE, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
			g1 <- g00; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=attrc, cat.att=TRUE, color.isolates=TRUE, file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
		}
		
		# add to matrix
		if(!fast)
		{	tlog(4,"Adding attribute \"",attr,"\" to data matrix")
			uvals <- sort(unique(c(m)))
			for(uval in uvals)
			{	# binarize tags
				vals <- apply(m, 1, function(v) uval %in% v[!is.na(v)])
				idxt <- which(vals)
				idxf <- which(!vals)
				vals[idxt] <- VAL_TRUE
				vals[idxf] <- VAL_FALSE
				vals[idx.nas] <- NA
				cat.data <- cbind(cat.data, vals)
				att_name <- paste(attr,uval,sep="_")
				colnames(cat.data)[ncol(cat.data)] <- att_name
				
				# setup folder
				short_val <- trimws(substr(uval,1,30))
				plot.folder2 <- file.path(plot.folder, short_val)
				dir.create(path=plot.folder2, showWarnings=FALSE, recursive=TRUE)
				
				# produce TRUE/FALSE barplots
				tlog(6,"Producing barplots for attribute \"",att_name,"\"")
				tt <- table(vals, useNA="ifany")
				plot.file <- file.path(plot.folder2, "bars")
				tlog(8,"Plot distribution in '",plot.file,"'")
				custom.barplot(tt, 
					text=names(tt), 
					xlab=paste0(LONG_NAME[attr]," : ",uval), ylab="Frequence", 
					file=plot.file,
					ylim=c(0,ymax))
				# record values as table
				tt <- as.data.frame(tt)
				colnames(tt) <- c("Value","Frequency")
				table.file <- file.path(plot.folder2, "vals.csv")
				tlog(8,"Record data in '",table.file,"'")
				write.csv(tt, file=table.file, row.names=FALSE)
				
				# plot the graph using colors for attribute values
				tlog(6,"Graph-plotting attribute \"",att_name,"\"")
				plot.file <- file.path(plot.folder2, "graphs")
				tlog(8,"Plot graph in '",plot.file,"'")
				g00 <- set_vertex_attr(graph=g, name=att_name, index=non.est.idx, value=rep(NA,length(non.est.idx)))
				g00 <- set_vertex_attr(graph=g00, name=att_name, index=est.idx, value=vals)
				V(g00)$label <- paste(vertex_attr(g00,name=COL_LOC_ID), get.location.names(g00),sep="_")
				g1 <- g00; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=att_name, col.att.cap=paste0(LONG_NAME[attr]," : ",uval), cat.att=TRUE, color.isolates=TRUE, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
				g1 <- g00; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
				custom.gplot(g=g1, col.att=att_name, col.att.cap=paste0(LONG_NAME[attr]," : ",uval), cat.att=TRUE, color.isolates=TRUE, file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
			}
		}
	}
	
	# replace NAs by "Unknown" tags
#	cat.data[which(is.na(cat.data))] <- VAL_UNK
	
	if(!fast)
	{	tlog(3,"Plotting categorical attributes against one another")
		for(i in 1:ncol(cat.data))
		{	attr <- colnames(cat.data)[i]
			
			# plot one attribute versus another
			if(i<ncol(cat.data))
			{	for(j in (i+1):ncol(cat.data))
				{	attr2 <- colnames(cat.data)[j]
					vals1 <- cat.data[,i]
					vals2 <- cat.data[,j]
					tt <- table(vals1, vals2, useNA="ifany")
					names(dimnames(tt)) <- c(LONG_NAME[attr],LONG_NAME[attr2])
					win.limit <- 260
					threshold <- min(30, (win.limit-(nchar(normalizePath(comp.folder))+nchar("//")+nchar("_vs_")+nchar("_bars")+nchar(".xxx")))%/%2)
					shrt.attr1 <- substr(attr,1,threshold)		# to avoid long file names
					shrt.attr2 <- substr(attr2,1,threshold)		# same
					# plot file
					plot.file <- file.path(comp.folder, paste0(shrt.attr1,"_vs_",shrt.attr2,"_bars"))
					tlog(4,"Plot in file '",plot.file,"'")
					custom.barplot(vals=tt, 
						text=colnames(tt), 
						xlab=LONG_NAME[attr2], ylab="Frequence",
						file=plot.file)
					# record tag distribution as table
					tt <- as.data.frame(tt)
					table.file <- file.path(comp.folder, paste0(shrt.attr1,"_vs_",shrt.attr2,"_vals.csv"))
					write.csv(tt, file=table.file, row.names=FALSE)
				}
			}
		}
	}
	
	
	#############################
	# deal with numerical attributes
	tlog(3,"Dealing with numerical attributes")
	num.data <- NULL
	
	# gather regular numerical attributes
	tlog(4,"Gathering attribute values")
	attrs <- intersect(COL_NUM_SELECT, vertex_attr_names(g))
	for(attr in attrs)
	{	tlog(5,"Processing attribute \"",attr,"\"")
		# get values only for real-estate vertices
		g0 <- delete_vertices(graph=g, v=non.est.idx)
		tmp <- vertex_attr(g0, attr)
		
		# set colors
		cols <- retrieve.palette.num(values=tmp)$pal.cols
		
		# plot the attribute distribution as a histogram 
		# (actually a barplot, for now, as the only numeric attribute is an integer)
		tlog(6,"Bar-plotting attribute \"",attr,"\"")
		tt <- table(tmp, useNA="ifany")
		plot.folder <- file.path(attr.folder, attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
		tlog(7,"Plot distribution in '",plot.file,"'")
		custom.barplot(tt, 
			text=names(tt), 
			xlab=LONG_NAME[attr], ylab="Frequence", 
			cols=cols,
			file=plot.file
		)
		# record as a table
		tab <- as.data.frame(tt)
		colnames(tab) <- c("Value","Frequency")
		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
		tlog(7,"Record data in '",table.file,"'")
		write.csv(tab, file=table.file, row.names=FALSE)
		
		# barplot again, but without the NA values (if any)
		if("NA" %in% names(cols) && length(tt)>1)
		{	# remove NAs
			tt <- tt[-which(is.na(names(tt)))]
			cols <- cols[-which(names(cols)=="NA")]
			# plot again
			plot.file <- file.path(plot.folder, paste0(attr,"_bars_withoutNA"))
			tlog(6,"Plot distribution in '",plot.file,"'")
			custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				cols=cols,
				file=plot.file
			)
		}
		
		# add to matrix
		if(is.null(num.data))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# replace NAs by "Unknown" tags
#	num.data[which(is.na(num.data))] <- VAL_UNK
	
	# plot the graph using colors for attribute values
	tlog(4,"Producing plot files")
	if(!is.null(num.data))
	{	for(i in 1:ncol(num.data))
		{	attr <- colnames(num.data)[i]
			tlog(5,"Plotting attribute \"",attr,"\"")
			g00 <- set_vertex_attr(graph=g, name=attr, index=non.est.idx, value=rep(NA,length(non.est.idx)))
			g00 <- set_vertex_attr(graph=g00, name=attr, index=est.idx, value=num.data[,i])
			V(g00)$label <- paste(vertex_attr(g00,name=COL_LOC_ID), get.location.names(g00),sep="_")
			plot.folder <- file.path(attr.folder, attr)
			plot.file <- file.path(plot.folder, paste0(attr,"_graph"))
			tlog(6,"Producing file \"",plot.file,"\"")
			g1 <- g00; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=attr, cat.att=FALSE, color.isolates=TRUE, file=paste0(plot.file,"_lambert"), asp=1, size.att=2, edge.arrow.mode=0, vertex.label.cex=0.1)
			g1 <- g00; V(g1)$x <- V(g1)$x2; V(g1)$y <- V(g1)$y2; E(g1)$weight <- 0.5; g1 <- delete_edge_attr(g1, LK_TYPE); g1 <- simplify(g1)
			custom.gplot(g=g1, col.att=attr, cat.att=FALSE, color.isolates=TRUE, file=paste0(plot.file,"_algo"), rescale=FALSE, xlim=range(V(g1)$x), ylim=range(V(g1)$y), edge.arrow.mode=0, vertex.label.cex=0.1, size.att=6)
		}
	}
	else
		tlog(4,"No numerical attribute detected")
	
	
	#############################
	# attributes over
	return(g)
}
