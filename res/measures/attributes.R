#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
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
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.attributes <- function(g, out.folder)
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
	cat.data <- NA
	
	# gather regular categorical attributes
	attrs <- intersect(COL_CAT, vertex_attr_names(g))
	for(attr in attrs)
	{	# get values only for real-estate vertices
		g0 <- delete_vertices(graph=g, v=non.est.idx)
		tmp <- vertex_attr(g0, attr)
		g00 <- set_vertex_attr(graph=g, name=attr, index=non.est.idx, value=rep(NA,length(non.est.idx)))
		
		# plot the attribute distribution as a barplot
		tlog(4,"Bar-plotting attribute \"",attr,"\"")
		tt <- table(tmp, useNA="ifany")
		plot.folder <- file.path(attr.folder, attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
		custom.barplot(tt, 
			text=names(tt), 
			xlab=LONG_NAME[attr], ylab="Frequence",
			file=plot.file)
		# record as a table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		
		# plot the graph using colors for attribute values
		tlog(4,"Graph-plotting attribute \"",attr,"\"")
		plot.file <- file.path(plot.folder, paste0(attr,"_graph"))
		V(g00)$label <- rep(NA, gorder(g00))
		custom.gplot(g=g00, col.att=attr, cat.att=TRUE, color.isolates=TRUE, file=plot.file)
		#custom.gplot(g=g, col.att=attr, cat.att=TRUE, color.isolates=TRUE)
		
		# add to matrix
		tlog(4,"Adding attribute \"",attr,"\" to data matrix")
		if(all(is.na(cat.data)))
			cat.data <- matrix(tmp,ncol=1)
		else
			cat.data <- cbind(cat.data, tmp)
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- intersect(names(COL_TAG), vertex_attr_names(g))
	for(attr in attrs)
	{	# get values only for real-estate vertices
		g0 <- delete_vertices(graph=g, v=non.est.idx)
		tmp <- vertex_attr(g0, attr)
		g00 <- set_vertex_attr(graph=g, name=attr, index=non.est.idx, value=rep(NA,length(non.est.idx)))
		
		attrc <- intersect(COL_TAG[[attr]], vertex_attr_names(g))
		m <- sapply(attrc, function(att) vertex_attr(g0, att))
		
		# count tag distribution
		idx.nas <- which(apply(m,1,function(r) all(is.na(r))))	# detect individuals with only NAs
		nbr.nas <- length(idx.nas) 								# count them
		dt <- c(m)[!is.na(c(m))]								# handles non-NA values
		dt <- c(dt,rep(NA,nbr.nas))								# insert the appropriate number of NAs
		# compute highest frequency for later use (to handle plot y-scale)
		tt <- table(dt, useNA="ifany")
		if(any(is.na(names(tt))))
			na.nbr <- tt[is.na(names(tt))]
		else
			na.nbr <- 0
		tmp <- sapply(tt, function(x) gorder(g0)-x-na.nbr)
		ymax <- max(tmp,na.nbr)
		# identify least frequent values
		unfrequent <- names(tt)[which(tt<=2)]
		# plot tag distribution as barplot
		tlog(4,"Bar-plotting attributes containing \"",attr,"\"")
		plot.folder <- file.path(attr.folder, attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
		custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				file=plot.file)
		# record tag distribution as table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		# plot tags on a graph
		g00 <- g
		for(a in colnames(m))
		{	vals <- vertex_attr(g0,a)
			vals[which(!is.na(match(vals,unfrequent)))] <- paste0(" ",VAL_OTHER) # represent all unfrequent values under an umbrella name
			g00 <- set_vertex_attr(graph=g00, name=a, index=non.est.idx, value=rep(NA,length(non.est.idx)))
			g00 <- set_vertex_attr(graph=g00, name=a, index=est.idx, value=vals)
		}
		plot.file <- file.path(plot.folder, paste0(attr,"_graph"))
		V(g)$label <- rep(NA, gorder(g00))
		custom.gplot(g=g00, col.att=attrc, cat.att=TRUE, color.isolates=TRUE, file=plot.file)
		#custom.gplot(g=g00, col.att=attrc, cat.att=TRUE, color.isolates=TRUE)
			
		# add to matrix
		tlog(4,"Adding attribute \"",attr,"\" to data matrix")
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
			custom.barplot(tt, 
				text=names(tt), 
				xlab=paste0(LONG_NAME[attr]," : ",uval), ylab="Frequence", 
				file=plot.file,
				ylim=c(0,ymax))
			# record values as table
			tt <- as.data.frame(tt)
			colnames(tt) <- c("Value","Frequency")
			table.file <- file.path(plot.folder2, "vals.csv")
			write.csv(tt, file=table.file, row.names=FALSE)
			
			# plot the graph using colors for attribute values
			tlog(6,"Graph-plotting attribute \"",att_name,"\"")
			plot.file <- file.path(plot.folder2, "graphs")
			g00 <- set_vertex_attr(graph=g, name=att_name, index=non.est.idx, value=rep(NA,length(non.est.idx)))
			g00 <- set_vertex_attr(graph=g00, name=att_name, index=est.idx, value=vals)
			V(g00)$label <- rep(NA, gorder(g00))
			custom.gplot(g=g00, col.att=att_name, col.att.cap=paste0(LONG_NAME[attr]," : ",uval), cat.att=TRUE, color.isolates=TRUE, file=plot.file)
			#custom.gplot(g=g00, col.att=att_name, col.att.cap=paste0(LONG_NAME[attr]," : ",uval), cat.att=TRUE, color.isolates=TRUE)
		}
	}
	
	# replace NAs by "Unknown" tags
#	cat.data[which(is.na(cat.data))] <- VAL_UNK
	
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
				shrt.attr1 <- substr(attr,1,30)		# to avoid long file names
				shrt.attr2 <- substr(attr2,1,30)	# same
				# plot file
				plot.file <- file.path(comp.folder, paste0(shrt.attr1,"_vs_",shrt.attr2,"_bars"))
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
	
	
	#############################
	# deal with numerical attributes
	num.data <- NULL
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM, vertex_attr_names(g))
	for(attr in attrs)
	{	# get values only for real-estate vertices
		g0 <- delete_vertices(graph=g, v=non.est.idx)
		tmp <- vertex_attr(g0, attr)
		
		# plot the attribute distribution as a histogram 
		# (actually a barplot, for now, as the only numeric attribute is an integer)
		tlog(4,"Bar-plotting attribute \"",attr,"\"")
		tt <- table(tmp, useNA="ifany")
		plot.folder <- file.path(attr.folder, attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder, paste0(attr,"_bars"))
		custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				file=plot.file)
		# record as a table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder, paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		
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
	if(!is.null(num.data))
	{	for(i in 1:ncol(num.data))
		{	attr <- colnames(num.data)[i]
			tlog(4,"Plotting attribute \"",attr,"\"")
			g00 <- set_vertex_attr(graph=g, name=attr, index=non.est.idx, value=rep(NA,length(non.est.idx)))
			g00 <- set_vertex_attr(graph=g00, name=attr, value=num.data[,i])
			V(g00)$label <- rep(NA, gorder(g00))
			plot.folder <- file.path(attr.folder, attr)
			plot.file <- file.path(plot.folder, paste0(attr,"_graph"))
			custom.gplot(g=g00, col.att=attr, cat.att=FALSE, color.isolates=TRUE, file=plot.file)
#			custom.gplot(g=g00, col.att=attr, cat.att=FALSE, color.isolates=TRUE)
		}
	}
	else
		tlog(4,"No numerical attribute detected")
	
	
	#############################
	# attributes over
	return(g)
}
