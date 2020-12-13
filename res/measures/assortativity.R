#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#############################################################################################




#############################################################
# measure name
MEAS_ASSORTATIVITY <- "assortativity"
MEAS_LONG_NAMES[MEAS_ASSORTATIVITY] <- "Assortativity"




#############################################################
# Computes the assortativity of the network.
#
# g: original graph to process.
# out.folder: main output folder.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.assortativity <- function(g, out.folder)
{	tlog(2,"Computing the assortativity")
	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	# init result table
	val.tab <- matrix(nrow=0,ncol=2)
	colnames(val.tab) <- MEAS_LONG_NAMES[modes]
	
	#############################
	# deal with categorical attributes
	tlog(4,"Dealing with categorical attributes")
	cat.data <- NA
	
	# gather regular categorical attributes 
	attrs <- intersect(COL_CAT, vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- intersect(names(COL_TAG), vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- intersect(COL_TAG[[attr]], vertex_attr_names(g))
		m <- sapply(tmp, function(att) vertex_attr(g, att))
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	cat.data <- cbind(cat.data, as.integer(factor(apply(m, 1, function(v) uval %in% v[!is.na(v)]))))
			colnames(cat.data)[ncol(cat.data)] <- paste(attr,uval,sep="_")
		}
	}
	
	tlog(6,"Computing undirected vs. directed links")
	for(mode in modes)
	{	tlog(8,"Computing mode=",mode)
		
		# compute the assortativity for all categorical attributes
		for(i in 1:ncol(cat.data))
		{	# compute the assortativity
			attr <- colnames(cat.data)[i]
			tlog(10,"Computing attribute ",attr," (",i,"/",ncol(cat.data),")")
			
			# if there are some NAs
			if(any(is.na(cat.data[,i])))
			{	# explicitly represent NAs as a class
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)))
					ass <- NA
				else
				{	cd[is.na(cd)] <- max(cd,na.rm=TRUE) + 1
					ass <- assortativity_nominal(graph=g, types=cd, directed=mode==MEAS_MODE_DIR)
				}
				tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when representing NAs explicitly: ",ass)
				name <- paste0(attr,"_expNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- ass
				# just ignore NAs
				cd <- as.integer(cat.data[,i])
				if(all(is.na(cd)))
					ass <- NA
				else
				{	cd <- cd[!is.na(cd)]
					gg <- delete_vertices(g, which(is.na(cat.data[,i])))
					ass <- assortativity_nominal(graph=gg, types=cd, directed=mode==MEAS_MODE_DIR)
				}
				tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",ass)
				name <- paste0(attr,"_noNA")
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- ass
			}
			
			# no NA at all
			else
			{	cd <- as.integer(cat.data[,i])
				ass <- assortativity_nominal(graph=g, types=cd, directed=mode==MEAS_MODE_DIR)
				tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,"): ",ass)
				name <- paste0(attr)
				if(!(name %in% rownames(val.tab)))
				{	val.tab <- rbind(val.tab, c(NA,NA))
					rownames(val.tab)[nrow(val.tab)] <- name
				}
				val.tab[name,mode] <- ass
			}
		}
	}
	
	#############################
	# deal with numerical attributes
	tlog(4,"Dealing with numerical attributes")
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM, vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# compute the assortativity for all numerical attributes
	if(!is.null(ncol(num.data)))
	{	tlog(6,"Computing undirected vs. directed links")
		for(mode in modes)
		{	tlog(8,"Computing mode=",mode)
			
			for(i in 1:ncol(num.data))
			{	# compute the assortativity
				attr <- colnames(num.data)[i]
				tlog(10,"Computing attribute ",attr," (",i,"/",ncol(num.data),")")
				
				# if there are some NAs
				if(any(is.na(num.data[,i])))
				{	# explicitly represent them as zeroes
					cd <- num.data[,i]
					if(all(is.na(cd)))
						ass <- NA
					else
					{	cd[is.na(cd)] <- 0
						ass <- assortativity(graph=g, types1=cd, directed=mode==MEAS_MODE_DIR)
					}
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when replacing NAs by 0: ",ass)
					name <- paste0(attr,"_expNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
					# ignore them
					cd <- num.data[,i]
					if(all(is.na(cd)))
						ass <- NA
					else
					{	cd <- cd[!is.na(cd)]
						gg <- delete_vertices(g, which(is.na(num.data[,i])))
						ass <- assortativity(graph=gg, types1=cd, directed=mode==MEAS_MODE_DIR)
					}
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,") when ignoring NAs: ",ass)
					name <- paste0(attr,"_noNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
				}
				# no NA at all
				else
				{	ass <- assortativity(graph=g, types1=num.data[,i], directed=mode==MEAS_MODE_DIR)
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode,"): ",ass)
					name <- paste0(attr)
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, c(NA,NA))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
				}
			}
		}
	}
	else
		tlog(6,"No numerical attribute detected")
	
	#############################
	# record the results
	res.file <- file.path(out.folder, g$name, paste0(MEAS_ASSORTATIVITY,".csv"))
	write.csv(val.tab, file=res.file, row.names=TRUE)
	
	#############################
	# assortativity over
	return(g)
}
