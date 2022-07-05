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
# fast: whether to perform a fast computation of these measures.
# 
# returns: same graph, updated with the results.
#############################################################
analyze.net.assortativity <- function(g, out.folder, fast)
{	tlog(2,"Computing the assortativity")
#	modes <- c(MEAS_MODE_UNDIR, MEAS_MODE_DIR)
	modes <- c(MEAS_MODE_UNDIR)
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	# indices of real estate vertices
	est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)=="Bien")
	non.est.idx <- which(vertex_attr(g, name=COL_LOC_TYPE)!="Bien")
	
	# init result table
	val.tab <- matrix(nrow=0,ncol=length(modes))
	colnames(val.tab) <- modes
	
	
	#############################
	# deal with categorical attributes
	tlog(4,"Dealing with categorical attributes")
	cat.data <- NA
	
	# gather regular categorical attributes 
	attrs <- intersect(COL_CAT_SELECT, vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- intersect(names(COL_TAG_SELECT), vertex_attr_names(g))
	for(attr in attrs)
	{	tmp <- intersect(COL_TAG_SELECT[[attr]], vertex_attr_names(g))
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
			
			# treat all vertices vs. only real estate
			for(focus in c("all","realestate"))
			{	# prepare data
				if(focus=="all")
				{	cdat <- cat.data[,i]
					g0 <- g
					txt <- "all vertices"
					an <- "_all"
				}
				else
				{	cdat <- cat.data[est.idx,i]
					g0 <- delete_vertices(graph=g, v=non.est.idx)
					txt <- "real estate only"
					an <- "_realEstOnly"
				}
				
				# if there are some NAs
				if(any(is.na(cdat)))
				{	# explicitly represent NAs as a class
					cd <- as.integer(cdat)
					if(all(is.na(cd)))
						ass <- NA
					else
					{	cd[is.na(cd)] <- max(cd,na.rm=TRUE) + 1
						ass <- assortativity_nominal(graph=g0, types=cd, directed=mode==MEAS_MODE_DIR)
					}
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode," -- ",txt,") when representing NAs explicitly: ",ass)
					name <- paste0(attr,"_",an,"_expNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
					
					# just ignore NAs
					cd <- as.integer(cdat)
					if(all(is.na(cd)))
						ass <- NA
					else
					{	cd <- cd[!is.na(cd)]
						g00 <- delete_vertices(g0, which(is.na(cdat)))
						ass <- assortativity_nominal(graph=g00, types=cd, directed=mode==MEAS_MODE_DIR)
					}
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode," -- ",txt,") when ignoring NAs: ",ass)
					name <- paste0(attr,"_",an,"_noNA")
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
				}
				
				# no NA at all
				else
				{	cd <- as.integer(cdat)
					ass <- assortativity_nominal(graph=g0, types=cd, directed=mode==MEAS_MODE_DIR)
					tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode," -- ",txt,"): ",ass)
					name <- paste0(attr,"_",an)
					if(!(name %in% rownames(val.tab)))
					{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
						rownames(val.tab)[nrow(val.tab)] <- name
					}
					val.tab[name,mode] <- ass
				}
			}
		}
	}
	
	
	#############################
	# deal with numerical attributes
	tlog(4,"Dealing with numerical attributes")
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- intersect(COL_NUM_SELECT, vertex_attr_names(g))
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
				
				# treat all vertices vs. only real estate
				for(focus in c("all","realestate"))
				{	# prepare data
					if(focus=="all")
					{	ndat <- num.data[,i]
						g0 <- g
						txt <- "all vertices"
						an <- "_all"
					}
					else
					{	ndat <- num.data[est.idx,i]
						g0 <- delete_vertices(graph=g, v=non.est.idx)
						txt <- "real estate only"
						an <- "_realEstOnly"
					}
					
					# if there are some NAs
					if(any(is.na(ndat)))
					{	# explicitly represent them as zeroes
						cd <- ndat
						if(all(is.na(cd)))
							ass <- NA
						else
						{	cd[is.na(cd)] <- 0
							ass <- assortativity(graph=g0, types1=cd, directed=mode==MEAS_MODE_DIR)
						}
						tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode," - ",txt,") when replacing NAs by 0: ",ass)
						name <- paste0(attr,"_",an,"_expNA")
						if(!(name %in% rownames(val.tab)))
						{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
							rownames(val.tab)[nrow(val.tab)] <- name
						}
						val.tab[name,mode] <- ass
						# ignore them
						cd <- ndat
						if(all(is.na(cd)))
							ass <- NA
						else
						{	cd <- cd[!is.na(cd)]
							g00 <- delete_vertices(g0, which(is.na(ndat)))
							ass <- assortativity(graph=g00, types1=cd, directed=mode==MEAS_MODE_DIR)
						}
						tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode," - ",txt,") when ignoring NAs: ",ass)
						name <- paste0(attr,"_",an,"_noNA")
						if(!(name %in% rownames(val.tab)))
						{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
							rownames(val.tab)[nrow(val.tab)] <- name
						}
						val.tab[name,mode] <- ass
					}
					# no NA at all
					else
					{	ass <- assortativity(graph=g0, types1=ndat, directed=mode==MEAS_MODE_DIR)
						tlog(12,"Assortativity for attribute \"",attr,"\" (mode=",mode," - ",txt,"): ",ass)
						name <- paste0(attr,"_",an)
						if(!(name %in% rownames(val.tab)))
						{	val.tab <- rbind(val.tab, rep(NA,length(modes)))
							rownames(val.tab)[nrow(val.tab)] <- name
						}
						val.tab[name,mode] <- ass
					}
				}
			}
		}
	}
	else
		tlog(6,"No numerical attribute detected")
	
	
	#############################
	# record the results
	res.file <- file.path(out.folder, g$name, paste0(MEAS_ASSORTATIVITY,".csv"))
	tlog(4,"Record results in '",res.file,"'")
	write.csv(val.tab, file=res.file, row.names=TRUE)
	
	
	#############################
	# assortativity over
	return(g)
}
