# A log table is a special table produced by Nclamp
# It consist of a PXP file with variables only
ReadNclampLogTable<-function(f,Verbose=FALSE){
	x=ReadIgorPackedExperiment(f,Verbose=Verbose)
	
#	WaveFields	
	# Select text fields only
	# cat("orig date field is:",x$vars$FileDate)
	# remove any commas and rationalise spaces
	exptDate=gsub("[, ]+"," ",x$vars$FileDate)
	exptDate=strptime(exptDate,format="%a %b %d %Y") # "Thu Oct 25 2007"
	if(is.na(exptDate))
		exptDate=strptime(x$vars$FileDate, format = "%a %e %b %Y") # "Thu 04 Oct 2007"
	if(is.na(exptDate)) stop("Unable to parse date format: ",x$vars$FileDate)
	exptDateStr=format(exptDate)
	
	cat("date is:",exptDateStr)
	
	# Do we have a table representation of the data (in waves) ?
	TableFields=sapply(x,function(x) !is.null(attr(x,"WaveHeader")))
	dateFields=NULL
	if(any(TableFields)){
		d=x[TableFields]
		xx=x[TableFields]
	} else{
		selFields=sapply(x[[2]]$vars,class)=="character"
		d=do.call(rbind,lapply(x[-1],function(x) unlist(x$vars[selFields])))
		dateFields=grep("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]",as.character(d[1,]))
		d=data.frame(d)		
	}
	#setdiff(names(x), grep("_[0-9]{3}",names(x),val=T))
	
	for (i in dateFields){
		d[i]=as.POSIXct(paste(exptDateStr,as.character(d[,i])))
#		d[i]=as.POSIXct(strptime(as.character(d[,i]),format="%H:%M:%S"))
	}
	d
}

ReadAllNclampLogTables<-function(logfiledir="/GD/projects/PhysiologyData/logs",...){
	logfilenames=dir(logfiledir,full=T)
	logfiles<<-list()
	for(i in seq(logfilenames)){
		if(i%%10>0) cat(".") else cat(as.integer(i))
		cat("logfilenames[i]=",logfilenames[i],"\n")
		logfiles[[i]]<<-try(ReadNclampLogTable(logfilenames[i]),...)
	}
	logfilenames
}
