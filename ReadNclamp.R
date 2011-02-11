# A log table is a special table produced by Nclamp
# It consist of a PXP file with variables only
ReadNclampLogTable<-function(f,Verbose=FALSE){
	x=ReadIgorPackedExperiment(f,Verbose=Verbose)
	
#	WaveFields	
	# Select text fields only
	# cat("orig date field is:",x$vars$FileDate)
	# remove any commas and rationalise spaces
	
	exptDate<-.ParseDate(x$vars$FileDate)
	exptDateStr=format(exptDate)
	if(Verbose) cat("date is:",exptDateStr)
	
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

.ParseDate<-function(d)
{
	d=gsub("[, ]+"," ",d)
	exptDate=strptime(d,format="%a %b %d %Y") # "Thu Oct 25 2007"
	if(is.na(exptDate))
		exptDate=strptime(d, format = "%a %e %b %Y") # "Thu 04 Oct 2007"
	if(is.na(exptDate)) stop("Unable to parse date format: ",d)
	exptDate
}

ReadSweepFile<-function(f){
	# function to extract key data from Sweep File 
	# e.g. for import into Physiology database
	s=ReadIgorPackedExperiment(f)
	
	fileinfo=file.info(f)
	extraFields=list(FilePath=f,Folder=basename(dirname(f)),
		FileSize=fileinfo$size,FileMTime=fileinfo$mtime,FileMD5=md5sum(f))
	
	chosenFields=c("FileFormat","NumWaves",
		"NumChannels","FileName","FileDate","FileTime","WavePrefix","AcqMode")
	
	summaryFields=s$vars[chosenFields]
	listnames=names(s)[sapply(s,class)=="list"]
	possStimuli=setdiff(listnames, c("vars","Notes",grep("^Chan[A-Z]$",listnames,val=T)))
	if(length(possStimuli)!=1) stop("unable to identify stimulus protocol")
	ProtocolName=possStimuli[1]
	chosenProtocolFields=c("WaveLength","SampleInterval","SamplesPerWave",
		"NumStimWaves","InterStimTime","NumStimReps","InterRepTime","StimRate",
		"RepRate","TotalTime","CurrentFile")
	stimFields=s[[ProtocolName]][["vars"]][chosenProtocolFields]
	names(stimFields)<-paste("Stim",chosenProtocolFields,sep="")
	rval=c(extraFields,summaryFields,ProtocolName=ProtocolName,stimFields)
	nullfields=sapply(rval,is.null)
	rval[nullfields]=NA
	return(rval)
}

SweepFilesToDataFrame<-function(ff){
	ll=lapply(ff, ReadSweepFile)
	lengths=sapply(ll,length)
	if(any(lengths!=lengths[1]))
		stop("Heterogeneous results from ReadSweepFile")
	# df=as.data.frame(ll[1],stringsAsFactors=FALSE)
	# if(length(ll)==1) return(df)
	# ll=ll[-1]
	do.call(rbind,lapply(ll,as.data.frame,stringsAsFactors=FALSE))
}

UpdateSweepDataFrame<-function(foldername,outfile=NULL,action=c("update","force"),DryRun=FALSE){
	action=match.arg(action)
	foldername=path.expand(foldername) # replace ~ by full path if necessary
	if(is.null(outfile)){
		outfile=file.path(foldername,paste(basename(foldername),sep=".","csv"))
	}
	infiles=dir(foldername,patt="_[0-9]+\\.pxp$",full=T)
	if(!file.exists(outfile)){
		# we need to process all infiles
		newinfiles=infiles
	} else {
		intimes=file.info(infiles)$mtime
		outtime=file.info(outfile)$mtime
		newinfiles=infiles[intimes>outtime]
	}
	if(length(newinfiles)>0){
		if(DryRun) {
			cat("Would process the following files:\n",sep="",paste(newinfiles,collapse="\n"))
		} else {
			newdf=SweepFilesToDataFrame(newinfiles)
			if(file.exists(outfile)){
				# read in the old data frame, and overwrite updated rows/append new rows
				olddf=read.csv(outfile,stringsAsFactors=FALSE)
				rownames(olddf)=olddf[,1] # set the rownames to first col which should be FilePath
				olddf[rownames(newdf),]=newdf
				newdf=olddf
			}
			write.csv(newdf,outfile)
		}
		invisible(TRUE)
	}
}
