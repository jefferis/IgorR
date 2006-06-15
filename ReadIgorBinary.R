# ReadIgorBinary.R
# 
# 2005-11-14
# File to read the igor binary file format into R
# There are various issues to deal with
# 1) Formats are either v2 or v5
# 2) Can be either Mac or PC

# Tested with the following from Igor Tech Note # 3 package
# zeroPointWave.ibw
# version5.ibw
# version2.ibw
# double.ibw
# along with my own testuint and testuword

# 2006-06-15
# Now reads the Igor packed experiment format into a list
# So far just works on the open/close data folders and reads 
# v2 and v5 multidimensional data.  Still need to check if matrices and
# higher dimensional arrays are laid out in the same order.
# Can supply a regex to read in only a selection and can also request that 
# only headers rather than dat are loaded. 
# Have also tested it with one of Rachel's .pxp files and that worked fine 
# as well

ReadIgorBinary<-function(con,Verbose=FALSE,ReturnTimeSeries=FALSE,MakeWave=FALSE,...){
	if (is.character(con)) {
		con <- file(con, "rb")
		on.exit(close(con))
	}
	
	# read one byte
	bytes=readBin(con,"integer",2,size=1)
	if(bytes[1]==0){
		endian="big"; version=bytes[2]
	} else {
		endian="little"; version=bytes[1]
	}
	if(Verbose) cat("version = ",version,"endian = ",endian,"\n")
	
	if(version==5) {
		rval=ReadIgorBinary.V5(con,Verbose=Verbose,endian=endian,ReturnTimeSeries=ReturnTimeSeries,...)
	} else if(version==2){
		rval=ReadIgorBinary.V2(con,Verbose=Verbose,endian=endian,ReturnTimeSeries=ReturnTimeSeries,...)
	}
	else stop(paste("Unable to read from Igor Binary File:",summary(con)$description,"with version:",version))

	# makes a wave with a specified name in the user environment
	if(MakeWave){
		WaveName=attr(rval,"WaveHeader")$WaveName
		assign(WaveName,rval,inherits=TRUE)
		invisible(WaveName)
	} else invisible(rval)

}

.DetermineIgorWaveType<-function(WaveHeader,endian=NULL){
	if(endian=="big") WaveHeader$type=rev(WaveHeader$type)
	WaveHeader$typeBits=as.integer(rawToBits(WaveHeader$type[1]))

	# // From IgorMath.h
	#define NT_CMPLX 1			// Complex numbers.
	#define NT_FP32 2			// 32 bit fp numbers.
	#define NT_FP64 4			// 64 bit fp numbers.
	#define NT_I8 8				// 8 bit signed integer. Requires Igor Pro 2.0 or later.
	#define NT_I16 	0x10		// 16 bit integer numbers. Requires Igor Pro 2.0 or later.
	#define NT_I32 	0x20		// 32 bit integer numbers. Requires Igor Pro 2.0 or later.
	#define NT_UNSIGNED 0x40	// Makes above signed integers unsigned. Requires Igor Pro 3.0 or later.

	if(WaveHeader$typeBits[6]) { what="integer"; size=4 }
	if(WaveHeader$typeBits[5]) { what="integer"; size=2 }
	if(WaveHeader$typeBits[4]) { what="integer"; size=1 }
	if(WaveHeader$typeBits[3]) { what="double"; size=8 }
	if(WaveHeader$typeBits[2]) { what="double"; size=4 }
	if(WaveHeader$typeBits[1]) what="complex"
	WaveHeader$signAdjustment=0 # note that readBin handles unsigned data poorly,
	# so have to do it myself
	if(WaveHeader$typeBits[7]) { what="integer"; WaveHeader$signAdjustment=2^(size*8)}
	WaveHeader$what=what;
	WaveHeader$size=size;
	return(WaveHeader)
}

.ConvertIntToUInt<-function(x,adjustment=2^32){
	x=as.numeric(x)
	signs=sign(x)
	x[signs<0]=x[signs<0]+adjustment
	x
}
.readNullTermString<-function(con,totalLength,...){
	s=readBin(con,what=character(),n=1,size=1,...)
	# note totalLength is the length including the null terminator
	readBin(con,what=character(),size=1,n=totalLength-nchar(s)-1,...)
	s
}

.readIgorDate<-function(){
	# Igor uses date as seconds since midnight 1 Jan 1904
	# Unfortunately does not seem to record time zone, so have to use
	# current
	# readBin can only read signed shorts or bytes,
	# so must add 2^32 to read as unsigned long
	i=as.numeric(myread())+2^32
	i=i+as.numeric(ISOdate(1904,1,1,hour=0,tz=""))
	class(i)<-"POSIXct"
	i
}

.readIgorDate<-function(){
	# Igor uses date as seconds since midnight 1 Jan 1904
	# Unfortunately does not seem to record time zone, so have to use
	# current
	# readBin can only read signed shorts or bytes,
	# so must add 2^32 to read as unsigned long
	i=as.numeric(myread())+2^32
	i=i+as.numeric(ISOdate(1904,1,1,hour=0,tz=""))
	class(i)<-"POSIXct"
	i
}
.ReadPackedHeader<-function(con,endian){
	recordType=readBin(con,size=2,what=integer(),signed=FALSE,endian=endian)
	version=readBin(con,size=2,what=integer(),endian=endian)
	numDataBytes=readBin(con,size=4,what=integer(),endian=endian) # is 4 correct?
	return(list(recordType=recordType,version=version,numDataBytes=numDataBytes))
}

.ReadDataFolderStartRecord<-function(con,endian){
	folderName=.readNullTermString(con,32)
	#cat("Start of Data Folder",folderName,"\n")
	folderName
}
.ReadDataFolderEndRecord<-function(con,endian){
	#cat("End of Data Folder")
}
.ReadWaveRecord<-function(con,endian,...){	
	x=ReadIgorBinary(con,...)
	#cat("Wave Record", attr(x,"WaveHeader")$WaveName,"\n")
	x
}

ReadPackedRecords<-function(con,endian = .Platform$endian,Verbose=FALSE,StructureOnly=FALSE,regex,...){
	require(bitops)
	if (is.character(con)) {
		con <- file(con, "rb")
		on.exit(close(con))
	}
	filename=summary(con)$description
	
	# Check if this file needs to be byte swapped
	firstShort=readBin(con,"integer",1,size=2)
	firstShort=bitAnd(firstShort,0x7FFF)
	if (bitAnd(firstShort,0xFF00) != 0) endian="swap"
		
	root=list() # we will store data here
	currentNames="root"
	recordStartPos=0; fileSize=file.info(filename)$size
	while(recordStartPos<fileSize){
		seek(con,recordStartPos)
		ph=.ReadPackedHeader(con,endian)
		if(Verbose) cat("recordStartPos =",recordStartPos,",Record type =",ph$recordType,
			"of length =",ph$numDataBytes,"\n")

		recordStartPos=seek(con)+ph$numDataBytes
		#if(!is.na(recTypeFuns[ph$recordType])) x=match.fun(recTypeFuns[ph$recordType])(con,endian)
		if (ph$recordType==3){
			# wave record
			x=.ReadWaveRecord(con,endian,...)
			el=paste(paste(currentNames,collapse="$"),sep="$",attr(x,"WaveHeader")$WaveName)
			# store the record if required
			if(missing(regex) || any( grep(regex,el) )){
				eval(parse(text=paste(el,"<-x")))
			}
			if (Verbose) cat("el:",el,"\n")
		} else if (ph$recordType==9){
			# Open Data Folder
			currentNames=c(currentNames,.ReadDataFolderStartRecord(con,endian))
		} else if (ph$recordType==10){
			# Close Data Folder
			currentNames=currentNames[-length(currentNames)]
		}		
	}
	invisible(root)
}


ReadIgorBinary.V2<-function(con,Verbose=FALSE,ReturnTimeSeries=NULL,endian=NULL,HeaderOnly=FALSE){
	myread=function(what="integer",size=4,...) readBin(con,endian=endian,what=what,size=size,...)
	# File pointer should be positioned at the start of the header
	# this should be 2 bytes into the wave data 
	# (ie after endian and version bytes).  Store that position:
	startPos=seek(con)

	# Read binary header 
	BinHeader2=list()
	BinHeader2$wfmSize=myread()
	BinHeader2$noteSize=myread(); myread()
	BinHeader2$checksum=myread(size=2)

	if(Verbose) print(BinHeader2)
	# Read WaveHeader
	seek(con,where=startPos+16-2) # is this necessary?
	WaveHeader2=list()
	WaveHeader2$type=myread(what="raw",size=1,n=2)
	myread()
	WaveHeader2$WaveName=.readNullTermString(con,18+2)
	myread(n=2) # Skip 8 bytes
	WaveHeader2$dataUnits=readNullTermString(3+1)
	WaveHeader2$xUnits=readNullTermString(3+1)
	WaveHeader2$npts=myread()
	myread(size=2) # skip aModified
	WaveHeader2$hsA=myread(what="double",size=8)
	WaveHeader2$hsB=myread(what="double",size=8)
	myread()
	WaveHeader2$fsValid=myread(size=2)!=0
	WaveHeader2$topFullScale=myread(what="double",size=8)
	WaveHeader2$botFullScale=myread(what="double",size=8)
	myread(size=1,n=10)
	WaveHeader2$CreationDate=.readIgorDate()
	myread(size=2)
	WaveHeader2$modDate=.readIgorDate()
	myread()
	
	WaveHeader2=.DetermineIgorWaveType(WaveHeader2,endian=endian)
	cat("signed=",(WaveHeader2$signAdjustment==0),"\n")
	
	if(HeaderOnly) {
		x=NA; attr(x,"WaveHeader")=WaveHeader2
		return(x)
	}
	# Note that only unsigned 16 bit data can be read by readBin
	WaveData=myread( what=WaveHeader2$what,size=WaveHeader2$size,n=WaveHeader2$npts,
		signed=(WaveHeader2$signAdjustment==0))
	# Handle adjustment for unsigned integer data
	if(WaveHeader2$signAdjustment!=0  && WaveHeader2$size>2){
		WaveData=.ConvertIntToUInt(WaveData,WaveHeader2$signAdjustment)
	}
	
	# Finish up
	attr(WaveData,"WaveHeader")=WaveHeader2
	if(ReturnTimeSeries){
		return(ts(WaveData,start=attr(WaveData,"WaveHeader")$hsB,deltat=attr(WaveData,"WaveHeader")$hsA))
	}
	else return (WaveData)

}

ReadIgorBinary.V5<-function(con,Verbose=FALSE,ReturnTimeSeries=NULL,endian=NULL,HeaderOnly=FALSE){
	# File pointer should be positioned at the start of the header
	# this should be 2 bytes into the wave data 
	# (ie after endian and version bytes).  Store that position:
	startPos=seek(con)
	
	# Read the BinHeader5 (64 bytes)
	# 1 byte	char
	# 2 bytes	short
	# 4 bytes	int, long, float, Handle, any kind of pointer
	# 8 bytes	double
	MAXDIMS=4

# typedef struct BinHeader5 {
# 	short version;						// Version number for backwards compatibility.
# 	short checksum;						// Checksum over this header and the wave header.
# 	long wfmSize;						// The size of the WaveHeader5 data structure plus the wave data.
# 	long formulaSize;					// The size of the dependency formula, if any.
# 	long noteSize;						// The size of the note text.
# 	long dataEUnitsSize;				// The size of optional extended data units.
# 	long dimEUnitsSize[MAXDIMS];		// The size of optional extended dimension units.
# 	long dimLabelsSize[MAXDIMS];		// The size of optional dimension labels.
# 	long sIndicesSize;					// The size of string indicies if this is a text wave.
# 	long optionsSize1;					// Reserved. Write zero. Ignore on read.
# 	long optionsSize2;					// Reserved. Write zero. Ignore on read.
# } BinHeader5;

	BinHeader5Def=data.frame(name=I(c("checksum","wfmSize","formulaSize","noteSize","dataEUnitsSize",
			"dimEUnitsSize","dimLabelsSize","sIndicesSize","optionsSize1","optionsSize2")),
		size=c(2,rep(4,9)),what="integer",n=c(rep(1,5),rep(4,2),rep(1,3)))
	BinHeader5=list()
	for(i in seq(nrow(BinHeader5Def)))
		BinHeader5[[BinHeader5Def$name[i]]]=readBin(con,endian=endian,what=BinHeader5Def$what[i],size=BinHeader5Def$size[i])
	if(Verbose) print(BinHeader5)
	
	
	
	# Read the WaveHeader5 (320 bytes)
	seek(con,where=startPos+64-2)
	WaveHeader5=list()
	myread=function(what="integer",size=4,...) readBin(con,endian=endian,what=what,size=size,...)
	
	# next WaveHeader5 pointer
	myread()
	WaveHeader5$creationDate=.readIgorDate()
	WaveHeader5$modDate=.readIgorDate()
	WaveHeader5$npts=myread()
	WaveHeader5$type=myread(what="raw",size=1,n=2)
	
	myread(size=1,n=10)
	WaveHeader5$WaveName=.readNullTermString(con,32)
	myread(n=2,size=4)
	WaveHeader5$nDim=myread(n=MAXDIMS)
	#highestDim=max(which(WaveHeader5$nDim>0))
	dims=WaveHeader5$nDim[WaveHeader5$nDim>0]
	nDims=length(dims)
	#if(any(WaveHeader5$nDim[-1]>0)) warning(paste("Ignoring additional dimensions for wave with dims",WaveHeader5$nDim))

	WaveHeader5$sfA=myread(n=MAXDIMS,what="double",size=8)
	WaveHeader5$sfB=myread(n=MAXDIMS,what="double",size=8)	
	# Units
	WaveHeader5$dataUnits=.readNullTermString(con(3+1)
	WaveHeader5$dimUnits=replicate(MAXDIMS,.readNullTermString(con(3+1))
	
	# Read the wave data 
	seek(con,startPos+64+320-2)
	WaveHeader5=.DetermineIgorWaveType(WaveHeader5,endian=endian)
	if(Verbose) print(WaveHeader5)
	if(Verbose) cat("data position = ",seek(con),"\n")

	# Have the option to return the header info alone - as a sort of
	# preview
	if(HeaderOnly) {
		x=NA; attr(x,"WaveHeader")=WaveHeader5
		return(x)
	}
	
	# Note that only unsigned 16 bit data can be read by readBin
	WaveData=myread( what=WaveHeader5$what,size=WaveHeader5$size,n=WaveHeader5$npts,
		signed=(WaveHeader5$signAdjustment==0))
	# Handle adjustment for unsigned integer data
	if(WaveHeader5$signAdjustment!=0  && WaveHeader5$size>2){
		# nb must convert int to numeric because R does not handle ints >
		# 2^31-1
		WaveData=.ConvertIntToUInt(WaveData,WaveHeader5$signAdjustment)
	}

	# Read anything else
	# Finish up
	if(nDims>1) dim(WaveData)=dims
	attr(WaveData,"WaveHeader")=WaveHeader5
	if(ReturnTimeSeries){
		return(ts(WaveData,start=attr(WaveData,"WaveHeader")$sfB[1],deltat=attr(WaveData,"WaveHeader")$sfA[1]))
	}
	else return (WaveData)
		
}
