# 
# Author: jfcollin
###############################################################################

#' Creates a desc object of "quantitative" statistics
#' 
#'
#' @param data Data.frame object
#' @param y Character indicating a numerical vector in the data frame passed to \code{data} argument
#' @param x1 Character indicating a factor in the data (levels will be displayed in columns)
#' @param x2 Character indicating a factor in the data (levels will be displayed in lines)
#' @param round Numeric to indicate how to round statistics
#' @param total Logical to indicate if a "Total" column should be added
#' @param scientific Logical to indicate if statistics should be displayed in scientific notations
#' @param digits Numeric (used if scientifc=T) to indicate how many digits to use in scientific notation
#' @param at.row a character. Used to space the results (see example below)
#' @param y.label Character. Indicates the label for y parameter
#' @param subjid Character. Indicates the column in which there is the subject Id to add the number of subjects in the column header if x1 and x2 are not null.
#' @param geomean Boolean. If yes geometric mean is calculated  instead of arithmetic mean: (exp(mean(log(x),na.rm=T))) fpr x>0

#' @description
#' \code{report.quanti} 
#' Returns quantitative descriptive statistics such as mean, median, standard deviation etc...
#' 
#' 
#' @details
#' This function computes and reports quantitative statistics on \code{y}. And can gives the statistics by level of two factors (\code{x1}
#' in columns and/or \code{x2} in rows). 
#' See the example to show the results. If \code{total=T}, the last column is the statistics
#' performed overall levels of \code{x1} for each levels of \code{x2}. 
#' Quantiles are calculated using type 3 (SAS) algorithms.
#' 
#' "geomean" compute the geometric mean defined as exp(mean(log(y))). The values below or equal 0 are removed and
#' a message is printed  to indicate how many values were deleted to calculate the geometric mean.
#' 
#' \code{N} returns the number of observations (including NA values)

#' @return  
#' A desc object.
#' 
#' @seealso \code{\link{report.quali}} \code{\link{report.doc}}

#' @examples
#'  
#' data(data)
#' 
#' # Quantitative statistics with no factor
#' 
#' report.quanti(data=data,y="y_numeric",y.label="Awesome results")
#' 
#' #' # Quantitative statistics with no factor with geometric mean (option geomean=TRUE)
#' 
#' report.quanti(data=data,y="y_numeric",y.label="Awesome results",geomean=TRUE)
#' 
#' # Quantitative statistics with one factor
#' 
#' report.quanti(data=data,y="y_numeric",x1="GROUP")
#' 
#' # One factor with total column
#' 
#' report.quanti(data=data,y="y_numeric",x1="GROUP",total=TRUE)
#' 
#' # Quantitative statistics with two factors
#' 
#' report.quanti(data=data,y="y_numeric",x1="GROUP",x2="TIMEPOINT")
#' 
#' # Quantitative statistics with two factors and a total column
#' 
#' report.quanti(data=data,y="y_numeric",x1="GROUP",x2="TIMEPOINT",total=TRUE)
#' 
#' # Quantitative statistics with spacing rows (option at.row)
#' 
#' report.quanti(data=data,y="y_numeric",x1="GROUP",
#' x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT")
#' 
#' # Add number of subjects in headers (option subjid)
#' 
#' report.quanti(data=data,y="y_numeric",x1="GROUP",
#' x2="TIMEPOINT",total=TRUE,at.row="TIMEPOINT",subjid="SUBJID")
#' 
#' 
#' @import reshape2
#' 
#' @importFrom dplyr %>% summarise_at group_by_ funs_
#' 
#' @export




report.quanti=function(data,y,x1=NULL,x2=NULL,y.label=y,
		round=2,
		total=F,scientific=F,digits=NULL,at.row=NULL,subjid=NULL,geomean=F)
{
	
	
#	y="DEMEANOUR_num"
#	x1="GROUP"
#	x2="TIMEPOINT"
#	y.label=y	
#	round=2
#	total=T
#	scientific=F
#	digits=NULL
#	at.row=NULL
	
	
	################################
	# Check 
	################################
	
	if(is.null(y)) stop("y argument cannot be NULL. Thank you for your comprehension. We love you anyway")
	if(class(data)!="data.frame") stop("data argument should be a data.frame. Thank you for your comprehension")
	if(class(y)!="character") stop("Dear user. y argument should be a character.  Thank you for your comprehension")
	if(!any(colnames(data)==y)) stop("y argument should be in data colnames. Thank you for your comprehension")
	if(total==T & is.null(x1) & is.null(x2)) message("Since x1 and x2 are NULL, it's not necessary to set total=T. In fact it doesn't make any sense... So it's not taken into account in the end. Thank you for your comprehension")
	
	
	if(!is.logical(total))		stop("Argument total argument must be logical")
	if(!is.numeric(digits) & !is.null(digits)) stop("Argument digits must be numeric")
	
	
	if(!is.numeric(data[,y])) stop(paste("y should be a numeric variable"))
	if(!is.numeric(round)) stop(paste("round should be numeric"))
	
	# if x1 and x2 are NULLwe use the function with temporary intercepts
	
	if(is.null(x1)) x1="int";data$int=1
	if(is.null(x2)) x2="int";data$int=1
	
	
	################################
	# start function
	################################
	
	# group data by factors
	
	
	by_GROUP=data %>% group_by_(x1)%>% group_by_(x2,add=T)	
	
	
	# define statistics
	
	N=as.formula(paste0("~","length(.)"))
	n=as.formula(paste0("~","length(",y,")"))
	mean=as.formula(paste0("~","mean(",y,",na.rm=T)"))
	sd=as.formula(paste0("~","sd(",y,",na.rm=T)"))
	median=as.formula(paste0("~","median(",y,",na.rm=T)"))
	mad=as.formula(paste0("~","mad(",y,",na.rm=T)"))
	q1=as.formula(paste0("~","quantile(",y,",na.rm=T,0.25,type =3)"))
	q3=as.formula(paste0("~","quantile(",y,",na.rm=T,0.75,type =3)"))
	min=as.formula(paste0("~","min(",y,",na.rm=T)"))
	max=as.formula(paste0("~","max(",y,",na.rm=T)"))
	missing=as.formula(paste0("~","length(",y,"[is.na(",y,")])"))
	
	geomean_func=function(x)
	{
		if(any(x<=0)) message(paste0(length(x[x<=0])," values were removed to calculate the Geometric mean"))
		x=x[x>0]
		exp(mean(log(x),na.rm=T))
	}
	
	#TODO: add mad option
	geo.mean=as.formula(paste0("~","geomean_func(",y,")"))
	
#	geomean1=as.formula(paste0("~","geomean_func1(",y,")"))
#	
	#	select statistics
	
#	stat_list=c("N"=N,"n"=n,"mean"=mean,
#			"sd"=sd,"median"=median,"mad"=mad,
#			"q1"=q1,"q3"=q3,"min"=min,"max"=max,
#			"missing"=missing,
#			"geomean"=geomean,
#			"geomean1"=geomean1)
	
	if(geomean)
	{
		stat_list=c("N"=N,"mean"=geo.mean,
				"sd"=sd,"median"=median,"mad"=mad,
				"q1"=q1,"q3"=q3,"min"=min,"max"=max,
				"missing"=missing)
		
	}else
	{
		
		stat_list=c("N"=N,"mean"=mean,
				"sd"=sd,"median"=median,"mad"=mad,
				"q1"=q1,"q3"=q3,"min"=min,"max"=max,
				"missing"=missing)
		
	}
	
	# compute statistics
	
	stat=data.frame(by_GROUP %>% summarise_at(.funs=funs_(dots=stat_list),.vars=y))
	
	# format outputs
	
	ind=which(sapply(stat,class)!="factor") 
	ind=ind[!"%in%"(ind,which(colnames(stat)=="N" | colnames(stat)=="missing"))] 
	stat[ind]=format(round(stat[ind],round),nsmall=round,scientific=scientific,digits=digits)
	
	# Regroup stat
	
	stat$mean_sd=paste0(stat$mean,"(",stat$sd,")")
	stat$q1_q3=paste0("[",stat$q1,";",stat$q3,"]")
	stat$min_max=paste0("[",stat$min,";",stat$max,"]")
	stat$median_mad=paste0(stat$median,"(",stat$mad,")")
	
	stat$mean=NULL
	stat$sd=NULL
	stat$median=NULL
	stat$mad=NULL
	stat$min=NULL
	stat$max=NULL
	stat$q1=NULL
	stat$q3=NULL
	
	# reshape 
	
	m=melt(data=stat,id.vars=c(x1,x2),variable.name="Statistics")
	stat2=dcast(m,as.formula(paste0(x2,"+","Statistics","~",x1)),value.var="value")
	if(any(colnames(stat2)=="1.00"))colnames(stat2)[colnames(stat2)=="1.00"]=y.label
	
	levels(stat2$Statistics)[levels(stat2$Statistics)=="N"]="N"
	if(geomean)
	{
		levels(stat2$Statistics)[levels(stat2$Statistics)=="mean_sd"]="Geo Mean (SD)"
	}else
	{
		levels(stat2$Statistics)[levels(stat2$Statistics)=="mean_sd"]="Mean (SD)"	
	}
	
	levels(stat2$Statistics)[levels(stat2$Statistics)=="median_mad"]="Median (MAD)"
	levels(stat2$Statistics)[levels(stat2$Statistics)=="min_max"]="[Min;Max]"
	levels(stat2$Statistics)[levels(stat2$Statistics)=="missing"]="Missing"
	levels(stat2$Statistics)[levels(stat2$Statistics)=="q1_q3"]="[Q1;Q3]"
	
	
# add Total, if requested
	
	if(total)
	{
		if(is.null(x2))
		{
			data$intercept=1
			temp=report.quanti(data=data,y=y,x1="intercept")$output
			stat2=cbind(stat2,Total=temp[,2])
		}
		
		if(!is.null(x2))
		{
			data$intercept=1
			temp=report.quanti(data=data,y=y,x1=x2,x2="intercept")$output
			temp$intercept=NULL
			temp=melt(temp,id.vars="Statistics")
			stat2=cbind(stat2,Total=temp[,3])
		}
		
	}
	
	
	if(geomean)
	{
		stat2$Statistics=factor(stat2$Statistics,levels=c("N","Geo Mean (SD)","Median (MAD)",
						"[Q1;Q3]","[Min;Max]","Missing"))
	}else
	{
		stat2$Statistics=factor(stat2$Statistics,levels=c("N","Mean (SD)","Median (MAD)",
						"[Q1;Q3]","[Min;Max]","Missing"))
	}
	
	stat2=stat2[order(stat2[,x2],stat2$Statistics),]
	
	

	
	if(!is.null(subjid))
	{
		if(!is.null(x1) & !is.null(x2))
		{
			
			if(!any("%in%"(colnames(data),subjid))) stop(paste0(subjid," variable is not in data colnames"))
			
			if(!total)
			{
				N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
				colnames(stat2)[-c(1,2)]=paste0(colnames(stat2)[-c(1,2)]," (N=",N,")")
				
			}
			
			
			if(total)
			{
				N=tapply(data[,subjid],data[,x1],function(x)length(unique(x)))
				N=c(N,sum(N))
				colnames(stat2)[-c(1,2)]=paste0(colnames(stat2)[-c(1,2)]," (N=",N,")")
				
			}
		}
	}
	
	if(any("%in%"(colnames(stat2),"int"))) stat2=stat2[,-which("%in%"(colnames(stat2),"int"))]
	
	
	
	if(!is.null(at.row))
	{
		stat2=spacetable(stat2,at.row=at.row)
	}
	
	
	# determination of the number of columns
	if(is.null(x2) )
	{
		nbcol=1
	}else
	{
		nbcol=2
	}
	
	if(x1=="int") x1=NULL
	if(x2=="int") x2=NULL
	
	stat2=ClinReport::desc(output=stat2,total=total,nbcol=nbcol,y=y,x1=x1,x2=x2,at.row=at.row,
			subjid=subjid,type.desc="quanti",type=NULL,y.label=y.label)
	

	
	return(stat2)
}











