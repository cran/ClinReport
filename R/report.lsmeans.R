# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#' Returns in a pretty format, the LS means of a model
#' (It's not that fantastic but it's handy)
#' 
#'
#' @param lsm an emmGrid object (result of a emmeans call)
#' @param x1.name Character indicating a factor in the data. Levels will be displayed in columns
#' @param x2.name Character indicating a factor in the data. Levels will be displayed in rows
#' @param x3.name Character indicating a factor in the data. Levels will be displayed in rows
#' @param variable.name Character. The label of the column which indicates the statistics reported.
#' @param infer pass to summary.emmGrid function. Can be c(T,T) c(T,F) c(F,T) c(F,F) or T or F
#' @param at.row Character. Passed to spacetable function. Used to space the results per levels of the mentioned variable
#' @param type Character. pass to summary.emmGrid function. can be "link" or "response"
#' 
#' @description
#' Create nice reporting table of lsmeans (or emmeans if you prefer, whatever...).
#' 
#' 
#' @details
#' This function has been created with love and passion.


#' @return  
#' A desc object that can be used for the report.doc function.
#' 
#' @seealso \code{\link{report.quali}} \code{\link{emmeans}} \code{\link{report.doc}} 

#' @examples
#' 
#' library(emmeans)
#' library(lme4)
#' 
#' data(data)
#' 
#' #Simple lm model
#' 
#' luke=lm(Petal.Width~Species,data=iris)
#' vador=emmeans(luke,~Species)
#' report.lsmeans(vador,"Species")
#' 
#' # In case of just one intercept, x1.name="X1"
#' lucky=glm(Species~1,data=iris,family=binomial)
#' strike=emmeans(lucky,~1)
#' report.lsmeans(strike,"X1")
#' 
#' #Mixed model example using lme4
#' 
#' james=lmer(y_numeric~GROUP+TIMEPOINT+GROUP*TIMEPOINT+(1|SUBJID),data=data) 
#' bond=emmeans(james,~GROUP|TIMEPOINT)
#' report.lsmeans(lsm=bond,x1.name="GROUP",x2.name="TIMEPOINT",at.row="TIMEPOINT")
#' 
#' 
#' @import reshape2 stats
#' 
#' @export

report.lsmeans=function(lsm,x1.name="treatment",x2.name=NULL,x3.name=NULL,
		variable.name="Statistics",at.row=NULL,infer=c(T,T),type="link")
{
	
	# Number of factor (explicative variables)
	
	if(class(lsm)!="emmGrid") stop("This function takes emmGrid object only as lsm argument")
	
	nblev=length(lsm@model.info$xlev)
	
	if(nblev==0)nblev=1
	
	if(length(c(x1.name,x2.name,x3.name))!=nblev) stop("The number of levels in lsm argument doesn't match the number of filled explicative variables x1.name, x2.name and x3.name")
	
	
	call=as.character(lsm@model.info$call)[1]
	
	
	nbcol=1:nblev
	
	if(any("%in%"(call,c("lm","lmer","lme.formula")))) type.mod="quanti"
	if(any("%in%"(call,c("glm","glmer")))) type.mod="quali"
	if(!any("%in%"(call,c("glm","glmer","lm","lmer","lme.formula")))) stop("This function only supports lm, lmer, lme, glm or glmer models")
	
	lsm=data.frame(summary(lsm,infer=infer,type=type))
	

	lsm[,-nbcol]=format(round(lsm[,-nbcol],2), nsmall = 2)
	lsm[,-nbcol]=apply(lsm[,-nbcol],2,function(x)gsub(" ","",x))
	
	if(!is.null(lsm$prob)) lsm$emmean=lsm$prob; lsm$prob=NULL
	if(!is.null(lsm$rate)) lsm$emmean=lsm$rate; lsm$rate=NULL
	
	lsm$"Estimate (SE)"=paste(lsm$emmean,"(",lsm$SE,")",sep="")
	
	if(!is.null(lsm$lower.CL))
	{
		lsm$"95% CI"=paste("[",lsm$lower.CL,";",lsm$upper.CL,"]",sep="")	
	}
	
	if(!is.null(lsm$asymp.LCL))
	{
		lsm$"95% CI"=paste("[",lsm$asymp.LCL,";",lsm$asymp.UCL,"]",sep="")	
	}
	
	

	
	lsm$"P-value"=prettyp(suppressWarnings(as.numeric(lsm$p.value)))

	
	ind=which("%in%"(colnames(lsm),c("Estimate (SE)","P-value","95% CI")))
	
	if(type.mod=="quali")
	{
		ind=which("%in%"(colnames(lsm),c("Estimate (SE)","Probability","P-value","95% CI")))
	}
	
	lsm=lsm[,c(nbcol,ind)]
	
	if(!is.null(x2.name) & !is.null(x3.name))	
	{
		m=melt(data=lsm, id.vars=c(x1.name,x2.name,x3.name),
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(x2.name,"+",x3.name,"+",variable.name,"~",x1.name,sep="")
		form=as.formula(form)			
		d=dcast(m,form)
	}
	
	if(!is.null(x2.name) & is.null(x3.name))	
	{
		m=melt(data=lsm, id.vars=c(x1.name,x2.name),
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(x2.name,"+",variable.name,"~",x1.name,sep="")
		form=as.formula(form)	
		
		d=dcast(m,form)
	}
	
	if(is.null(x2.name) & is.null(x3.name))	
	{
		m=melt(data=lsm, id.vars=c(x1.name),
				measure.vars=colnames(lsm)[(length(nbcol)+1):length(colnames(lsm))],
				variable.name=variable.name)
		
		form=paste(variable.name,"~",x1.name,sep="")
		form=as.formula(form)	
		
		d=dcast(m,form)
	}

	
	if(!is.null(at.row))
	{
		d=spacetable(d,at.row=at.row)
	}
	
	lsm=ClinReport::desc(output=d,total=F,nbcol=length(nbcol),type.desc="lsmeans",type=type,y.label="")
	

	
	lsm
	
}
