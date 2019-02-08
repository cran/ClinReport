# TODO: Add comment
# 
# Author: jfcollin
###############################################################################


###############################################################################
# Author: jfcollin
###############################################################################

.output=new.env(parent = emptyenv())


#' Transform a desc object to a flexTable object ready to export to Word using officer
#'
#' @param table a desc object that report statistics (the results of \code{report.quanti} or \code{report.quali})
#' @param title Character. The title of the table
#' @param colspan.value Character. Add the label of the x1 variable levels (typically "Treatment Groups")
#' @param doc NULL or a rdocx object
#' @param numbering Boolean. If TRUE Output numbers are added before the title.
#' @param init.numbering Boolean. If TRUE Start numbering of the output at 1, otherwise it increase the output numbering of 1 unit
#' @param font.name Character. Passed to font function. Set the font of the output in Word
#' @param ... Other arguments
#' 
#' 
#' @description
#' \code{report.doc} 
#' Export the table to MS word in "clinical standard" format. 
#' 
#' @details
#' It creates a flextable object from a desc object and can eventually add it directly into a rdocx object.
#'  @return  
#' A flextable object or a rdocx object.
#' 
#' @seealso \code{\link{report.quali}} \code{\link{report.quanti}} \code{\link{report.lsmeans}}
#' @examples
#' \dontshow{
#' 
#' 
#'library(officer)
#'library(flextable)
#'library(reshape2)
#'library(emmeans)
#'library(lme4)
#'library(nlme)
#'
#'data(data)
#'
#'tab=report.quanti(data=data,y="y_numeric",
#'		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID")
#' 
#' mod=glm(y_logistic~GROUP+TIMEPOINT+GROUP*TIMEPOINT,
#' family=binomial,data=data,na.action=na.omit)
#'test=emmeans(mod,~GROUP|TIMEPOINT)
#'tab.mod=report.lsmeans(lsm=test,x1.name="GROUP",
#' x2.name="TIMEPOINT",at.row="TIMEPOINT")
#' 
#' 
#'doc=read_docx()
#'
#'doc=body_add_par(doc,"A beautiful reporting using ClinReport", style = "heading 1")
#'
#'doc=report.doc(tab,title="Quantitative statistics",
#'		colspan.value="Treatment group",doc=doc,init.numbering=TRUE)
#'
#' 
#'doc=report.doc(tab.mod,title="Generalized Linear Mixed Model LS-Means results using lme",
#'		colspan.value="Treatment group",doc=doc)
#'
#' }
#' 
#' 
#'\donttest{
#'library(officer)
#'library(flextable)
#'library(reshape2)
#'library(emmeans)
#'library(lme4)
#'library(nlme)
#'
#'data(data)
#'
#'tab1=report.quanti(data=data,y="y_numeric",
#'		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID")
#'
#'tab2=report.quali(data=data,y="y_logistic",
#'		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",total=T,subjid="SUBJID")
#'
#'
#'tab3=report.quali(data=data,y="VAR",y.label="Whatever")
#'
#'tab4=report.quali(data=data,y="VAR",y.label="Whatever",
#'		subjid="SUBJID")
#'
#'
#'tab5=report.quali(data=data,y="VAR",y.label="Whatever",x1="GROUP",
#'		subjid="SUBJID")
#'
#'
#'mod1=lm(y_numeric~GROUP+TIMEPOINT+GROUP*TIMEPOINT,data=data)
#'test1=emmeans(mod1,~GROUP|TIMEPOINT)
#'tab.mod1=report.lsmeans(lsm=test1,x1.name="GROUP",
#' x2.name="TIMEPOINT",at.row="TIMEPOINT")
#'
#'
#'mod2=lm(y_numeric~GROUP,data=data)
#'test2=emmeans(mod2,~GROUP)
#'tab.mod2=report.lsmeans(lsm=test2,x1.name="GROUP")
#'
#'
#'mod3=lme(y_numeric~GROUP+TIMEPOINT+GROUP*TIMEPOINT,
#' random=~1|SUBJID,data=data,na.action=na.omit)
#'test3=emmeans(mod3,~GROUP|TIMEPOINT)
#'tab.mod3=report.lsmeans(lsm=test3,x1.name="GROUP",
#' x2.name="TIMEPOINT",at.row="TIMEPOINT")
#'
#'
#'mod4=glm(y_logistic~GROUP+TIMEPOINT+GROUP*TIMEPOINT,
#' family=binomial,data=data,na.action=na.omit)
#'test4=emmeans(mod4,~GROUP|TIMEPOINT)
#'tab.mod4=report.lsmeans(lsm=test4,x1.name="GROUP",
#' x2.name="TIMEPOINT",at.row="TIMEPOINT")
#'
#'
#'mod5=glm(y_logistic~GROUP+TIMEPOINT+GROUP*TIMEPOINT,
#' family=binomial,data=data,na.action=na.omit)
#'test5=emmeans(mod5,~GROUP|TIMEPOINT)
#'tab.mod5=report.lsmeans(lsm=test5,x1.name="GROUP",
#' x2.name="TIMEPOINT",at.row="TIMEPOINT",type="response")
#'
#'
#'mod6=glm(y_poisson~GROUP+TIMEPOINT+GROUP*TIMEPOINT,
#' family=poisson,data=data,na.action=na.omit)
#'test6=emmeans(mod6,~GROUP|TIMEPOINT)
#' 
#'tab.mod6=report.lsmeans(lsm=test6,x1.name="GROUP",
#' x2.name="TIMEPOINT",at.row="TIMEPOINT",type="response")
#'
#'mod7=lm(y_numeric~+GROUP+TIMEPOINT+VAR,
#' data=data,na.action=na.omit)
#'test7=emmeans(mod7,~GROUP:VAR|TIMEPOINT)
#'tab.mod7=report.lsmeans(lsm=test7,x1.name="GROUP",
#' x2.name="TIMEPOINT",x3.name="VAR",at.row="TIMEPOINT")
#'
#'
#'
#'doc=read_docx()
#'
#'doc=body_add_par(doc,"A beautiful reporting using ClinReport", style = "heading 1")
#'
#'doc=report.doc(tab1,title="Quantitative statistics (2 explicative variables)",
#'		colspan.value="Treatment group",doc=doc,init.numbering=T)
#'
#'
#'doc=report.doc(tab2,title="Qualitative statistics (2 explicative variables)",
#'		colspan.value="Treatment group",doc=doc)
#'
#'
#'doc=report.doc(tab3,title="Qualitative statistics (1 variable only)",doc=doc)
#'
#'
#'doc=report.doc(tab.mod1,title="Linear Model LS-Means results using lm with interactions",
#'		colspan.value="Treatment group",doc=doc)
#'
#'doc=report.doc(tab.mod2,title="Linear Model LS-Means results using lm without interaction",
#'		colspan.value="Treatment group",doc=doc)
#'
#'doc=report.doc(tab.mod3,title="Linear Mixed Model LS-Means results using lme",
#'		colspan.value="Treatment group",doc=doc)
#'
#'doc=report.doc(tab.mod4,title="Generalized Linear Mixed Model LS-Means results using lme",
#'		colspan.value="Treatment group",doc=doc)
#'
#' 
#'doc=report.doc(tab.mod6,title="Poisson Model LS-Means results",
#'		colspan.value="Treatment group",doc=doc)
#'
#'
#'file=paste(tempfile(),".docx",sep="")
#'print(doc, target =file)
#'shell.exec(file)
#' 
#' }
#' 
#' @import officer flextable
#' 
#' @rdname report.doc
#' @export


report.doc <- function(table,...)
{
	UseMethod("report.doc")
}



#' @rdname report.doc
#' @export 

report.doc.desc=function(table,title,colspan.value="",doc=NULL,
		init.numbering=F,numbering=T,font.name="Times",...)
{
	
	
	
	total=table$total
	nb.col=table$nbcol
	output=table$output
	
	# n.stat= number of columns that reports statistics
	# not counting the Total column
	
	n.stat=ncol(output)-nb.col
	
	if(total) n.stat=n.stat-1
	
	#Initialize the numbering if this function is launched for the first time
	
	if(is.null(.output$number)) .output$number=1
	
	#Re-initialize if init.numbering =T
	
	if(init.numbering) .output$number=1
	
	
	# Add output numbering to the title
	
	if(numbering)
	{
		title= paste0("Output ",get("number",envir=.output),": ",c(title))
	}
	
	
	# Increase numbering by one
	
	.output$number=.output$number+1
	
	#####################
	
	
	# mapping col_keys and column labels
	
	old.col=colnames(output)
	new.col=gsub("(",".",old.col,fixed=T)
	new.col=gsub(")",".",new.col,fixed=T)
	new.col=gsub("=","",new.col,fixed=T)
	new.col=gsub(" ","",new.col,fixed=T)
	colnames(output)=new.col
	map=data.frame(col_keys=new.col,old.col,
			stringsAsFactors = FALSE)
	
	# naked flextable
	
	ft=regulartable(output,col_keys = colnames(output))
	ft <- border_remove(ft)
	ft=autofit(ft)
	
	# change header
	
	ft <- set_header_df(ft, mapping = map, key = "col_keys" )
	
	# Add colspan.value as header
	
	if(colspan.value!="")
	{
		
		l=c(rep("''",nb.col),
				rep(paste0("'",colspan.value,"'"),n.stat))
		
		if(total) l=c(l,"''")
		
		add=paste0(new.col,"=",l,collapse=",")
		text=paste0("add_header(ft,top=T,",add,")")
		ft=eval(parse(text=text))
		ft <- merge_h(ft, part = "header") 
		ft <- align(ft,align = "center", part = "header")
	}
	
	
	# Add title line
	
	title=rep(paste0("'",title,"'"),ncol(output))
	text=paste0("add_header(ft,top=T,", paste0(new.col,"=",title,collapse=","),")")
	ft=eval(parse(text=text))
	ft <- merge_h(ft, part = "header")
	ft <- align(ft,align = "center", part = "header")
	
	# headers in bold and bg in grey for title
	
	ft <- bold(ft, part = "header")
#	ft <- bold(ft, j=1:nb.col,part = "body")
	
	ft <- bg(ft,i=1,bg="#DCDCDC", part = "header")
	
	# Add lines
	
	ft=hline(ft, border = fp_border(width = 2), part = "header" )
	ft=hline_top(ft, border = fp_border(width = 2), part = "header" )
	ft=hline_bottom(ft, border = fp_border(width = 2), part = "body" )
	ft=vline(ft, j =1:nb.col,border = fp_border(width = 1),part = "body")
	
	# merge first column in case there are repetitions
	
	ft=merge_v(ft,j=1)
	
	# change font to Times
	
	ft=font(ft,fontname=font.name,part ="all")
	
	# change row height
	
	ft=height_all(ft, height=0.1, part = "body")
	ft=height_all(ft, height=0.3, part = "header")
	
	# add to doc
	
	if(!is.null(doc))
	{	
		if(class(doc)!="rdocx") stop("doc must be a rdocx object")
		
		doc <- body_add_par(doc,"", style = "Normal")
		doc <- body_add_flextable(doc, value = ft)		
		return(doc)
	}else
	{
		return(ft)
	}
	
}




