# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

#Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/Users/jfcollin/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64", sep=.Platform$path.sep))
#library(devtools)
#setwd("C:\\Users\\jfcollin\\Google Drive\\Dev\\ClinReport")
#check(args ="--as-cran")
#install()

##############################################
#Test 
##############################################

library(ClinReport)
library(officer)
library(flextable)
library(reshape2)
library(emmeans)
library(lme4)
library(nlme)
library(dplyr)

data(data)

tab1=report.quanti(data=data,y="y_numeric",
		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID")

tab2=report.quali(data=data,y="y_logistic",
		x1="GROUP",x2="TIMEPOINT",at.row="TIMEPOINT",subjid="SUBJID")


tab3=report.quali(data=data,y="VAR",y.label="Whatever")

tab4=report.quali(data=data,y="VAR",y.label="Whatever",
		subjid="SUBJID")


tab5=report.quali(data=data,y="VAR",y.label="Whatever",x1="GROUP",
		subjid="SUBJID")



mod2=lm(y_numeric~GROUP,data=data)
test2=emmeans(mod2,~GROUP)
tab.mod2=report.lsmeans(lsm=test2,x1.name="GROUP")


mod3=lme(y_numeric~GROUP+TIMEPOINT+GROUP*TIMEPOINT,random=~1|SUBJID,data=data,na.action=na.omit)
test3=emmeans(mod3,~GROUP|TIMEPOINT)
tab.mod3=report.lsmeans(lsm=test3,x1.name="GROUP",x2.name="TIMEPOINT",at.row="TIMEPOINT")


mod4=glm(y_logistic~GROUP+TIMEPOINT+GROUP*TIMEPOINT,family=binomial,data=data,na.action=na.omit)
test4=emmeans(mod4,~GROUP|TIMEPOINT)
tab.mod4=report.lsmeans(lsm=test4,x1.name="GROUP",x2.name="TIMEPOINT",at.row="TIMEPOINT")


mod5=glm(y_logistic~GROUP+TIMEPOINT+GROUP*TIMEPOINT,family=binomial,data=data,na.action=na.omit)
test5=emmeans(mod5,~GROUP|TIMEPOINT)
tab.mod5=report.lsmeans(lsm=test5,x1.name="GROUP",x2.name="TIMEPOINT",at.row="TIMEPOINT",type="response")


mod6=glm(y_poisson~GROUP+TIMEPOINT+GROUP*TIMEPOINT,family=poisson,data=data,na.action=na.omit)
test6=emmeans(mod6,~GROUP|TIMEPOINT)
tab.mod6=report.lsmeans(lsm=test6,x1.name="GROUP",x2.name="TIMEPOINT",at.row="TIMEPOINT",type="response")

mod7=lm(y_numeric~+GROUP+TIMEPOINT+VAR,data=data,na.action=na.omit)
test7=emmeans(mod7,~GROUP:VAR|TIMEPOINT)
tab.mod7=report.lsmeans(lsm=test7,x1.name="GROUP",x2.name="TIMEPOINT",x3.name="VAR",at.row="TIMEPOINT")

 
tab8=report.quanti(data=data,y="y_numeric",
		x1="GROUP",subjid="SUBJID",y.label="Y num")

tab9=report.quali(data=data,y="y_logistic",
		x1="GROUP",subjid="SUBJID",y.label="Y quali")

tab10=regroup(tab8,tab9)

tab11=report.quanti(data=data,y="y_numeric",
		x1="GROUP",subjid="SUBJID",y.label="Y num",total=T)

tab12=report.quali(data=data,y="y_logistic",
		x1="GROUP",subjid="SUBJID",y.label="Y quali",total=T)

tab13=regroup(tab11,tab12)

# Starts reporting

doc=read_docx()

doc=body_add_par(doc,"A beautiful reporting using ClinReport", style = "heading 1")

doc=report.doc(tab1,title="Demeanour: quantitative statistics",
		colspan.value="Treatment group",doc=doc,init.numbering=T)


doc=report.doc(tab2,title="Demeanour: qualitative statistics",
		colspan.value="Treatment group",doc=doc)


doc=report.doc(tab3,title="Temp: qualitative statistics",doc=doc)


doc=report.doc(tab.mod2,title="Linear Model LS-Means results using lm",
		colspan.value="Treatment group",doc=doc)

doc=report.doc(tab.mod3,title="Linear Mixed Model LS-Means results using lme",
		colspan.value="Treatment group",doc=doc)


doc=report.doc(tab.mod6,title="Poisson Model LS-Means results",
		colspan.value="Treatment group",doc=doc)


doc=report.doc(tab10,title="Regroup qualitative and quantitative statistics",
		colspan.value="Treatment group",doc=doc)


doc=report.doc(tab13,title="Regroup qualitative and quantitative statistics with a Total column",
		colspan.value="Treatment group",doc=doc)



file=paste(tempfile(),".docx",sep="")
print(doc, target =file)
shell.exec(file)









